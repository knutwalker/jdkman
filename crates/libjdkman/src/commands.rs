use crate::prelude::{candidates_dir, Candidate};
use std::{ffi::OsStr, io};

pub(crate) mod results {
    pub use super::default_command::DefaultResult;
    pub use super::list_command::ListResult;
    pub use super::use_command::UseResult;
}

pub struct JdkCurrent;
pub struct JdkDefault;
pub struct JdkList;
pub struct JdkUse;

impl JdkCurrent {
    pub fn run() -> Option<Candidate> {
        current_command::run(candidates_dir())
    }
}

impl JdkDefault {
    pub fn run(
        query: Option<impl AsRef<OsStr>>,
        verbose: bool,
    ) -> io::Result<results::DefaultResult> {
        default_command::run(query.as_ref().map(OsStr::new), candidates_dir(), verbose)
    }
}

impl JdkList {
    pub fn run(verbose: bool) -> io::Result<Vec<results::ListResult>> {
        let current = JdkCurrent::run();
        list_command::run(
            current.as_ref().map(Candidate::name),
            candidates_dir(),
            verbose,
        )
    }
}

impl JdkUse {
    pub fn run(query: Option<impl AsRef<OsStr>>, verbose: bool) -> io::Result<results::UseResult> {
        let current = JdkCurrent::run();
        use_command::run(
            query.as_ref().map(OsStr::new),
            current.as_ref().map(Candidate::name),
            candidates_dir(),
            verbose,
        )
    }
}

mod current_command {
    use crate::candidate::Candidate;
    use std::{env, fs, path::Path};

    pub(super) fn run(candidates_dir: &Path) -> Option<Candidate> {
        // try to find the currently enabled candidate in the PATH
        // If path is empty None is returned
        // If the candidate is not found, None is returned
        // If the candidate appears multiple times, the first match is returned
        //   as this is consistent with how the PATH would be searched
        let path = env::var_os("PATH").unwrap_or_default();
        env::split_paths(&path)
            .find_map(|path| {
                path.starts_with(candidates_dir)
                    .then(|| {
                        // we have a component that starts with the candidate but
                        // we may have additional components in the path (such as /bin)
                        // so we navigate the ancestors until the parent matches the candidates_dir
                        // then we take the current path as the one that has the correct version

                        path.ancestors()
                            // ancestors always starts with the current path,
                            //  but we're only interested in the actual parents
                            .skip(1)
                            // once a parent no longer starts with the candidate dir, we can stop searching
                            .take_while(|path| path.starts_with(candidates_dir))
                            .try_fold(&*path, |parent_parent, parent| {
                                if parent == candidates_dir {
                                    // try_fold short circuits when an Err is returned
                                    Err(parent_parent)
                                } else {
                                    // if not found, we traverse up the ancestors chain
                                    Ok(parent)
                                }
                            })
                            // we're only interested in the found case, where we returned an error
                            .err()
                            .map(ToOwned::to_owned)
                    })
                    .flatten()
            })
            // Resolve any links, most likely
            // resolve 'current' to where it actually points at.
            // This already returns the correct absolute path that we want
            // SDKman sets the links to relativ paths, so just using `std::fs::read_link`
            // would only yield the version as the full path, not the full absolute path.
            .map(fs::canonicalize)
            .and_then(Result::ok)
            .map(Candidate::new)
    }
}

mod use_command {
    use super::shared::select_candidates;
    use crate::select::Selection;
    use std::{
        env,
        ffi::{OsStr, OsString},
        io,
        path::{Path, PathBuf},
    };

    pub enum UseResult {
        KeepCurrent,
        Invalid(Option<String>),
        Use {
            name: String,
            java_home: PathBuf,
            path: Option<OsString>,
        },
    }

    pub(super) fn run(
        query: Option<&OsStr>,
        current: Option<&str>,
        candidates_dir: &Path,
        verbose: bool,
    ) -> io::Result<UseResult> {
        let selected = select_candidates(query, current, candidates_dir, verbose)?;

        let use_result = match selected {
            Selection::Cancelled => UseResult::KeepCurrent,
            Selection::NoMatch => {
                UseResult::Invalid(query.and_then(|q| q.to_str()).map(String::from))
            }
            Selection::Selected(selection) => {
                let name = selection.to_string();
                let candidate_path = selection.into_path();

                let old_path = env::var_os("PATH");
                let new_path = old_path.map(|path| {
                    // remove references to the previous candidate from the path
                    let mut candidate_path = Some(candidate_path.clone());
                    let new_path = env::split_paths(&path).filter_map(|path| {
                        if path.starts_with(candidates_dir) {
                            candidate_path.take().map(|mut new_candidate_path| {
                                let previous_path = path
                                    .strip_prefix(candidates_dir)
                                    .expect("starts_with returned true");
                                // skip old candidate name and add all entries that came after it
                                // this actual path usually contains an addition /bin
                                for path_segment in previous_path.iter().skip(1) {
                                    new_candidate_path.push(path_segment);
                                }
                                new_candidate_path
                            })
                        } else {
                            Some(path)
                        }
                    });
                    env::join_paths(new_path).expect("input is from existing PATH")
                });

                UseResult::Use {
                    name,
                    java_home: candidate_path,
                    path: new_path,
                }
            }
        };

        Ok(use_result)
    }
}

mod default_command {
    use super::shared::select_candidates;
    use crate::select::Selection;
    use std::{
        ffi::OsStr,
        fs,
        io::{self, ErrorKind},
        path::Path,
    };

    #[cfg(unix)]
    use std::os::unix::fs::symlink;
    #[cfg(windows)]
    use std::os::windows::fs::symlink_dir as symlink;

    pub enum DefaultResult {
        KeepCurrent,
        CandidateNotFound {
            query: Option<String>,
        },
        Selected {
            name: String,
            before: Option<String>,
        },
    }

    pub(super) fn run(
        query: Option<&OsStr>,
        candidates_dir: &Path,
        verbose: bool,
    ) -> io::Result<DefaultResult> {
        let selected = select_candidates(query, None, candidates_dir.as_ref(), verbose)?;

        let default_result = match selected {
            Selection::Cancelled => DefaultResult::KeepCurrent,
            Selection::NoMatch => {
                let query = query.and_then(|q| q.to_str()).map(String::from);
                DefaultResult::CandidateNotFound { query }
            }
            Selection::Selected(selection) => {
                let name = selection.to_string();
                let candidate_path = selection.into_path();
                let current_path = candidates_dir.join("current");

                // We need to unlink any existing link and bail if current seems to not be a link at all
                let previous = match fs::symlink_metadata(&current_path) {
                    Ok(meta) => {
                        if meta.is_file() || meta.is_dir() {
                            return Err(io::Error::new(ErrorKind::AlreadyExists, format!("The 'current' link [{}] cannot be changed since it is not a link.", current_path.display())));
                        }
                        let previous = fs::read_link(&current_path)?;
                        let previous = previous.strip_prefix(candidates_dir).unwrap_or(&previous);
                        let previous = previous.to_str().map(String::from);

                        fs::remove_file(&current_path)?;

                        previous
                    }
                    Err(err) if err.kind() == ErrorKind::NotFound => None,
                    Err(e) => return Err(e),
                };

                // emulate sdkman by linking the relative path
                let candidate_path = candidate_path
                    .strip_prefix(candidates_dir)
                    .unwrap_or(&candidate_path);

                symlink(candidate_path, &current_path)?;
                DefaultResult::Selected {
                    name,
                    before: previous,
                }
            }
        };

        Ok(default_result)
    }
}

mod list_command {
    use super::shared::list_candidates;
    use std::{
        io::{self},
        path::Path,
    };
    pub enum ListResult {
        Installed(String),
        Current(String),
    }

    pub(super) fn run(
        current: Option<&str>,
        candidates_dir: &Path,
        _verbose: bool,
    ) -> io::Result<Vec<ListResult>> {
        let candidates = list_candidates(candidates_dir)?;

        let candidates = candidates
            .into_iter()
            .map(|candidate| {
                let candidate = candidate.into_name();
                match current {
                    Some(current) if current == candidate => ListResult::Current(candidate),
                    _ => ListResult::Installed(candidate),
                }
            })
            .collect();

        Ok(candidates)
    }
}

mod shared {
    use crate::{
        prelude::Candidate,
        select::{SelectOptions, Selection},
    };
    use std::{
        ffi::OsStr,
        fs::{self, DirEntry},
        io,
        path::Path,
    };

    pub(super) fn list_candidates(candidates_dir: &Path) -> io::Result<Vec<Candidate>> {
        let mut candidates = fs::read_dir(candidates_dir)?
            .filter_map(read_entry)
            .collect::<Vec<_>>();

        // sort by version
        candidates.sort();

        Ok(candidates)
    }

    pub(super) fn select_candidates(
        query: Option<&OsStr>,
        current: Option<&str>,
        candidates_dir: &Path,
        verbose: bool,
    ) -> io::Result<Selection<Candidate>> {
        let candidates = list_candidates(candidates_dir)?;

        // pre select whatever current points to
        let pre_select = current
            .and_then(|current| candidates.iter().position(|c| c.name() == current))
            .map(|p| p as u32);

        // Note that `java -version` prints to stderr and
        // backends might only show stderr when the command fails.
        // So we use `--version` instead, which is also more idiomatic.
        let preview_command = format!(
            "{0}/{{2}}/bin/java --version; {0}/{{2}}/bin/java -Xinternalversion",
            candidates_dir.display()
        );

        let selected = SelectOptions::new(candidates)
            .pre_select(pre_select)
            .query(query)
            .preview_command(preview_command.as_str())
            .verbose(verbose)
            .select()?;

        Ok(selected)
    }

    fn read_entry(entry: io::Result<DirEntry>) -> Option<Candidate> {
        let entry = entry.ok()?;

        // We are only interested in directories,
        // the alternatives being files or symlinks.
        // We only want to list actual files,
        // not duplicate current or other symlinks
        entry
            .metadata()
            .ok()?
            .is_dir()
            .then(|| Candidate::new(entry.path()))
    }
}
