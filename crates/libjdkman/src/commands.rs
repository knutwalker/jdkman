use crate::prelude::{candidates_dir, Candidate};
use std::ffi::OsStr;

pub struct JdkCurrent;
pub struct JdkUse;

impl JdkCurrent {
    pub fn run() -> Option<Candidate> {
        current_command::run(candidates_dir())
    }
}

impl JdkUse {
    pub fn run(
        query: Option<impl AsRef<OsStr>>,
        verbose: bool,
    ) -> std::io::Result<use_command::UseResult> {
        let current = JdkCurrent::run();

        Ok(use_command::run(
            query.as_ref().map(OsStr::new),
            current.as_ref().map(Candidate::name),
            candidates_dir(),
            verbose,
        )?)
    }
}

pub mod current_command {
    use crate::candidate::Candidate;
    use std::{env, fs, path::Path};

    pub fn run(candidates_dir: impl AsRef<Path>) -> Option<Candidate> {
        let candidates_dir = candidates_dir.as_ref();

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

pub mod use_command {
    use crate::{
        candidate::Candidate,
        select::{SelectOptions, Selection},
    };
    use std::{
        env,
        ffi::{OsStr, OsString},
        fs::{self, DirEntry},
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

    pub fn run(
        query: Option<&OsStr>,
        current: Option<&str>,
        candidates_dir: impl AsRef<Path>,
        verbose: bool,
    ) -> io::Result<UseResult> {
        let candidates_dir = candidates_dir.as_ref();
        let mut candidates = fs::read_dir(candidates_dir)?
            .filter_map(read_entry)
            .collect::<Vec<_>>();

        // sort by version
        candidates.sort();

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
