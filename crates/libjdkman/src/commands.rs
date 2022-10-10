use crate::{
    prelude::{candidates_api, candidates_dir, platform, Candidate},
    sdkman::{archives_dir, temp_dir},
};
use std::{ffi::OsStr, io};

pub(crate) mod results {
    pub use super::default_command::{ChangedDefault, DefaultResult};
    pub use super::install_command::InstallError;
    pub use super::install_command::InstallResult;
    pub use super::use_command::{UseResult, Using};
}

pub struct JdkCurrent;
pub struct JdkDefault;
pub struct JdkInstall;
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
        show_suggested_default: bool,
        verbose: bool,
    ) -> io::Result<results::DefaultResult> {
        if show_suggested_default {
            Ok(results::DefaultResult::Suggest(default_command::suggested(
                verbose,
                candidates_api(),
            )?))
        } else {
            default_command::run(candidates_dir(), verbose, query.as_ref().map(OsStr::new))
        }
    }

    pub fn set(candidate: Candidate) -> io::Result<results::ChangedDefault> {
        default_command::set_default(candidates_dir(), candidate)
    }
}

impl JdkList {
    pub fn run(verbose: bool, all: bool) -> io::Result<String> {
        let current = JdkCurrent::run();
        if all {
            list_command::run_online(
                current.as_ref().map(Candidate::name),
                candidates_dir(),
                candidates_api(),
                platform(),
                verbose,
            )
        } else {
            list_command::run(current.as_ref().map(Candidate::name), candidates_dir())
        }
    }
}

impl JdkInstall {
    pub fn run(
        verbose: bool,
        install_suggested_default: bool,
        validate_version: bool,
        version: &str,
    ) -> results::InstallResult {
        install_command::run(
            candidates_dir(),
            archives_dir(),
            temp_dir(),
            candidates_api(),
            platform(),
            verbose,
            install_suggested_default,
            validate_version,
            version,
        )
    }
}

impl JdkUse {
    pub fn run(query: Option<impl AsRef<OsStr>>, verbose: bool) -> io::Result<results::UseResult> {
        let current = JdkCurrent::run();
        use_command::run(
            candidates_dir(),
            current.as_ref().map(Candidate::name),
            verbose,
            query.as_ref().map(OsStr::new),
        )
    }

    pub fn set(candidate: Candidate) -> results::Using {
        use_command::set_use(candidates_dir(), candidate)
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
    use crate::{prelude::Candidate, select::Selection};
    use std::{
        env,
        ffi::{OsStr, OsString},
        io,
        path::{Path, PathBuf},
    };

    pub enum UseResult {
        KeepCurrent,
        Invalid(Option<String>),
        Use(Using),
    }

    pub struct Using {
        pub name: String,
        pub java_home: PathBuf,
        pub path: Option<OsString>,
    }

    pub(super) fn run(
        candidates_dir: &Path,
        current: Option<&str>,
        verbose: bool,
        query: Option<&OsStr>,
    ) -> io::Result<UseResult> {
        let selected = select_candidates(query, current, candidates_dir, verbose)?;

        let use_result = match selected {
            Selection::Cancelled => UseResult::KeepCurrent,
            Selection::NoMatch => {
                UseResult::Invalid(query.and_then(|q| q.to_str()).map(String::from))
            }
            Selection::Selected(selection) => {
                let using = set_use(candidates_dir, selection);
                UseResult::Use(using)
            }
        };

        Ok(use_result)
    }

    pub(super) fn set_use(candidates_dir: &Path, candidate: Candidate) -> Using {
        let name = String::from(candidate.name());
        let candidate_path = candidate.into_path();

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

        Using {
            name,
            java_home: candidate_path,
            path: new_path,
        }
    }
}

mod default_command {
    use super::shared::{request, select_candidates};
    use crate::{prelude::Candidate, select::Selection};
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
        CandidateNotFound { query: Option<String> },
        Selected(ChangedDefault),
        Suggest(String),
    }

    pub struct ChangedDefault {
        pub name: String,
        pub before: Option<String>,
    }

    pub(super) fn suggested(verbose: bool, candidates_api: &str) -> io::Result<String> {
        request(
            format!("{}/candidates/default/java", candidates_api),
            verbose,
        )
    }

    pub(super) fn run(
        candidates_dir: &Path,
        verbose: bool,
        query: Option<&OsStr>,
    ) -> io::Result<DefaultResult> {
        let selected = select_candidates(query, None, candidates_dir, verbose)?;

        let default_result = match selected {
            Selection::Cancelled => DefaultResult::KeepCurrent,
            Selection::NoMatch => {
                let query = query.and_then(|q| q.to_str()).map(String::from);
                DefaultResult::CandidateNotFound { query }
            }
            Selection::Selected(selection) => {
                let selected = set_default(candidates_dir, selection)?;
                DefaultResult::Selected(selected)
            }
        };

        Ok(default_result)
    }

    pub(super) fn set_default(
        candidates_dir: &Path,
        candidate: Candidate,
    ) -> io::Result<ChangedDefault> {
        let name = String::from(candidate.name());
        let candidate_path = candidate.into_path();
        let current_path = candidates_dir.join("current");

        // We need to unlink any existing link and bail if current seems to not be a link at all
        let previous = match fs::symlink_metadata(&current_path) {
            Ok(meta) => {
                if meta.is_file() || meta.is_dir() {
                    return Err(io::Error::new(
                        ErrorKind::AlreadyExists,
                        format!(
                            "The 'current' link [{}] cannot be changed since it is not a link.",
                            current_path.display()
                        ),
                    ));
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

        Ok(ChangedDefault {
            name,
            before: previous,
        })
    }
}

mod validate_command {
    use super::shared::request;
    use std::io;

    pub(super) fn run(
        candidates_api: &str,
        platform: &str,
        verbose: bool,
        version: &str,
    ) -> io::Result<bool> {
        let url = format!(
            "{api}/candidates/validate/java/{version}/{platform}",
            api = candidates_api,
            version = version,
            platform = platform,
        );
        let valid = request(url, verbose)?;
        match valid.trim() {
            "valid" => Ok(true),
            "invalid" => Ok(false),
            otherwise => Err(io::Error::new(io::ErrorKind::Other, format!("Unexpected validation response, expected either 'valid' or 'invalid', but got {}", otherwise)))
        }
    }
}

mod install_command {
    use super::{default_command, validate_command};
    use crate::{eprintln_color, prelude::Candidate};
    use bstr::ByteSlice;
    use console::Style;
    use libdurl::DurlRequestBuilder;
    use std::{
        fs,
        io::{self, ErrorKind},
        path::{Path, PathBuf},
        process::{Command, Stdio},
        time::Duration,
    };

    pub type InstallResult = Result<Candidate, InstallError>;

    pub enum InstallError {
        AlreadyInstalled(String),
        InvalidVersion(String),
        ArchiveCorrupt(String, PathBuf),
        DownloadError(libdurl::RequestError),
        Other(io::Error),
    }

    impl From<io::Error> for InstallError {
        fn from(val: io::Error) -> Self {
            Self::Other(val)
        }
    }
    impl From<libdurl::RequestError> for InstallError {
        fn from(val: libdurl::RequestError) -> Self {
            Self::DownloadError(val)
        }
    }

    pub(super) fn run(
        candidates_dir: &Path,
        archives_dir: &Path,
        temp_dir: &Path,
        candidates_api: &str,
        platform: &str,
        verbose: bool,
        install_suggested_default: bool,
        validate_version: bool,
        version: &str,
    ) -> InstallResult {
        // determine version
        let default_version;
        let version = if install_suggested_default {
            default_version = default_command::suggested(verbose, candidates_api)?;
            &default_version
        } else {
            version
        };

        let candidates_path = validate_candidate(
            candidates_dir,
            candidates_api,
            platform,
            verbose,
            validate_version,
            version,
        )?;

        let zip_archive_target = archives_dir.join(format!("java-{}.zip", version));

        // download unless file already exists
        if !matches!(fs::symlink_metadata(&zip_archive_target), Ok(meta) if meta.is_file()) {
            download(
                temp_dir,
                candidates_api,
                platform,
                verbose,
                version,
                &zip_archive_target,
            )?;
        } else {
            eprintln!(
                "Found a previously downloaded java {} archive. Not downloading it again...",
                version
            );
        }

        let zip_archive_target = validate_archive(version, zip_archive_target)?;

        install_archive(temp_dir, version, candidates_path, zip_archive_target)
    }

    pub(super) fn validate_candidate(
        candidates_dir: &Path,
        candidates_api: &str,
        platform: &str,
        verbose: bool,
        validate_version: bool,
        version: &str,
    ) -> Result<PathBuf, InstallError> {
        let candidates_path = candidates_dir.join(version);
        if matches!(fs::symlink_metadata(&candidates_path), Ok(existing) if !existing.is_file()) {
            return Err(InstallError::AlreadyInstalled(version.into()));
        }

        if validate_version && !validate_command::run(candidates_api, platform, verbose, version)? {
            return Err(InstallError::InvalidVersion(version.into()));
        };

        Ok(candidates_path)
    }

    pub(super) fn download(
        temp_dir: &Path,
        candidates_api: &str,
        platform: &str,
        verbose: bool,
        version: &str,
        zip_archive_target: &Path,
    ) -> Result<(), InstallError> {
        pre_install(temp_dir, candidates_api, platform, verbose, version)?;

        let url = format!(
            "{api}/broker/download/java/{version}/{platform}",
            api = candidates_api,
            version = version,
            platform = platform
        );

        let binary_input = temp_dir.join(format!("java-{}.bin", version));

        let response = DurlRequestBuilder::new()
            .progress_bar()
            .progress_min_download(100_u64 << 10) // 100 KiB
            .progress_interval(Duration::from_millis(50)) // 20 fps
            .verbose_fn_if(verbose, super::shared::print_verbose)
            .url(&url)
            .write_to_file(true, &binary_input)
            .build()?
            .perform()?;

        if verbose {
            eprintln_color!(@Style::new().for_stderr().dim(), "Downloaded binary to {} in {:?}", binary_input.display(), response.timings.total);
        }

        let zip_output = post_install(
            temp_dir,
            candidates_api,
            platform,
            verbose,
            version,
            binary_input,
        )?;

        dbg!(fs::rename(dbg!(zip_output), dbg!(&zip_archive_target)))?;

        Ok(())
    }

    pub(super) fn validate_archive(
        version: &str,
        zip_archive_target: PathBuf,
    ) -> Result<PathBuf, InstallError> {
        let output = Command::new("unzip")
            .arg("-t")
            .arg(&zip_archive_target)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .output()?;

        if !output.status.success()
            || !output
                .stdout
                .contains_str("No errors detected in compressed data")
        {
            try_fs(|| fs::remove_file(&zip_archive_target))?;

            return Err(InstallError::ArchiveCorrupt(
                version.into(),
                zip_archive_target,
            ));
        }

        Ok(zip_archive_target)
    }

    pub(super) fn install_archive(
        temp_dir: &Path,
        version: &str,
        candidates_path: PathBuf,
        zip_archive_target: PathBuf,
    ) -> InstallResult {
        let out = temp_dir.join("out");
        try_fs(|| fs::remove_dir_all(&out))?;

        if !Command::new("unzip")
            .arg("-oq")
            .arg(&zip_archive_target)
            .arg("-d")
            .arg(&out)
            .status()?
            .success()
        {
            return Err(InstallError::ArchiveCorrupt(
                version.into(),
                zip_archive_target,
            ));
        }

        dbg!(fs::rename(dbg!(out), dbg!(&candidates_path)))?;

        Ok(Candidate::new(candidates_path))
    }

    fn pre_install(
        _temp_dir: &Path,
        _candidates_api: &str,
        _platform: &str,
        _verbose: bool,
        _version: &str,
    ) -> io::Result<()> {
        // TODO: implement
        // # pre-installation hook: implements function __sdkman_pre_installation_hook
        // local pre_installation_hook="${SDKMAN_DIR}/tmp/hook_pre_${candidate}_${version}.sh"
        // __sdkman_echo_debug "Get pre-installation hook: ${SDKMAN_CANDIDATES_API}/hooks/pre/${candidate}/${version}/${platform_parameter}"
        // __sdkman_secure_curl "${SDKMAN_CANDIDATES_API}/hooks/pre/${candidate}/${version}/${platform_parameter}" >| "$pre_installation_hook"
        // __sdkman_echo_debug "Copy remote pre-installation hook: $pre_installation_hook"
        // source "$pre_installation_hook"
        // __sdkman_pre_installation_hook || return 1
        // __sdkman_echo_debug "Completed pre-installation hook..."

        Ok(())
    }

    fn post_install(
        temp_dir: &Path,
        _candidates_api: &str,
        _platform: &str,
        _verbose: bool,
        version: &str,
        binary_input: PathBuf,
    ) -> Result<PathBuf, InstallError> {
        // TODO: implement
        // # post-installation hook: implements function __sdkman_post_installation_hook
        // # responsible for taking `binary_input` and producing `zip_output`
        // local post_installation_hook="${SDKMAN_DIR}/tmp/hook_post_${candidate}_${version}.sh"
        // __sdkman_echo_debug "Get post-installation hook: ${SDKMAN_CANDIDATES_API}/hooks/post/${candidate}/${version}/${platform_parameter}"
        // __sdkman_secure_curl "${SDKMAN_CANDIDATES_API}/hooks/post/${candidate}/${version}/${platform_parameter}" >| "$post_installation_hook"
        // __sdkman_echo_debug "Copy remote post-installation hook: ${post_installation_hook}"
        // source "$post_installation_hook"
        // __sdkman_post_installation_hook || return 1
        // __sdkman_echo_debug "Processed binary as: $zip_output"
        // __sdkman_echo_debug "Completed post-installation hook..."

        eprintln!("Running tar test");

        let binary_validation = Command::new("tar")
            .arg("tzf")
            .arg(&binary_input)
            .stdout(Stdio::null())
            .status()?;
        if !binary_validation.success() {
            return Err(InstallError::ArchiveCorrupt(version.into(), binary_input));
        }

        let work_dir = temp_dir.join("out");
        fs::create_dir_all(&work_dir)?;

        eprintln!("Running tar extract");

        if !Command::new("tar")
            .arg("zxf")
            .arg(&binary_input)
            .arg("-C")
            .arg(&work_dir)
            .status()?
            .success()
        {
            return Err(InstallError::ArchiveCorrupt(version.into(), binary_input));
        }

        let zip_output = binary_input.with_extension("zip");

        eprintln!("Running zip");

        if !Command::new("zip")
            .current_dir(&work_dir)
            .arg("-qyr")
            .arg(&zip_output)
            .arg(".")
            .status()?
            .success()
        {
            return Err(InstallError::ArchiveCorrupt(version.into(), zip_output));
        }

        fs::remove_file(binary_input)?;
        fs::remove_dir_all(work_dir)?;

        Ok(zip_output)
    }

    fn try_fs(f: impl FnOnce() -> io::Result<()>) -> io::Result<()> {
        match f() {
            Err(e) if e.kind() == ErrorKind::NotFound => Ok(()),
            otherwise => otherwise,
        }
    }
}

mod list_command {
    use super::shared::list_candidates;
    use crate::commands::shared::request;
    use std::{fmt::Write, io, path::Path};

    pub(super) fn run(current: Option<&str>, candidates_dir: &Path) -> io::Result<String> {
        let candidates = list_candidates(candidates_dir)?;

        let mut output = String::with_capacity(1024);
        output.push_str(
            "--------------------------------------------------------------------------------\n",
        );
        if candidates.is_empty() && crate::use_color() {
            let _ = writeln!(
                &mut output,
                "   {}",
                console::style("None installed!").yellow()
            );
        }

        for candidate in candidates {
            let candidate = candidate.into_name();

            if matches!(current, Some(c) if c == candidate) {
                let _ = writeln!(&mut output, " > {}", candidate);
            } else {
                let _ = writeln!(&mut output, " * {}", candidate);
            }
        }

        output.push_str(
            "--------------------------------------------------------------------------------\n",
        );
        output.push_str(
            "* - installed                                                                   \n",
        );
        output.push_str(
            "> - currently in use                                                            \n",
        );
        output.push_str(
            "--------------------------------------------------------------------------------\n",
        );

        Ok(output)
    }

    pub(super) fn run_online(
        current: Option<&str>,
        candidates_dir: &Path,
        candidates_api: &str,
        platform: &str,
        verbose: bool,
    ) -> io::Result<String> {
        let installed = list_candidates(candidates_dir)?;

        let url = format!(
            "{api}/candidates/java/{platform}/versions/list?current={current}&installed=${installed}",
            api = candidates_api,
            platform = platform,
            current = current.unwrap_or_default(),
            installed = installed.join(",")
        );

        request(url, verbose)
    }
}

mod shared {
    use crate::{
        eprint_color, eprintln_color,
        prelude::Candidate,
        select::{SelectOptions, Selection},
    };
    use console::Style;
    use libdurl::{DurlRequestBuilder, VerboseMessage};
    use std::{
        ffi::OsStr,
        fs::{self, DirEntry},
        io,
        path::Path,
    };

    pub(super) fn request(url: String, verbose: bool) -> io::Result<String> {
        let response = DurlRequestBuilder::new()
            .verbose_fn_if(verbose, print_verbose)
            .url(&url)
            .return_as_string()
            .build()?
            .perform()
            .map_err(|e| e.into_io_error())?;

        if verbose {
            eprintln_color!(@Style::new().for_stderr().dim(), "{:#?}", response.timings);
        }

        Ok(response.output)
    }

    pub(super) fn print_verbose(msg: VerboseMessage, data: &[u8]) {
        let style = match msg {
            VerboseMessage::Text => Style::new().for_stderr().dim(),
            VerboseMessage::OutgoingHeader => Style::new().for_stderr().dim().magenta(),
            VerboseMessage::FirstIncomingHeader => Style::new().for_stderr().green(),
            VerboseMessage::IncomingHeader => Style::new().for_stderr().cyan(),
        };
        match std::str::from_utf8(data) {
            Ok(s) => eprint_color!(@style, "{}", s),
            Err(_) => eprint_color!(@style.red(), "({} bytes of data)", data.len()),
        }
    }

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
