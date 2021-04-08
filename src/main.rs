#[macro_use]
extern crate derivative;

use clap::{App, AppSettings, Arg};
use lenient_semver::VersionBuilder;
use once_cell::sync::OnceCell;
use skim::{
    prelude::{bounded, SkimOptionsBuilder},
    Skim, SkimItem, SkimItemReceiver, SkimItemSender,
};
use std::{
    borrow::Cow,
    env, fs, io,
    path::{Path, PathBuf},
    sync::Arc,
};
use use_command::UseResult;

#[derive(Debug, Clone, Derivative)]
#[derivative(PartialEq, Eq, PartialOrd, Ord)]
struct Candidate {
    version: Option<Version>,
    #[derivative(PartialEq = "ignore", PartialOrd = "ignore", Ord = "ignore")]
    path: PathBuf,
}

impl SkimItem for Candidate {
    fn text(&self) -> std::borrow::Cow<str> {
        Cow::Borrowed(self.name())
    }

    fn display<'a>(&'a self, context: skim::DisplayContext<'a>) -> skim::AnsiString<'a> {
        skim::AnsiString::from(context)
    }
}

impl Candidate {
    fn name(&self) -> &str {
        Self::name_from_path(&self.path)
    }

    fn name_from_path(path: &Path) -> &str {
        path.file_name()
            .and_then(|n| n.to_str())
            .expect("invalid filename")
    }

    fn new(path: PathBuf) -> Self {
        let version =
            lenient_semver::parser::parse_partial::<Version>(Self::name_from_path(&*path))
                .ok()
                .map(|(v, _)| v);
        Self { version, path }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Version {
    major: u64,
    minor: u64,
    patch: u64,
}

impl VersionBuilder<'_> for Version {
    type Out = Self;

    fn new() -> Self {
        Self::default()
    }

    fn build(self) -> Self::Out {
        self
    }

    fn set_major(&mut self, major: u64) {
        self.major = major;
    }

    fn set_minor(&mut self, minor: u64) {
        self.minor = minor;
    }

    fn set_patch(&mut self, patch: u64) {
        self.patch = patch;
    }
}

mod current_command {
    use crate::Candidate;
    use std::{env, fs, path::Path};

    pub(crate) fn run(candidates_dir: impl AsRef<Path>) -> Option<Candidate> {
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

mod use_command {
    use crate::Candidate;
    use std::{
        env,
        ffi::OsString,
        fs::{self, DirEntry},
        io,
        path::{Path, PathBuf},
    };

    pub(crate) enum UseResult {
        KeepCurrent,
        Use {
            name: String,
            java_home: PathBuf,
            path: Option<OsString>,
        },
    }

    pub(crate) fn run(
        query: Option<&str>,
        current: Option<&str>,
        candidates_dir: impl AsRef<Path>,
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

        let selected = crate::skim_select_one(candidates, pre_select, query, true);
        let use_result = selected.map_or(UseResult::KeepCurrent, |selection| {
            let name = selection.name().to_string();
            let candidate_path = selection.path;

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
        });

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

fn sdkman_candidates_dir() -> &'static Path {
    fn find() -> io::Result<PathBuf> {
        let candidates_dir = env::var_os("SDKMAN_CANDIDATES_DIR").ok_or_else(|| {
            io::Error::new(io::ErrorKind::NotFound, "No sdkman installation found")
        })?;
        let mut candidates_dir = PathBuf::from(candidates_dir);
        candidates_dir.push("java");
        Ok(candidates_dir)
    }

    static SDKMAN_CANDIDATES_DIR: OnceCell<PathBuf> = OnceCell::new();
    SDKMAN_CANDIDATES_DIR.get_or_init(|| match find() {
        Ok(path) => path,
        Err(err) => panic!("{}", err),
    })
}

fn sdkman_config() -> &'static [(String, String)] {
    fn load() -> Vec<(String, String)> {
        let sdkman_config = env::var_os("SDKMAN_DIR")
            .and_then(|sdkman_dir| {
                let mut sdkman_dir = PathBuf::from(sdkman_dir);
                sdkman_dir.push("etc");
                sdkman_dir.push("config");
                fs::read_to_string(sdkman_dir).ok()
            })
            .unwrap_or_default();

        sdkman_config
            .lines()
            .filter_map(|s| {
                let s = s.trim();
                if s.is_empty() || s.starts_with('#') {
                    None
                } else {
                    let mut parts = s.splitn(2, '=');
                    let key = parts.next()?.trim();
                    let value = parts.next()?.trim();
                    Some((String::from(key), String::from(value)))
                }
            })
            .collect()
    }

    static SDKMAN_CONFIG: OnceCell<Vec<(String, String)>> = OnceCell::new();
    SDKMAN_CONFIG.get_or_init(load).as_slice()
}

fn use_color() -> bool {
    fn check_for_colors() -> bool {
        if clicolors_control::colors_enabled() {
            // check if colors are explicitly disabled
            sdkman_config()
                .iter()
                .all(|(k, v)| k != "sdkman_colour_enable" || v != "false")
        } else {
            // check if colors are explicitly enabled
            sdkman_config()
                .iter()
                .any(|(k, v)| k == "sdkman_colour_enable" && v == "true")
        }
    }

    static SDKMAN_COLOR_ENABLED: OnceCell<bool> = OnceCell::new();
    *SDKMAN_COLOR_ENABLED.get_or_init(check_for_colors)
}

macro_rules! eprintln_color {
    ($color:path, $($arg:tt)*) => ({
        if use_color() {
            let text = ::std::fmt::format(format_args!($($arg)*));
            eprintln!("{}", $color.paint(text));
        } else {
            eprintln!($($arg)*);
        }
    })
}

macro_rules! eprintln_green {
    ($($arg:tt)*) => { eprintln_color!(::ansi_term::Colour::Green, $($arg)*); }
}

macro_rules! eprintln_red {
    ($($arg:tt)*) => { eprintln_color!(::ansi_term::Colour::Red, $($arg)*); }
}

fn skim_select_one<T: SkimItem + Clone>(
    items: Vec<T>,
    pre_select: Option<u32>,
    query: Option<&str>,
    tac: bool,
) -> Option<T> {
    let (tx, rx): (SkimItemSender, SkimItemReceiver) = bounded(items.len());
    for item in items {
        tx.send(Arc::new(item)).unwrap();
    }

    // so that skim knows when to stop receiving
    drop(tx);

    // Note that `java -version` prints to stderr and
    // skim only shows stderr when the command fails.
    // So we use `--version` instead, which is also more idiomatic.
    let preview_command = format!(
        "{0}/{{}}/bin/java --version; {0}/{{}}/bin/java -Xinternalversion",
        sdkman_candidates_dir().display()
    );

    let options = SkimOptionsBuilder::default()
        .pre_select(pre_select)
        .query(query.as_deref())
        .preview(Some(&preview_command))
        .preview_window(Some("up:wrap:hidden"))
        .bind(vec!["?:toggle-preview", "esc:cancel"])
        .nosort(true)
        .sync(true)
        .tac(tac)
        .exact(true)
        .prompt(Some(" "))
        .select1(true)
        .exit0(true)
        .build()
        .unwrap();

    let selected_item = Skim::run_with(&options, Some(rx)).and_then(|items| {
        if items.is_abort {
            None
        } else {
            items.selected_items.into_iter().next()
        }
    });

    if let Some(item) = selected_item {
        if let Some(item) = item.as_any().downcast_ref::<T>() {
            return Some(item.clone());
        }
    }

    None
}

fn run() -> Result<(), Box<dyn std::error::Error + 'static>> {
    let matches = App::new(clap::crate_name!())
        .version(clap::crate_version!())
        .about(clap::crate_description!())
        .setting(AppSettings::UnifiedHelpMessage)
        .setting(AppSettings::ColorAuto)
        .arg(
            Arg::with_name("CURRENT")
                .long("current")
                .help("Only show the current version and quit"),
        )
        .arg(
            Arg::with_name("QUERY")
                .help("Optional query to start a selection")
                .required(false)
                .multiple(false),
        )
        .get_matches();

    let current = current_command::run(sdkman_candidates_dir());
    if matches.is_present("CURRENT") {
        match current {
            Some(current) => eprintln!("Using java version {}", current.name()),
            None => eprintln_red!("Not using any version of java"),
        }
        return Ok(());
    }

    let query = matches.value_of("QUERY");
    let use_result = use_command::run(
        query.as_deref(),
        current.as_ref().map(Candidate::name),
        sdkman_candidates_dir(),
    )?;
    if let UseResult::Use {
        name,
        java_home,
        path,
    } = use_result
    {
        eprintln_green!("Using java version {} in this shell.", name);
        println!("export JAVA_HOME=\"{}\"", java_home.display());
        if let Some(path) = path {
            let path = path.to_str().expect(r"Invalid PATH conversion to UTF-8. This is probably a Windows machine, sooooooo ¯\_(ツ)_/¯");
            println!("export PATH=\"{}\"", path);
        }
    }

    Ok(())
}

pub fn main() {
    if let Err(err) = run() {
        eprintln!("{}", err);
        std::process::exit(1)
    }
}
