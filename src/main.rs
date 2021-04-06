#[macro_use]
extern crate derivative;

use once_cell::sync::OnceCell;
use skim::{
    prelude::{bounded, SkimOptionsBuilder},
    Skim, SkimItem, SkimItemReceiver, SkimItemSender,
};
use std::{
    env, fs, io,
    path::{Path, PathBuf},
    sync::Arc,
};
use use_command::UseResult;

mod use_command {
    use lenient_semver::Version;
    use skim::SkimItem;
    use std::{
        borrow::Cow,
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
        query: Option<String>,
        candidates_dir: impl AsRef<Path>,
    ) -> io::Result<UseResult> {
        let candidates_dir = candidates_dir.as_ref();
        let mut candidates = fs::read_dir(candidates_dir)?
            .filter_map(read_entry)
            .collect::<Vec<_>>();

        // sort by version
        candidates.sort();

        let selected = crate::skim_select_one(candidates, query, true);
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

    #[derive(Debug, Clone, Derivative)]
    #[derivative(PartialEq, Eq, PartialOrd, Ord)]
    struct Candidate {
        version: Option<Version<'static>>,
        #[derivative(PartialEq = "ignore", PartialOrd = "ignore", Ord = "ignore")]
        path: PathBuf,
    }

    impl SkimItem for Candidate {
        fn text(&self) -> std::borrow::Cow<str> {
            Cow::Borrowed(self.name())
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
            let version = lenient_semver::parse_into::<Version>(Self::name_from_path(&path))
                .ok()
                .map(|version| {
                    let (version, _, _) = version.disassociate_metadata::<'static>();
                    version
                });
            Self { version, path }
        }
    }

    fn read_entry(entry: io::Result<DirEntry>) -> Option<Candidate> {
        let entry = entry.ok()?;

        // Check if that entry is not a directory.
        // This is either a regular file or a symlink
        // Files are generally skipped, symlinks could be useful
        // with regard to alias handling. For now, we skip those as well.
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

macro_rules! eprint_color {
    ($color:path, $($arg:tt)*) => ({
        if use_color() {
            let text = ::std::fmt::format(format_args!($($arg)*));
            eprintln!("{}", $color.paint(text));
        } else {
            eprintln!($($arg)*);
        }
    })
}

macro_rules! eprint_green {
    ($($arg:tt)*) => { eprint_color!(::ansi_term::Colour::Green, $($arg)*); }
}

fn skim_select_one<T: SkimItem + Clone>(
    items: Vec<T>,
    query: Option<String>,
    tac: bool,
) -> Option<T> {
    let (tx, rx): (SkimItemSender, SkimItemReceiver) = bounded(items.len());
    for item in items {
        tx.send(Arc::new(item)).unwrap();
    }

    // so that skim knows when to stop receiving
    drop(tx);

    let options = SkimOptionsBuilder::default()
        // .height(Some("50%"))
        .query(query.as_deref())
        .tac(tac)
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
    let mut args = pico_args::Arguments::from_env();
    let query: Option<String> = args.opt_free_from_str()?;
    let use_result = use_command::run(query, sdkman_candidates_dir())?;
    if let UseResult::Use {
        name,
        java_home,
        path,
    } = use_result
    {
        eprint_green!("Using java version {} in this shell.", name);
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
