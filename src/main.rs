use once_cell::sync::OnceCell;
use skim::{
    prelude::{bounded, SkimOptionsBuilder},
    Skim, SkimItem, SkimItemReceiver, SkimItemSender,
};
use std::{
    env, io,
    path::{Path, PathBuf},
    sync::Arc,
};
use use_command::UseResult;

mod use_command {
    use lenient_semver::Version;
    use skim::SkimItem;
    use std::{
        borrow::Cow,
        cmp::Ordering,
        env,
        ffi::OsString,
        fs::{self, DirEntry, Metadata},
        io,
        path::{Path, PathBuf},
    };

    pub(crate) enum UseResult {
        KeepCurrent,
        Use {
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
            .filter(|c| matches!(c.candidate_type, CandidateType::Candidate))
            .collect::<Vec<_>>();

        // sort by version
        candidates.sort();

        let selected = crate::skim_select_one(candidates, query, true);
        let use_result = selected.map_or(UseResult::KeepCurrent, |selection| {
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
                java_home: candidate_path,
                path: new_path,
            }
        });

        Ok(use_result)
    }

    #[derive(Debug, Clone)]
    struct Candidate {
        version: Option<Version<'static>>,
        path: PathBuf,
        metadata: Metadata,
        candidate_type: CandidateType,
    }

    impl SkimItem for Candidate {
        fn text(&self) -> std::borrow::Cow<str> {
            Cow::Borrowed(self.name())
        }
    }

    impl PartialEq for Candidate {
        fn eq(&self, other: &Self) -> bool {
            if self.candidate_type != other.candidate_type {
                return false;
            }
            match self.candidate_type {
                CandidateType::Current => true,
                CandidateType::Alias => self.name() == other.name(),
                CandidateType::Candidate => self.version == other.version,
            }
        }
    }

    impl Eq for Candidate {}

    impl PartialOrd for Candidate {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    impl Ord for Candidate {
        fn cmp(&self, other: &Self) -> Ordering {
            match (self.candidate_type, other.candidate_type) {
                (CandidateType::Current, CandidateType::Current) => Ordering::Equal,
                (CandidateType::Current, _) => Ordering::Less,
                (CandidateType::Alias, CandidateType::Current) => Ordering::Greater,
                (CandidateType::Alias, _) => self.version.cmp(&other.version),
                (CandidateType::Candidate, CandidateType::Current) => Ordering::Greater,
                (CandidateType::Candidate, _) => self.version.cmp(&other.version),
            }
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

        fn current(path: PathBuf, metadata: Metadata) -> Self {
            Self::new(path, metadata, CandidateType::Current)
        }

        fn alias(path: PathBuf, metadata: Metadata) -> Self {
            Self::new(path, metadata, CandidateType::Alias)
        }

        fn candidate(path: PathBuf, metadata: Metadata) -> Self {
            Self::new(path, metadata, CandidateType::Candidate)
        }

        fn new(path: PathBuf, metadata: Metadata, candidate_type: CandidateType) -> Self {
            let version = lenient_semver::parse_into::<Version>(Self::name_from_path(&path))
                .ok()
                .map(|version| {
                    let (version, _, _) = version.disassociate_metadata::<'static>();
                    version
                });
            Self {
                version,
                path,
                metadata,
                candidate_type,
            }
        }
    }

    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    enum CandidateType {
        Current,
        Alias,
        Candidate,
    }

    fn read_entry(entry: io::Result<DirEntry>) -> Option<Candidate> {
        let entry = entry.ok()?;
        let metadata = entry.metadata().ok()?;

        if !metadata.is_dir() {
            if metadata.is_file() {
                // a regular file: skip
                return None;
            }

            // neither dir nor file: it's a symlink
            if entry.file_name() == "current" {
                // check if it is the 'current' pointer
                Some(Candidate::current(entry.path(), metadata))
            } else {
                // interpret every other symlink as an alias
                Some(Candidate::alias(entry.path(), metadata))
            }
        } else {
            Some(Candidate::candidate(entry.path(), metadata))
        }
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

fn skim_select_one<T: SkimItem + Clone>(
    items: Vec<T>,
    query: Option<String>,
    tac: bool,
) -> Option<T> {
    let (tx, rx): (SkimItemSender, SkimItemReceiver) = bounded(items.len());
    for item in items {
        // println!("item typeid = {:?}", item.as_any().type_id());
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

    match use_command::run(query, sdkman_candidates_dir())? {
        UseResult::KeepCurrent => {
            eprint!("Keeping current version");
        }
        UseResult::Use { java_home, path } => {
            println!("export JAVA_HOME=\"{}\"", java_home.display());
            if let Some(path) = path {
                let path = path.to_str().expect(r"Invalid PATH conversion to UTF-8. This is probably a Windows machine, sooooooo ¯\_(ツ)_/¯");
                println!("export PATH=\"{}\"", path);
            }
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
