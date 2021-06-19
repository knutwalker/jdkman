use lenient_semver_parser::VersionBuilder;
use std::{
    borrow::Borrow,
    fmt::Display,
    path::{Path, PathBuf},
};

#[derive(Debug, Eq)]
pub struct Candidate {
    version: Option<Version>,
    path: PathBuf,
}

impl Candidate {
    pub(crate) fn new(path: PathBuf) -> Self {
        let version = lenient_semver_parser::parse_partial::<Version>(Self::name_from_path(&*path))
            .ok()
            .map(|(v, _)| v);
        Self { version, path }
    }

    pub fn name(&self) -> &str {
        Self::name_from_path(&self.path)
    }

    pub(crate) fn into_path(self) -> PathBuf {
        self.path
    }

    pub(crate) fn into_name(self) -> String {
        Self::name_from_path(&self.path).into()
    }

    fn name_from_path(path: &Path) -> &str {
        path.file_name()
            .and_then(|n| n.to_str())
            .expect("invalid filename")
    }
}

impl Display for Candidate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(self.name())
    }
}

impl PartialEq for Candidate {
    fn eq(&self, other: &Self) -> bool {
        self.version == other.version
    }
}

impl Ord for Candidate {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.version.cmp(&other.version)
    }
}

impl PartialOrd for Candidate {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.version.partial_cmp(&other.version)
    }
}

impl Borrow<str> for Candidate {
    fn borrow(&self) -> &str {
        self.name()
    }
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
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
