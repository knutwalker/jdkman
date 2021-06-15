use once_cell::sync::OnceCell;
use std::{
    env, fs, io,
    path::{Path, PathBuf},
};

pub fn candidates_dir() -> &'static Path {
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

pub(crate) fn config() -> &'static [(String, String)] {
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
