use once_cell::sync::OnceCell;
use std::{
    env,
    ffi::{CStr, OsString},
    fs, io,
    mem::MaybeUninit,
    path::{Path, PathBuf},
};

pub fn sdkman_dir() -> &'static Path {
    fn find() -> io::Result<PathBuf> {
        env::var_os("SDKMAN_DIR")
            .map(PathBuf::from)
            .or_else(|| {
                env::var_os("HOME").map(|home| {
                    let mut sdk_dir = PathBuf::from(home);
                    sdk_dir.push(".sdkman");
                    sdk_dir
                })
            })
            .ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::NotFound,
                    "No sdkman installation found: User has no home directory and no $SDKMAN_DIR.",
                )
            })
    }

    static SDKMAN_DIR: OnceCell<PathBuf> = OnceCell::new();
    SDKMAN_DIR.get_or_init(|| match find() {
        Ok(path) => path,
        Err(err) => panic!("{}", err),
    })
}

pub fn candidates_dir() -> &'static Path {
    fn find() -> PathBuf {
        let mut candidates_dir = env::var_os("SDKMAN_CANDIDATES_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|| sdkman_dir().join("candidates"));
        candidates_dir.push("java");
        candidates_dir
    }

    static SDKMAN_CANDIDATES_DIR: OnceCell<PathBuf> = OnceCell::new();
    SDKMAN_CANDIDATES_DIR.get_or_init(|| find())
}

pub fn candidates_api() -> &'static str {
    fn find() -> io::Result<String> {
        let api = env::var_os("JDKMAN_CANDIDATES_API")
            .or_else(|| env::var_os("SDKMAN_CANDIDATES_API"))
            .map(to_string("SDKMAN_CANDIDATES_API"))
            .transpose()?
            .unwrap_or_else(|| String::from("https://api.sdkman.io/2"));
        Ok(api)
    }

    static SDKMAN_CANDIDATES_API: OnceCell<String> = OnceCell::new();
    SDKMAN_CANDIDATES_API.get_or_init(|| match find() {
        Ok(path) => path,
        Err(err) => panic!("{}", err),
    })
}

pub fn platform() -> &'static str {
    fn infer() -> Option<&'static str> {
        cfg_if::cfg_if! {
            if #[cfg(target_os = "linux")] {
                cfg_if::cfg_if! {
                    if #[cfg(target_arch = "i686")] {
                        Some("LinuxX32")
                    } else if #[cfg(target_arch = "x86_64")] {
                        Some("LinuxX64")
                    } else if #[cfg(target_arch = "armv7")] {
                        Some("LinuxARM32")
                    } else if #[cfg(target_arch = "aarch64")] {
                        Some("LinuxARM64")
                    } else {
                        Some("LinuxX64")
                    }
                }
            } else if #[cfg(target_os = "macos")] {
                cfg_if::cfg_if! {
                    if #[cfg(target_arch = "x86_64")] {
                        Some("DarwinX64")
                    } else if #[cfg(target_arch = "aarch64")] {
                        if config().iter().any(|(k, v)| k == "sdkman_rosetta2_compatible" && v == "true") {
                            Some("DarwinX64")
                        } else {
                            Some("DarwinARM64")
                        }
                    } else {
                        Some("DarwinX64")
                    }
                }
            } else {
                None
            }
        }
    }

    fn unmae_s() -> io::Result<String> {
        let mut uname = MaybeUninit::zeroed();

        let r = unsafe { libc::uname(uname.as_mut_ptr()) };
        if r != 0 {
            return Err(io::Error::last_os_error());
        }

        let uname = unsafe { uname.assume_init() };
        let sysname = unsafe { CStr::from_ptr(uname.sysname.as_ptr()) };
        let sysname = sysname.to_string_lossy().to_ascii_lowercase();
        // let sysname = Box::leak(sysname.into_boxed_str());
        Ok(sysname)
    }

    fn infer_platform() -> String {
        infer()
            .map(|s| s.to_ascii_lowercase())
            .or_else(|| unmae_s().ok())
            .unwrap_or_else(|| std::env::consts::OS.to_ascii_lowercase())
    }

    fn find() -> io::Result<String> {
        let platform = env::var_os("SDKMAN_PLATFORM")
            .map(to_string("SDKMAN_PLATFORM"))
            .transpose()?
            .unwrap_or_else(infer_platform);
        Ok(platform)
    }

    static SDKMAN_PLATFORM: OnceCell<String> = OnceCell::new();
    SDKMAN_PLATFORM.get_or_init(|| match find() {
        Ok(path) => path,
        Err(err) => panic!("{}", err),
    })
}

pub(crate) fn config() -> &'static [(String, String)] {
    fn load() -> Vec<(String, String)> {
        let mut sdkman_dir = sdkman_dir().to_path_buf();
        sdkman_dir.push("etc");
        sdkman_dir.push("config");
        let sdkman_config = fs::read_to_string(sdkman_dir).unwrap_or_default();

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

fn to_string(var: &str) -> impl FnOnce(OsString) -> io::Result<String> + '_ {
    move |value| {
        value.into_string().map_err(|invalid| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("Invalid {}: Not UTF-8: {}", var, invalid.to_string_lossy()),
            )
        })
    }
}
