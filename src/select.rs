use once_cell::sync::OnceCell;
use std::{
    convert::TryFrom,
    env,
    ffi::{OsStr, OsString},
    fmt::Display,
    io::{self, Write},
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

pub(crate) enum Selection<T> {
    Selected(T),
    NoMatch,
    Cancelled,
}

#[derive(Debug)]
pub(crate) struct SelectOptions<'a, T> {
    items: Vec<T>,
    pre_select: Option<u32>,
    query: Option<&'a OsStr>,
    preview: Option<&'a str>,
    tac: bool,
    verbose: bool,
}

impl<'a, T> Default for SelectOptions<'a, T> {
    fn default() -> Self {
        Self {
            items: Vec::new(),
            pre_select: None,
            query: None,
            preview: None,
            tac: true,
            verbose: false,
        }
    }
}

impl<'a, T> SelectOptions<'a, T> {
    pub(crate) fn new(items: Vec<T>) -> Self {
        Self {
            items,
            ..Self::default()
        }
    }

    pub(crate) fn pre_select(&mut self, item: impl Into<Option<u32>>) -> &mut Self {
        let item = item.into();
        if let Some(item) = item {
            assert!(
                (item as usize) < self.items.len(),
                "The pre-selected items must smaller than {}",
                self.items.len()
            );
        }
        self.pre_select = item;
        self
    }

    pub(crate) fn query(&mut self, query: impl Into<Option<&'a OsStr>>) -> &mut Self {
        self.query = query.into();
        self
    }

    pub(crate) fn preview_command(&mut self, preview: impl Into<Option<&'a str>>) -> &mut Self {
        self.preview = preview.into();
        self
    }

    pub(crate) fn verbose(&mut self, verbose: bool) -> &mut Self {
        self.verbose = verbose;
        self
    }
}

impl<'a, T: Display> SelectOptions<'a, T> {
    pub(crate) fn select(&mut self) -> io::Result<Selection<T>> {
        fzf_select(std::mem::take(self))
    }
}

pub(crate) fn fzf_select<T: Display>(options: SelectOptions<T>) -> io::Result<Selection<T>> {
    const DELIMITER: &str = "\x1F";

    let mut fzf_command = Command::new(fzf_command());
    fzf_command
        .env_clear()
        .envs(sh_path())
        .arg("--exact")
        .arg("-i")
        .args(["--with-nth", "2"])
        .args(["--delimiter", DELIMITER])
        .arg("--no-sort")
        .args(options.tac.then_some("--tac"))
        .args(["--bind", "enter:accept-non-empty"])
        .args(["--bind", "double-click:accept-non-empty"])
        .args(["--bind", "esc:cancel"])
        .args(["--prompt", " "])
        .args(crate::use_color().then_some("--ansi"))
        .arg("--no-bold")
        .args(options.preview.into_iter().flat_map(|p| {
            [
                "--preview",
                p,
                "--preview-window",
                "right:sharp:hidden",
                "--bind",
                "?:toggle-preview",
            ]
        }))
        .args(
            options
                .query
                .into_iter()
                .flat_map(|q| [OsStr::new("--query"), q]),
        )
        .arg("--select-1")
        .arg("--exit-0")
        .arg("--sync")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit());

    if options.verbose {
        eprintln!("Calling fzf with the command: {:?}", fzf_command);
    }

    let mut child = fzf_command.spawn()?;

    let mut stdin = child
        .stdin
        .take()
        .expect("Failed to open stdin but it is piped");

    let pre_select = options
        .pre_select
        .and_then(|x| usize::try_from(x).ok())
        .unwrap_or(usize::MAX);

    for (index, item) in options.items.iter().enumerate() {
        let item = if index == pre_select && crate::use_color() {
            console::style(item).bold()
        } else {
            console::style(item)
        };
        writeln!(
            stdin,
            "{index}{delimiter}{item}",
            index = index,
            delimiter = DELIMITER,
            item = item,
        )?;
    }

    drop(stdin);

    let output = child.wait_with_output()?;
    let status = output.status;

    if options.verbose {
        eprintln!("fzf returned with {:?}", status);
    }

    // EXIT STATUS
    // 0      Normal exit
    // 1      No match
    // 2      Error
    // 130    Interrupted with CTRL-C or ESC

    match status.code() {
        Some(1) => Ok(Selection::NoMatch),
        Some(130) => Ok(Selection::Cancelled),
        Some(0) => {
            let mut items = options.items;
            let output = output.stdout;
            let index = output
                .into_iter()
                .take_while(|b| *b != DELIMITER.as_bytes()[0])
                .fold(0, |a, c| a * 10 + (c & 0x0F) as usize);

            Ok(Selection::Selected(items.swap_remove(index)))
        }
        otherwise => {
            let output = String::from_utf8(output.stderr).expect("non utf8 stderr");
            let ec = otherwise.unwrap_or(-1);
            println!("Non-zero exit-code: {}: {}", ec, output);
            std::process::exit(ec)
        }
    }
}

pub fn fzf_command() -> &'static OsStr {
    fn find() -> OsString {
        env::var_os("JDKMAN_FZF")
            .or_else(|| search_path("fzf").map(PathBuf::into_os_string))
            .unwrap_or_else(|| OsString::from("fzf"))
    }

    static FZF_COMMAND: OnceCell<OsString> = OnceCell::new();
    FZF_COMMAND.get_or_init(find).as_os_str()
}

fn sh_path() -> Option<(&'static str, &'static OsStr)> {
    fn find() -> Option<OsString> {
        search_path("sh").and_then(|mut p| p.pop().then(|| p.into_os_string()))
    }

    static SH_COMMAND: OnceCell<Option<OsString>> = OnceCell::new();
    SH_COMMAND
        .get_or_init(find)
        .as_deref()
        .map(|path| ("PATH", path))
}

fn search_path<P: AsRef<Path>>(bin: P) -> Option<PathBuf> {
    env::var_os("PATH").and_then(|path| {
        let bin = bin.as_ref();
        env::split_paths(&path).find_map(|mut bin_path| {
            bin_path.push(bin);

            let meta = bin_path.metadata().ok()?;
            if !meta.is_file() {
                return None;
            }

            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                let mode = meta.permissions().mode();
                let can_exec = mode & 0o400 == 0o400;
                if !can_exec {
                    return None;
                }
            }

            Some(bin_path)
        })
    })
}
