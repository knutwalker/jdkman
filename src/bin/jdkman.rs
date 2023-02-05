use std::{
    ffi::{OsStr, OsString},
    fmt::Display,
};

use jdkman::prelude::*;
use pico_args::Arguments;

type Result<A> = std::result::Result<A, Box<dyn std::error::Error + 'static>>;

macro_rules! help {
    ($wants_help:ident) => {
        help!($wants_help, "COMMAND", "", include_str!("help_main.txt"),);
    };

    ("current", $wants_help:ident) => {
        help!($wants_help, "current", "Show the current java version for this shell\n\n");
    };

    ("default", $wants_help:ident) => {
        help!(
            $wants_help,
            "default [QUERY]",
            "Change the default version for all future shells.\nThis command requires `fzf` to be installed!\n\n",
            include_str!("help_default.txt"),
            fzf_bin = ::jdkman::fzf_command().to_string_lossy()
        );
    };

    ("list", $wants_help:ident) => {
        help!(
            $wants_help,
            "list",
            "List all locally installed versions or all available versions.\n\n",
            include_str!("help_list.txt"),
        );
    };

    ("install", $wants_help:ident) => {
        help!(
            $wants_help,
            "install",
            "Install a new version for further use.\n\n",
            include_str!("help_install.txt"),
        );
    };

    ("use", $wants_help:ident) => {
        help!(
            $wants_help,
            "[use] [QUERY]",
            "Select the version to use in this shell session.\nThis command requires `fzf` to be installed!\n\n",
            include_str!("help_use.txt"),
            fzf_bin = ::jdkman::fzf_command().to_string_lossy()
        );
    };

    ($wants_help:ident, $command:literal, $summary:literal) => {
        help!($wants_help, $command, $summary, "",);
    };

    ($wants_help:ident, $command:literal, $summary:literal, $body:expr, $($args:tt)*) => {
        if $wants_help {
            eprint!(
                concat!(
                    env!("CARGO_BIN_NAME"),
                    " ",
                    env!("CARGO_PKG_VERSION"),
                    "\n",
                    env!("CARGO_PKG_DESCRIPTION"),
                    "\n\n",
                    $summary,
                    include_str!("help_usage_template.txt"),
                    "\n",
                    $body,
                ),
                bin = env!("CARGO_BIN_NAME"),
                command = $command,
                $($args)*
            );

            return Ok(());
        }
    };
}

fn expect_query_arg(args: Arguments) -> Result<Option<OsString>> {
    let mut args = args.finish();
    let query = match args.split_first_mut() {
        None => None,
        Some((query, [])) => Some(std::mem::take(query)),
        Some((query, unexpected)) => {
            unexpected_args(unexpected, Some(query))?;
            None
        }
    };

    Ok(query)
}

fn expect_no_more_args(args: Arguments) -> Result<()> {
    unexpected_args(args.finish(), None)
}

fn unexpected_args<A>(args: A, query: Option<&OsStr>) -> Result<()>
where
    A: IntoIterator,
    A::Item: AsRef<OsStr>,
{
    let unexpected = args
        .into_iter()
        .map(|a| a.as_ref().to_string_lossy().into_owned())
        .collect::<Vec<_>>();

    if unexpected.is_empty() {
        return Ok(());
    }

    let argument = if unexpected.len() == 1 {
        "argument"
    } else {
        "arguments"
    };

    let msg = match query {
        Some(query) => {
            format!(
                r#"Error: found unexpected {argument}: '{unexpected_quotes}'
In order to use the {argument} in the query, pass it as one argument: "{query} {unexpected_args}"

For more information try --help
"#,
                argument = argument,
                query = query.to_string_lossy(),
                unexpected_quotes = unexpected.join("', '"),
                unexpected_args = unexpected.join(" "),
            )
        }
        None => {
            format!(
                r#"Error: found unexpected {argument}: '{unexpected_quotes}'

For more information try --help
"#,
                argument = argument,
                unexpected_quotes = unexpected.join("', '"),
            )
        }
    };

    Err(msg.into())
}

#[derive(Debug)]
struct InvalidPath;

impl Display for InvalidPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.pad(r"Invalid PATH conversion to UTF-8. This is probably a Windows machine, sooooooo ¯\_(ツ)_/¯")
    }
}

impl std::error::Error for InvalidPath {}

fn run_use(query: Option<&OsStr>, verbose: bool) -> Result<()> {
    let use_result = JdkUse::run(query, verbose)?;
    match use_result {
        UseResult::Use(using) => print_using(using)?,
        UseResult::Invalid(Some(name)) => {
            eprintln_red!("Stop! Candidate version {} is not installed.", name)
        }
        UseResult::Invalid(None) => {
            eprintln_red!("Stop! Candidate version is not installed.");
        }
        UseResult::KeepCurrent => {}
    }
    Ok(())
}

fn print_using(
    Using {
        name,
        java_home,
        path,
    }: Using,
) -> Result<()> {
    let path = path
        .as_ref()
        .map(|p| p.to_str().ok_or(InvalidPath))
        .transpose()?;

    eprintln_green!("Using java version {} in this shell.", name);
    println!("export JAVA_HOME=\"{}\"", java_home.display());
    if let Some(path) = path {
        println!("export PATH=\"{}\"", path);
    }
    Ok(())
}

fn print_changed_default(change: ChangedDefault) {
    match change {
        ChangedDefault {
            name,
            before: Some(before),
        } => {
            eprintln_green!("Default java version set from {} to {}", before, name);
        }
        ChangedDefault { name, .. } => {
            eprintln_green!("Default java version set to {}", name);
        }
    }
}

pub(crate) fn run() -> Result<()> {
    let mut args = pico_args::Arguments::from_env();
    let wants_help = args.contains(["-h", "--help"]);
    let verbose_flag = args.contains(["-v", "--verbose"]);
    let subcommand = args.subcommand()?;

    match subcommand.as_deref() {
        Some("current" | "c") => {
            help!("current", wants_help);
            expect_no_more_args(args)?;
            let current = JdkCurrent::run();
            match current {
                Some(current) => eprintln!("Using java version {}", current.name()),
                None => eprintln_red!("Not using any version of java"),
            }
        }
        Some("default" | "d") => {
            help!("default", wants_help);
            let show_suggested_default = args.contains("--suggest");
            let query = expect_query_arg(args)?;
            let result = JdkDefault::run(query, show_suggested_default, verbose_flag)?;
            match result {
                DefaultResult::KeepCurrent => {}
                DefaultResult::CandidateNotFound { query: Some(name) } => {
                    eprintln_red!("Stop! Candidate version {} is not installed.", name);
                }
                DefaultResult::CandidateNotFound { .. } => {
                    eprintln_red!("Stop! Candidate version is not installed.");
                }
                DefaultResult::Selected(change) => print_changed_default(change),
                DefaultResult::Suggest(name) => {
                    eprintln_yellow!("Suggested default version: {}", name);
                }
            }
        }
        Some("install" | "i" | "in") => {
            help!("install", wants_help);
            let set_default = args.contains(["-d", "--set-default"]);
            let set_use = args.contains(["-u", "--use"]);
            let validate = !args.contains("--unchecked");
            let use_suggested = args.contains("--suggested");
            let candidate = expect_query_arg(args)?;
            let candidate = candidate.and_then(|c| c.into_string().ok());
            let candidate = candidate.as_deref().unwrap_or_default();

            let result = JdkInstall::run(verbose_flag, use_suggested, validate, candidate);
            match result {
                Ok(candidate) => {
                    eprintln_green!("Done installing {}!", candidate.name());

                    if set_default {
                        let change = JdkDefault::set(candidate.clone())?;
                        print_changed_default(change);
                    }
                    if set_use {
                        let using = JdkUse::set(candidate);
                        print_using(using)?;
                    }
                }
                Err(err) => match err {
                    InstallError::AlreadyInstalled(v) => {
                        eprintln_yellow!("java {} is already installed.", v);
                    }
                    InstallError::InvalidVersion(v) => {
                        eprintln_red!("Stop! {} is not a valid java version.", v);
                    }
                    InstallError::ArchiveCorrupt(v, archive) => {
                        eprintln_red!("Stop! {} could not be installed because the archive '{}' is corrupt. This could be due to a download or disk error. Remove the archive if it still exists and try again.", v, archive.display());
                    }
                    InstallError::DownloadError(err) => {
                        eprintln_red!("Stop! Could not download java {}: {}", candidate, err);
                    }
                    InstallError::Other(err) => {
                        eprintln_red!("Stop! Could not install java {}: {}", candidate, err);
                    }
                },
            }
        }
        Some("list" | "ll" | "ls" | "l") => {
            help!("list", wants_help);
            let all_flag = args.contains(["-a", "--all"]);
            expect_no_more_args(args)?;
            let result = JdkList::run(verbose_flag, all_flag)?;
            eprint!("{}", result);
        }
        Some("use" | "us") => {
            help!("use", wants_help);
            let query = expect_query_arg(args)?;
            run_use(query.as_deref(), verbose_flag)?;
        }
        query => {
            help!(wants_help);
            let query = query.map(OsStr::new);
            unexpected_args(args.finish(), query)?;
            run_use(query, verbose_flag)?;
        }
    };

    Ok(())
}

pub fn main() {
    if let Err(err) = run() {
        eprintln!("{}", err);
        std::process::exit(1)
    }
}
