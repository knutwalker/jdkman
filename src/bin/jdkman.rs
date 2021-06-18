use std::{
    ffi::{OsStr, OsString},
    fmt::Display,
};

use libjdkman::prelude::*;
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
            fzf_bin = ::libjdkman::fzf_command().to_string_lossy()
        );
    };

    ("use", $wants_help:ident) => {
        help!(
            $wants_help,
            "[use] [QUERY]",
            "Select the version to use in this shell session.\nThis command requires `fzf` to be installed!\n\n",
            include_str!("help_use.txt"),
            fzf_bin = ::libjdkman::fzf_command().to_string_lossy()
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
        UseResult::Use {
            name,
            java_home,
            path,
        } => {
            eprintln_green!("Using java version {} in this shell.", name);
            let java_home = std::env::join_paths([java_home])?;
            let java_home = java_home.to_str().ok_or(InvalidPath)?;
            let path = path
                .as_ref()
                .map(|p| p.to_str().ok_or(InvalidPath))
                .transpose()?;

            println!("export JAVA_HOME={}", java_home);
            if let Some(path) = path {
                println!("export PATH={}", path);
            }
        }
        UseResult::Invalid(Some(name)) => {
            eprintln_red!("Stop! Candidate version {} is not installed.", name);
        }
        UseResult::Invalid(None) => {
            eprintln_red!("Stop! Candidate version is not installed.");
        }
        UseResult::KeepCurrent => {}
    }
    Ok(())
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
            let query = expect_query_arg(args)?;
            let result = JdkDefault::run(query, verbose_flag)?;
            match result {
                DefaultResult::KeepCurrent => {}
                DefaultResult::CandidateNotFound { query: Some(name) } => {
                    eprintln_red!("Stop! Candidate version {} is not installed.", name);
                }
                DefaultResult::CandidateNotFound { .. } => {
                    eprintln_red!("Stop! Candidate version is not installed.");
                }
                DefaultResult::Selected {
                    name,
                    before: Some(before),
                } => {
                    eprintln_green!("Default java version set from {} to {}", before, name);
                }
                DefaultResult::Selected { name, .. } => {
                    eprintln_green!("Default java version set to {}", name);
                }
            }
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
