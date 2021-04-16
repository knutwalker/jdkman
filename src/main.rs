use once_cell::sync::OnceCell;

mod candidate;
mod commands;
mod sdkman;
mod select;

fn use_color() -> bool {
    fn check_for_colors() -> bool {
        let use_color = if clicolors_control::colors_enabled() {
            // check if colors are explicitly disabled
            sdkman::config()
                .iter()
                .all(|(k, v)| k != "sdkman_colour_enable" || v != "false")
        } else {
            // check if colors are explicitly enabled
            sdkman::config()
                .iter()
                .any(|(k, v)| k == "sdkman_colour_enable" && v == "true")
        };
        console::set_colors_enabled(use_color);
        use_color
    }

    static SDKMAN_COLOR_ENABLED: OnceCell<bool> = OnceCell::new();
    *SDKMAN_COLOR_ENABLED.get_or_init(check_for_colors)
}

macro_rules! eprintln_color {
    ($color:path, $($arg:tt)*) => ({
        if $crate::use_color() {
            let text = ::std::fmt::format(format_args!($($arg)*));
            eprintln!("{}", ::console::style(text).fg($color));
        } else {
            eprintln!($($arg)*);
        }
    })
}

macro_rules! eprintln_green {
    ($($arg:tt)*) => { eprintln_color!(::console::Color::Green, $($arg)*); }
}

macro_rules! eprintln_red {
    ($($arg:tt)*) => { eprintln_color!(::console::Color::Red, $($arg)*); }
}

mod cli {
    use crate::{
        candidate::Candidate,
        commands::{
            current_command,
            use_command::{self, UseResult},
        },
        sdkman,
    };

    fn print_help() {
        eprintln!(concat!(
            env!("CARGO_PKG_NAME"),
            " ",
            env!("CARGO_PKG_VERSION")
        ));
        eprintln!(env!("CARGO_PKG_DESCRIPTION"));
        eprintln!(
            r#"
USAGE:
    jdkman [OPTIONS] [QUERY]

OPTIONS:
        --current    Only show the current version and quit
    -v, --verbose    Prints more information on stderr
    -h, --help       Prints this help information on stderr

ARGS:
    <QUERY>    Optional query to start a selection


Upon successful execution, jdkman prints shell code to stdout that
needs to be evaluated in order to provide all the functionality,
e.g. with `jdk () {{ $(jdkman "$*") }}`.
"#
        );
    }

    pub(crate) fn run() -> Result<(), Box<dyn std::error::Error + 'static>> {
        let mut args = pico_args::Arguments::from_env();
        if args.contains(["-h", "--help"]) {
            print_help();
            return Ok(());
        }

        let current_flag = args.contains("--current");
        let verbose_flag = args.contains(["-v", "--verbose"]);
        let args = args.finish();
        let query = match args.split_first() {
            None => None,
            Some((query, [])) => Some(query.as_os_str()),
            Some((query, unexpected)) => {
                let unexpected = unexpected
                    .iter()
                    .map(|a| a.to_string_lossy().into_owned())
                    .collect::<Vec<_>>();

                let argument = if unexpected.len() == 1 {
                    "argument"
                } else {
                    "arguments"
                };

                let msg = format!(
                    r#"
Error: found unexpected {argument}: '{unexpected_quotes}'
In order to use the {argument} in the query, pass it as one argument: "{query} {unexpected_args}"

For more information try --help
"#,
                    argument = argument,
                    query = query.to_string_lossy(),
                    unexpected_quotes = unexpected.join("', '"),
                    unexpected_args = unexpected.join(" "),
                );

                return Err(msg.into());
            }
        };

        let current = current_command::run(sdkman::candidates_dir());
        if current_flag {
            match current {
                Some(current) => eprintln!("Using java version {}", current.name()),
                None => eprintln_red!("Not using any version of java"),
            }
            return Ok(());
        }

        let use_result = use_command::run(
            query,
            current.as_ref().map(Candidate::name),
            sdkman::candidates_dir(),
            verbose_flag,
        )?;
        match use_result {
            UseResult::Use {
                name,
                java_home,
                path,
            } => {
                eprintln_green!("Using java version {} in this shell.", name);
                println!("export JAVA_HOME=\"{}\"", java_home.display());
                if let Some(path) = path {
                    let path = path.to_str().expect(r"Invalid PATH conversion to UTF-8. This is probably a Windows machine, sooooooo ¯\_(ツ)_/¯");
                    println!("export PATH=\"{}\"", path);
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
}

pub fn main() {
    if let Err(err) = cli::run() {
        eprintln!("{}", err);
        std::process::exit(1)
    }
}
