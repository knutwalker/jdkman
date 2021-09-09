use console::Style;
use libdurl::{DurlRequestBuilder, DurlResult, VerboseMessage};
use libjdkman::{eprint_color, eprintln_red};
use std::{error::Error, fmt::Debug, path::PathBuf, time::Duration};
use ubyte::ByteUnit;

fn print_response<T: Debug>(verbose: bool, print_to_stdout: bool, response: DurlResult<T>) {
    match response {
        Ok(response) => {
            if verbose {
                if print_to_stdout {
                    println!("{:#?}", response);
                } else {
                    eprintln!("{:#?}", response);
                }
            }
        }
        Err(err) => {
            eprintln_red!("{}", err);
        }
    }
}

fn print_verbose(msg: VerboseMessage, data: &[u8]) {
    let style = match msg {
        VerboseMessage::Text => Style::new().for_stderr().dim(),
        VerboseMessage::OutgoingHeader => Style::new().for_stderr().dim().magenta(),
        VerboseMessage::FirstIncomingHeader => Style::new().for_stderr().green(),
        VerboseMessage::IncomingHeader => Style::new().for_stderr().cyan(),
    };
    match std::str::from_utf8(data) {
        Ok(s) => eprint_color!(@style, "{}", s),
        Err(_) => eprint_color!(@style.red(), "({} bytes of data)", data.len()),
    }
}

fn print_help() {
    eprintln!(concat!(
        env!("CARGO_BIN_NAME"),
        " ",
        env!("CARGO_PKG_VERSION")
    ));
    eprintln!(env!("CARGO_PKG_DESCRIPTION"));
    eprintln!(concat!(
        r#"
USAGE:
    "#,
        env!("CARGO_BIN_NAME"),
        r#" [OPTIONS] -o TARGET URL

OPTIONS:
    -v, --verbose    Prints more information on stderr
    -p, --progress   Prints progress during downloading
    -f, --force      Overwrite the output file if it exists
    -o, --output     The output file
    -l, --limit      Limit the download speed to LIMIT bytes per second;
    -h, --help       Prints this help information on stderr

ARGS:
    URL              URL to download

"#
    ));
}

fn run() -> Result<(), Box<dyn Error>> {
    let mut args = pico_args::Arguments::from_env();
    if args.contains(["-h", "--help"]) {
        print_help();
        return Ok(());
    }

    let verbose_flag = args.contains(["-v", "--verbose"]);
    let progress_flag = args.contains(["-p", "--progress"]);
    let limit: Option<ByteUnit> = args.opt_value_from_str(["-l", "--limit"])?;

    enum OutputArg {
        Stdout,
        File(PathBuf, bool),
    }

    let mut output = args.value_from_os_str(["-o", "--output"], |s| -> Result<_, String> {
        if s == "-" {
            Ok(OutputArg::Stdout)
        } else {
            Ok(OutputArg::File(s.into(), false))
        }
    })?;

    let target_is_stdout = match &mut output {
        OutputArg::Stdout => true,
        OutputArg::File(_, overwrite) => {
            let force_flag = args.contains(["-f", "--force"]);
            *overwrite = force_flag;
            false
        }
    };

    let url: String = args.free_from_str()?;

    let args = args.finish();
    if !args.is_empty() {
        let unexpected = args.iter().map(|a| a.to_string_lossy()).collect::<Vec<_>>();

        let argument = if unexpected.len() == 1 {
            "argument"
        } else {
            "arguments"
        };

        let msg = format!(
            r#"
Error: found unexpected {argument}: '{unexpected_quotes}'

For more information try --help
"#,
            argument = argument,
            unexpected_quotes = unexpected.join("', '"),
        );

        return Err(msg.into());
    }

    let mut request = DurlRequestBuilder::new();

    if let Some(limit) = limit {
        request.limit_speed(limit.as_u64());
    }

    if progress_flag {
        let min_download = match limit {
            Some(limit) => (limit / 20).as_u64(),
            None => 100 << 10,
        };
        request
            .progress_bar()
            .progress_min_download(min_download)
            .progress_interval(Duration::from_millis(50));
    }

    if verbose_flag {
        request.verbose_fn(print_verbose);
        // print version line
        if target_is_stdout {
            libdurl::print_version(std::io::stderr());
        } else {
            libdurl::print_version(std::io::stdout());
        }
    }

    let mut request = request.url(&url);

    match output {
        OutputArg::Stdout => print_response(
            verbose_flag,
            false,
            request.write_to_stdout().build()?.perform(),
        ),
        OutputArg::File(file, overwrite) => print_response(
            verbose_flag,
            true,
            request.write_to_file(overwrite, file).build()?.perform(),
        ),
    }

    Ok(())
}

pub fn main() {
    if let Err(err) = run() {
        eprintln!("{}", err);
        std::process::exit(1)
    }
}
