use console::Style;
use libdurl::{DurlRequestBuilder, DurlResult, VerboseMessage};
use libjdkman::{eprint_color, eprintln_color, eprintln_red};
use number_prefix::NumberPrefix;
use std::{error::Error, path::PathBuf, time::Duration};

fn print_response(verbose: bool, response: DurlResult) {
    match response {
        Ok(response) => {
            if verbose {
                println!("{:#?}", response);
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

fn print_progress(now: u64, total: u64, elapsed: Duration) {
    if now == total {
        match NumberPrefix::binary(total as f64) {
            NumberPrefix::Standalone(total) => {
                eprintln_color!(
                    console::Color::Blue,
                    "[{:?}] done: download {} bytes",
                    elapsed,
                    total
                );
            }
            NumberPrefix::Prefixed(prefix, total) => {
                eprintln_color!(
                    console::Color::Blue,
                    "[{:?}] done: download {:.2}{}B",
                    elapsed,
                    total,
                    prefix
                );
            }
        }
    } else if total > 0 {
        match NumberPrefix::binary(total as f64) {
            NumberPrefix::Standalone(total) => {
                eprintln_color!(
                    console::Color::Blue,
                    "[{:?}] download: {:5.2}% {}/{} bytes",
                    elapsed,
                    (now as f64) / total * 100.0,
                    now,
                    total,
                );
            }
            NumberPrefix::Prefixed(prefix, total) => match NumberPrefix::binary(now as f64) {
                NumberPrefix::Standalone(now) => {
                    eprintln_color!(
                        console::Color::Blue,
                        "[{:?}] download: {:5.2}% {} bytes/{:.2}{}B",
                        elapsed,
                        now / total * 100.0,
                        now,
                        total,
                        prefix,
                    );
                }
                NumberPrefix::Prefixed(now_prefix, now) => {
                    eprintln_color!(
                        console::Color::Blue,
                        "[{:?}] download: {:5.2}% {:.2}{}B/{:.2}{}B",
                        elapsed,
                        now / total * 100.0,
                        now,
                        now_prefix,
                        total,
                        prefix,
                    );
                }
            },
        }
    } else {
        eprintln_color!(
            console::Color::Blue,
            "[{:?}] progress: download: {}",
            elapsed,
            now
        );
    };
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
        r#" [OPTIONS] -o TARGET [URL...]

OPTIONS:
    -v, --verbose    Prints more information on stderr
    -p, --progress   Prints progress during downloading
    -f, --force      Overwrite the output file if it exists
    -o, --output     The output file
    -h, --help       Prints this help information on stderr

ARGS:
    URL...           URLs to download

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
    let force_flag = args.contains(["-f", "--force"]);

    let output = args.value_from_os_str(["-o", "--output"], |s| -> Result<_, String> {
        Ok(PathBuf::from(s))
    })?;

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

    if progress_flag {
        request
            .progress_bar()
            .progress_min_download(100_u64 << 10)
            .progress_interval(Duration::from_millis(50));
    }

    if verbose_flag {
        request.verbose_fn(print_verbose);
        if !progress_flag {
            request.progress_fn(print_progress);
        }
    }

    let request = request
        .overwrite_target(force_flag)
        .url(&url)
        .output(output)
        .build()?;

    // print version line
    if verbose_flag {
        libdurl::print_version();
    }

    let response = request.perform();
    print_response(verbose_flag, response);

    Ok(())
}

pub fn main() {
    if let Err(err) = run() {
        eprintln!("{}", err);
        std::process::exit(1)
    }
}
