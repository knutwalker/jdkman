use console::{Color, Style};
use libdurl::{DurlClient, DurlRequestBuilder, DurlResult, VerboseMessage};
use libjdkman::{eprint_color, eprintln_color, eprintln_red};
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

fn print_progress(now: u64, total: u64, elapsed: Duration) {
    if now == total {
        eprintln_color!(
            console::Color::Blue,
            "[{:?}] done: download {}",
            elapsed,
            ubyte::ByteUnit::Byte(total)
        );
    } else {
        if total > 0 {
            eprintln_color!(
                console::Color::Blue,
                "[{:?}] download: {:5.2}% {}/{}",
                elapsed,
                (now as f64) / (total as f64) * 100.0,
                ubyte::ByteUnit::Byte(now),
                ubyte::ByteUnit::Byte(total),
            );
        } else {
            eprintln_color!(
                console::Color::Blue,
                "[{:?}] progress: download: {}",
                elapsed,
                now
            );
        };
    }
}

fn print_verbose(msg: VerboseMessage, data: &[u8]) {
    let style = match msg {
        VerboseMessage::Text => Style::new().for_stderr().dim(),
        VerboseMessage::OutgoingHeader => Style::new().for_stderr().dim().magenta(),
        VerboseMessage::FirstIncomingHeader => {
            Style::new()
                .for_stderr()
                .bold()
                .fg(if true { Color::Green } else { Color::Cyan })
        }
        VerboseMessage::IncomingHeader => {
            Style::new()
                .for_stderr()
                .bold()
                .fg(if false { Color::Green } else { Color::Cyan })
        }
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
        r#" [OPTIONS] -o TARGET [URL...]

OPTIONS:
    -v, --verbose    Prints more information on stderr
    -p, --progress   Prints progress during downloading
    -f, --force      Overwrite the output file if it exists
    -o, --output     The output file
        --mmap       Uuse memory mapping for writing the file
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
    let use_mmap = args.contains("--mmap");

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
            .progress_fn(print_progress)
            .progress_interval(Duration::from_millis(500));
    }

    if verbose_flag {
        request.verbose_fn(print_verbose);
    }

    let request = request
        .overwrite_target(force_flag)
        .use_mmap(use_mmap)
        .url(&url)
        .output(output)
        .build()?;

    // print version line
    if verbose_flag {
        DurlClient::print_version();
    }

    let mut client = DurlClient::new()?;
    client.add_request(request, move |res| print_response(verbose_flag, res))?;
    client.send_all()?;

    Ok(())
}

pub fn main() {
    if let Err(err) = run() {
        eprintln!("{}", err);
        std::process::exit(1)
    }
}
