use console::Style;
use libdurl::{DurlRequestBuilder, DurlResult, Target, VerboseMessage};
use libjdkman::{eprint_color, eprintln_red};
use std::{error::Error, time::Duration};
use ubyte::ByteUnit;

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

    let mut target = args.value_from_os_str(["-o", "--output"], |s| -> Result<_, String> {
        if s == "-" {
            Ok(Target::stdout())
        } else {
            Ok(Target::file(s, false))
        }
    })?;

    let force_flag = args.contains(["-f", "--force"]);
    target.set_overwrite_if_exists(force_flag);

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
    }

    let request = request.url(&url).target(target).build()?;

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
