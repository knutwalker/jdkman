mod request;

pub use request::{
    DropResponse, DurlRequest, DurlRequestBuilder, DurlResponse, DurlResult, FromTarget,
    ProgressHandler, RequestError, ResponseTimings, Target, VerboseHandler, VerboseMessage,
    WriteToBytes, WriteToFile, WriteToStdErr, WriteToStdOut, WriteToString, WriteToWriter,
};

pub type Result<A, E = Box<dyn std::error::Error>> = std::result::Result<A, E>;

/// print version line
pub fn print_version(to: impl std::io::Write) {
    fn _print(mut to: impl std::io::Write) -> std::io::Result<()> {
        let curl_version = curl::Version::get();
        write!(to, "Curl: {}, Protocols:", curl::Version::num())?;
        for proto in curl_version.protocols() {
            write!(to, " {}", proto)?;
        }
        writeln!(to)
    }

    if let Err(e) = _print(to) {
        panic!("failed printing curl version: {}", e);
    }
}
