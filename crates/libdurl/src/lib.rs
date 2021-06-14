#[cfg(feature = "client")]
mod client;
mod request;
#[cfg(feature = "client")]
mod selector;

#[cfg(feature = "client")]
pub use client::DurlClient;
pub use request::{
    DurlRequest, DurlRequestBuilder, DurlResult, ProgressHandler, RequestError, ResponseTimings,
    VerboseHandler, VerboseMessage,
};

pub type Result<A, E = Box<dyn std::error::Error>> = std::result::Result<A, E>;

/// print version line
pub fn print_version() {
    let curl_version = curl::Version::get();
    print!("Curl: {}, Protocols:", curl::Version::num());
    for proto in curl_version.protocols() {
        print!(" {}", proto);
    }
    println!();
}
