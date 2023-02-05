mod request;

pub use request::{DurlRequestBuilder, RequestError, VerboseMessage};

pub type Result<A, E = Box<dyn std::error::Error>> = std::result::Result<A, E>;
