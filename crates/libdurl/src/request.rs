use crate::Result;
use console::Color;
use curl::easy::{Auth, Easy2, Handler, InfoType};
use curl_sys::CURL;
use libc::c_double;
use libjdkman::{eprintln_color, eprintln_green};
use memmap::MmapMut;
use std::{
    fmt::{Debug, Display},
    fs::{File, OpenOptions},
    io::{self, Write},
    marker::PhantomData,
    mem,
    num::NonZeroUsize,
    path::PathBuf,
    time::{Duration, Instant},
};

pub type DurlResult = Result<ResponseTimings, RequestError>;

pub struct DurlRequest(pub(crate) Easy2<DurlRequestHandler>);

pub enum Uninitialized {}
pub enum OutputStage {}
pub enum BuildStage {}

#[derive(Debug, Clone)]
pub struct DurlRequestBuilder<'url, Stage> {
    url: Option<&'url str>,
    output: Option<PathBuf>,
    overwrite_target: bool,
    use_mmap: bool,
    verbose: bool,
    progress: bool,
    _stage: PhantomData<Stage>,
}

impl<S> DurlRequestBuilder<'_, S> {
    pub fn overwrite_target(&mut self, overwrite_target: bool) -> &mut Self {
        self.overwrite_target = overwrite_target;
        self
    }

    pub fn use_mmap(&mut self, use_mmap: bool) -> &mut Self {
        self.use_mmap = use_mmap;
        self
    }

    pub fn verbose(&mut self, verbose: bool) -> &mut Self {
        self.verbose = verbose;
        self
    }

    pub fn progress(&mut self, progress: bool) -> &mut Self {
        self.progress = progress;
        self
    }
}

impl DurlRequestBuilder<'static, Uninitialized> {
    pub fn new() -> Self {
        Self {
            url: None,
            output: None,
            overwrite_target: false,
            use_mmap: true,
            verbose: false,
            progress: false,
            _stage: PhantomData,
        }
    }

    pub fn url(self, url: &str) -> DurlRequestBuilder<OutputStage> {
        DurlRequestBuilder {
            url: Some(url),
            output: self.output,
            overwrite_target: self.overwrite_target,
            use_mmap: self.use_mmap,
            verbose: self.verbose,
            progress: self.progress,
            _stage: PhantomData,
        }
    }
}

impl<'url> DurlRequestBuilder<'url, OutputStage> {
    pub fn output(self, output: impl Into<PathBuf>) -> DurlRequestBuilder<'url, BuildStage> {
        DurlRequestBuilder {
            url: self.url,
            output: Some(output.into()),
            overwrite_target: self.overwrite_target,
            use_mmap: self.use_mmap,
            verbose: self.verbose,
            progress: self.progress,
            _stage: PhantomData,
        }
    }
}

impl DurlRequestBuilder<'_, BuildStage> {
    pub fn build(&mut self) -> Result<DurlRequest> {
        Ok(DurlRequest::new(
            self.url.take().unwrap(),
            self.output.take().unwrap(),
            self.overwrite_target,
            self.use_mmap,
            self.verbose,
            self.progress,
        )?)
    }
}

impl Default for DurlRequestBuilder<'static, Uninitialized> {
    fn default() -> Self {
        Self::new()
    }
}

pub(crate) struct DurlRequestHandler {
    first_header_in: bool,
    read_content_length: bool,
    is_redirect: bool,
    use_mmap: bool,
    content_length: Option<NonZeroUsize>,
    error: Option<RequestError>,
    progress: ResponseProgress,
    output: ResponseOutput,
    curl_handle: *mut CURL,
}

#[derive(Debug)]
pub struct ResponseTimings {
    namelookup_time: Duration,
    connect_time: Duration,
    appconnect_time: Duration,
    pretransfer_time: Duration,
    starttransfer_time: Duration,
    redirect_time: Duration,
    total_time: Duration,
}

#[derive(Debug)]
pub enum RequestError {
    SetLengthFailed {
        length: usize,
        path: PathBuf,
        error: io::Error,
    },
    FileWriteFailed {
        length: usize,
        path: PathBuf,
        error: io::Error,
    },
    MemoryMapFailed {
        length: usize,
        path: PathBuf,
        error: io::Error,
    },
    CurlError(curl::Error),
    MultiError(Vec<RequestError>),
}

struct ResponseProgress {
    start: Instant,
    next_progress_at: u64,
    last_logged: Option<Instant>,
}

struct ResponseOutput {
    path: Option<PathBuf>,
    file: File,
    map: Option<MappedFile>,
}

struct MappedFile {
    map: MmapMut,
    offset: usize,
    len: usize,
}

impl DurlRequest {
    pub fn new(
        url: &str,
        path: PathBuf,
        overwrite_target: bool,
        use_mmap: bool,
        verbose: bool,
        progress: bool,
    ) -> io::Result<Self> {
        let handle =
            DurlRequestHandler::new(url, path, overwrite_target, use_mmap, verbose, progress)?;
        Ok(DurlRequest(handle))
    }

    pub fn perform(self) -> DurlResult {
        DurlRequestHandler::perform(self.0)
    }
}

impl DurlRequestHandler {
    pub(crate) fn new(
        url: &str,
        path: PathBuf,
        overwrite_target: bool,
        use_mmap: bool,
        verbose: bool,
        progress: bool,
    ) -> io::Result<Easy2<Self>> {
        let output = ResponseOutput::new(path, overwrite_target)?;

        let handler = DurlRequestHandler {
            first_header_in: true,
            read_content_length: false,
            is_redirect: false,
            use_mmap,
            content_length: None,
            error: None,
            progress: ResponseProgress::new(),
            output,
            curl_handle: std::ptr::null_mut(),
        };

        // new curl easy that connects with the handler
        let mut handle = Easy2::new(handler);
        let curl_handle = handle.raw();
        handle.get_mut().set_handle(curl_handle);

        // debug info
        let curl_version = curl::Version::get();

        // user-agent is based on the linked libcurl
        handle.useragent(&format!("libcurl/{}", curl_version.version()))?;

        // connect to this url
        handle.url(url)?;

        // force GET request (not really required)
        // handle.get(true)?;

        // enable verbose logging
        if verbose {
            handle.verbose(true)?;
        }

        // enable progress logging
        if progress {
            handle.progress(true)?;
        }

        // connection close
        // handle.forbid_reuse(true)?;
        handle.forbid_reuse(false)?;

        // try HTTP/2, fallback to HTTP/1.1 // TODO: fall back to 1.1 on error
        let _ = handle.http_version(curl::easy::HttpVersion::V2TLS);

        // automatic decompression
        handle.accept_encoding("")?;

        // disable auth
        handle.http_auth(&Auth::new())?;

        // follow redirects
        handle.follow_location(true)?;

        Ok(handle)
    }

    fn perform(mut handle: Easy2<Self>) -> DurlResult {
        handle.get_mut().progress.start = Instant::now();
        handle.perform()?;
        Self::finish_response(handle)
    }

    pub(crate) fn finish_response(mut handle: Easy2<Self>) -> DurlResult {
        if let Some(err) = handle.get_mut().error.take() {
            return Err(err);
        }
        Ok(ResponseTimings {
            namelookup_time: handle.namelookup_time()?,
            connect_time: handle.connect_time()?,
            appconnect_time: handle.appconnect_time()?,
            pretransfer_time: handle.pretransfer_time()?,
            starttransfer_time: handle.starttransfer_time()?,
            redirect_time: handle.redirect_time()?,
            total_time: handle.total_time()?,
        })
    }

    fn set_handle(&mut self, handle: *mut CURL) {
        assert!(!handle.is_null(), "curl is NULL (not init?)");
        self.curl_handle = handle;
    }

    fn try_find_content_length(&mut self) -> Option<NonZeroUsize> {
        if !self.read_content_length && !self.curl_handle.is_null() {
            self.read_content_length = true;
            let mut p = 0 as c_double;
            let rc = unsafe {
                // SAFETY: self.curl_handle is not NULL and we are "inside" a request
                curl_sys::curl_easy_getinfo(
                    self.curl_handle,
                    curl_sys::CURLINFO_CONTENT_LENGTH_DOWNLOAD,
                    &mut p,
                )
            };

            if rc == curl_sys::CURLE_OK {
                let p = p as f64;
                // we interpret a 0 size as error
                if f64::is_finite(p) && p >= 1.0 && p <= (usize::MAX as f64) {
                    let p = unsafe {
                        // SAFETY:
                        // - p is not NaN because is_finite returned true
                        // - p is not infinite because is_finite returned true
                        // - p is smaller-or-equal to usize::MAX
                        let p = p.to_int_unchecked::<usize>();
                        // SAFETY: p is at least 1
                        NonZeroUsize::new_unchecked(p)
                    };
                    self.content_length = Some(p);
                    eprintln_green!("Found Content-Length: {}", p);
                }
            } else {
                let err = curl::Error::new(rc);
                eprint!(
                    "Encountered error while reading the content-length: {}",
                    err
                );
            }
        }
        self.content_length
    }

    fn parse_status_line(&mut self, data: &[u8]) -> Option<()> {
        let mut positions = data.iter();

        let version_len = positions.position(|&b| b == b' ')?;
        let status_len = positions.position(|&b| b == b' ')?;

        if status_len != 3 || version_len <= 5 {
            return None;
        }

        match &data[..version_len] {
            b"HTTP/0.9" => {}
            b"HTTP/1.0" | b"HTTP/1.1" => {}
            b"HTTP/2" | b"HTTP/2.0" => {}
            b"HTTP/3" | b"HTTP/3.0" => {}
            _ => {
                return None;
            }
        };

        let is_redirect = match data[version_len + 1..][..status_len] {
            [b'1', y, z] if y.is_ascii_digit() && z.is_ascii_digit() => false,
            [b'2', y, z] if y.is_ascii_digit() && z.is_ascii_digit() => false,
            [b'3', y, z] if y.is_ascii_digit() && z.is_ascii_digit() => true,
            [b'4', y, z] if y.is_ascii_digit() && z.is_ascii_digit() => false,
            [b'5', y, z] if y.is_ascii_digit() && z.is_ascii_digit() => false,
            _ => return None,
        };
        self.is_redirect = is_redirect;

        Some(())
    }

    fn set_error(&mut self, error: Option<RequestError>) {
        if let Some(error) = error {
            let error = match self.error.take() {
                Some(previous) => match previous {
                    RequestError::MultiError(mut errors) => {
                        errors.push(error);
                        RequestError::MultiError(errors)
                    }
                    _ => RequestError::MultiError(vec![previous, error]),
                },
                None => error,
            };
            self.error = Some(error);
        }
    }
}

impl ResponseOutput {
    pub fn new(path: PathBuf, overwrite_target: bool) -> io::Result<Self> {
        let mut options = OpenOptions::new();
        options.read(true).write(true);

        if overwrite_target {
            options.create(true);
        } else {
            options.create_new(true);
        };

        let file = options.open(&path)?;
        Ok(Self {
            path: Some(path),
            file,
            map: None,
        })
    }

    fn open(&mut self, use_mmap: bool, length: usize) -> Result<(), Option<RequestError>> {
        if let Err(error) = self.file.set_len(length as u64) {
            return Err(self.path.take().map(|path| RequestError::SetLengthFailed {
                length,
                path,
                error,
            }));
        }
        if use_mmap {
            let output = match MappedFile::new(&self.file, length) {
                Ok(output) => output,
                Err(error) => {
                    return Err(self.path.take().map(|path| RequestError::MemoryMapFailed {
                        length,
                        path,
                        error,
                    }));
                }
            };
            self.map = Some(output);
        }
        Ok(())
    }

    fn write(&mut self, data: &[u8]) -> Result<usize, Option<RequestError>> {
        match &mut self.map {
            Some(output) => Ok(output.write(data)),
            None => match self.file.write_all(data) {
                Ok(_) => Ok(data.len()),
                Err(error) => Err(self.path.take().map(|path| RequestError::FileWriteFailed {
                    length: data.len(),
                    path,
                    error,
                })),
            },
        }
    }
}

impl MappedFile {
    fn new(file: &File, len: usize) -> io::Result<Self> {
        let map = unsafe { MmapMut::map_mut(file) }?;
        Ok(Self {
            map,
            offset: 0,
            len,
        })
    }

    fn write(&mut self, mut data: &[u8]) -> usize {
        let start = self.offset;
        let mut end = start + data.len();

        if end > self.len {
            end = self.len;
            data = &data[..(end - start)];
        }

        (&mut self.map[start..end]).copy_from_slice(data);
        // self.map.flush_async_range(start, data.len())?;
        self.offset = end;
        data.len()
    }
}

impl ResponseProgress {
    const PROGRESS_INTERVAL: u64 = (1_u64 << 24) as u64;

    fn new() -> Self {
        Self {
            start: Instant::now(),
            next_progress_at: Self::PROGRESS_INTERVAL,
            last_logged: None,
        }
    }

    fn progress(&mut self, now: u64, total: u64) {
        if total > now {
            let elapsed = self.start.elapsed();
            eprintln_color!(
                console::Color::Blue,
                "[{:?}] done: download {}",
                elapsed,
                ubyte::ByteUnit::Byte(total)
            );
        } else if now > self.next_progress_at {
            self.next_progress_at += Self::PROGRESS_INTERVAL;
            if self
                .last_logged
                .map_or(true, |ll| ll.elapsed() > Duration::from_millis(500))
            {
                self.last_logged = Some(Instant::now());
                let elapsed = self.start.elapsed();
                let msg = if total > 0 {
                    format!(
                        "[{:?}] download: {:5.2}% {}/{}",
                        elapsed,
                        (now as f64) / (total as f64) * 100.0,
                        ubyte::ByteUnit::Byte(now),
                        ubyte::ByteUnit::Byte(total),
                    )
                } else {
                    format!("[{:?}] progress: download: {}", elapsed, now)
                };
                println!("{}", console::style(msg).blue());
            }
        }
    }
}

impl Handler for DurlRequestHandler {
    fn write(&mut self, data: &[u8]) -> Result<usize, curl::easy::WriteError> {
        if self.is_redirect {
            return Ok(0);
        }
        if data.is_empty() {
            return Ok(0);
        }
        // read content length at first data packet and open file for writing
        if let Some(cl) = self.try_find_content_length() {
            if let Err(error) = self.output.open(self.use_mmap, cl.get()) {
                self.set_error(error);
                // signal error to curl
                return Ok(0);
            }
        }

        match self.output.write(data) {
            Ok(len) => Ok(len),
            Err(error) => {
                self.set_error(error);
                // signal error to curl
                return Ok(0);
            }
        }
    }

    fn header(&mut self, data: &[u8]) -> bool {
        self.parse_status_line(data);
        true
    }

    fn progress(&mut self, dltotal: f64, dlnow: f64, _ultotal: f64, _ulnow: f64) -> bool {
        if dltotal > 0.0 {
            self.progress.progress(dlnow as u64, dltotal as u64);
        }

        true
    }

    fn debug(&mut self, kind: InfoType, data: &[u8]) {
        let style = match kind {
            InfoType::Text => console::Style::new().for_stderr().dim(),
            InfoType::HeaderIn => console::Style::new().for_stderr().bold().fg(
                if mem::take(&mut self.first_header_in) {
                    Color::Green
                } else {
                    Color::Cyan
                },
            ),
            InfoType::HeaderOut => {
                self.first_header_in = true;
                console::Style::new().for_stderr().dim().magenta()
            }
            _ => return,
        };
        match std::str::from_utf8(data) {
            Ok(s) => eprint!("{}", style.apply_to(s)),
            Err(_) => {
                let msg = format!("({} bytes of data)", data.len());
                eprint!("{}", console::style(msg).red())
            }
        }
    }
}

impl Display for RequestError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RequestError::SetLengthFailed {
                length,
                path,
                error,
            } => f.write_fmt(format_args!(
                "could not set the file size on [{}] to {} bytes: {}",
                path.display(),
                length,
                error
            )),
            RequestError::FileWriteFailed {
                length,
                path,
                error,
            } => f.write_fmt(format_args!(
                "could write to [{}], num_bytes={}: {}",
                path.display(),
                length,
                error
            )),
            RequestError::MemoryMapFailed {
                length,
                path,
                error,
            } => f.write_fmt(format_args!(
                "could not memory-map [{}] for writing {} bytes: {}",
                path.display(),
                length,
                error
            )),
            RequestError::CurlError(error) => {
                f.write_fmt(format_args!("Underlying curl error: {}", error))
            }
            RequestError::MultiError(errors) => match &errors[..] {
                [] => f.write_str("Empty errors"),
                [error] => Display::fmt(error, f),
                [init @ .., last] => {
                    f.write_str("Multiple errors: [")?;
                    for error in init {
                        f.write_fmt(format_args!("{}, ", error))?;
                    }
                    f.write_fmt(format_args!("{}]", last))
                }
            },
        }
    }
}

impl std::error::Error for RequestError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            RequestError::SetLengthFailed { error, .. } => Some(error),
            RequestError::FileWriteFailed { error, .. } => Some(error),
            RequestError::MemoryMapFailed { error, .. } => Some(error),
            RequestError::CurlError(error) => Some(error),
            RequestError::MultiError(errors) => errors.iter().find_map(|e| e.source()),
        }
    }
}

impl From<curl::Error> for RequestError {
    fn from(val: curl::Error) -> Self {
        RequestError::CurlError(val)
    }
}
