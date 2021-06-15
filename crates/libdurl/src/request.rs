use crate::Result;
use curl::easy::{Auth, Easy2, Handler, InfoType};
use curl_sys::CURL;
use libc::c_double;
#[cfg(feature = "memmap")]
use memmap::MmapMut;
use std::{
    fmt::{Debug, Display},
    fs::{File, OpenOptions},
    io::{self, Write},
    marker::PhantomData,
    mem,
    num::{NonZeroU64, NonZeroUsize},
    path::PathBuf,
    time::{Duration, Instant},
};

pub type DurlResult = Result<ResponseTimings, RequestError>;

pub struct DurlRequest(pub(crate) Easy2<DurlRequestHandler>);

pub enum Uninitialized {}
pub enum OutputStage {}
pub enum BuildStage {}

pub struct DurlRequestBuilder<'url, Stage> {
    overwrite_target: bool,
    #[cfg(feature = "memmap")]
    use_mmap: bool,
    speed_limit: Option<NonZeroU64>,

    progress_interval: Duration,
    progress_min_download: u64,
    progress_fn: Option<Box<dyn ProgressHandler>>,

    verbose_fn: Option<Box<dyn VerboseHandler>>,

    url: Option<&'url str>,
    output: Option<PathBuf>,

    _stage: PhantomData<Stage>,
}

impl<S> DurlRequestBuilder<'_, S> {
    pub fn overwrite_target(&mut self, overwrite_target: bool) -> &mut Self {
        self.overwrite_target = overwrite_target;
        self
    }

    #[cfg(feature = "memmap")]
    pub fn use_mmap(&mut self, use_mmap: bool) -> &mut Self {
        self.use_mmap = use_mmap;
        self
    }

    /// bytes per second, 0 = disable
    pub fn limit_speed(&mut self, bytes_per_second: u64) -> &mut Self {
        self.speed_limit = NonZeroU64::new(bytes_per_second);
        self
    }

    pub fn verbose(&mut self, verbose: impl VerboseHandler + 'static) -> &mut Self {
        self.verbose_fn = Some(Box::new(verbose));
        self
    }

    pub fn verbose_fn(
        &mut self,
        verbose: impl FnMut(VerboseMessage, &[u8]) + 'static,
    ) -> &mut Self {
        self.verbose_fn = Some(Box::new(VerboseHandlerFromFn(verbose)));
        self
    }

    pub fn progress(&mut self, progress: impl ProgressHandler + 'static) -> &mut Self {
        self.progress_fn = Some(Box::new(progress));
        self
    }

    pub fn progress_fn(&mut self, progress: impl FnMut(u64, u64, Duration) + 'static) -> &mut Self {
        self.progress_fn = Some(Box::new(ProgressHandlerFromFn(progress)));
        self
    }

    #[cfg(feature = "progress_bar")]
    pub fn progress_bar(&mut self) -> &mut Self {
        self.progress_fn = Some(Box::new(progress_bar::DurlProgress::new()));
        self
    }

    pub fn progress_interval(&mut self, interval: Duration) -> &mut Self {
        self.progress_interval = interval;
        self
    }

    pub fn progress_min_download(&mut self, progress_min_download: impl Into<u64>) -> &mut Self {
        self.progress_min_download = progress_min_download.into();
        self
    }
}

impl DurlRequestBuilder<'static, Uninitialized> {
    pub fn new() -> Self {
        DurlRequestBuilder {
            overwrite_target: false,
            #[cfg(feature = "memmap")]
            use_mmap: true,
            speed_limit: None,
            progress_interval: Duration::from_secs(1),
            progress_min_download: 1 << 20,
            progress_fn: None,
            verbose_fn: None,
            url: None,
            output: None,
            _stage: PhantomData,
        }
    }

    pub fn url<'url>(&mut self, url: &'url str) -> DurlRequestBuilder<'url, OutputStage> {
        DurlRequestBuilder {
            overwrite_target: self.overwrite_target,
            #[cfg(feature = "memmap")]
            use_mmap: self.use_mmap,
            speed_limit: self.speed_limit,
            progress_interval: self.progress_interval,
            progress_min_download: self.progress_min_download,
            progress_fn: self.progress_fn.take(),
            verbose_fn: self.verbose_fn.take(),
            url: Some(url),
            output: None,
            _stage: PhantomData,
        }
    }
}

impl<'url> DurlRequestBuilder<'url, OutputStage> {
    pub fn output(&mut self, output: impl Into<PathBuf>) -> DurlRequestBuilder<'url, BuildStage> {
        DurlRequestBuilder {
            overwrite_target: self.overwrite_target,
            #[cfg(feature = "memmap")]
            use_mmap: self.use_mmap,
            speed_limit: self.speed_limit,
            progress_interval: self.progress_interval,
            progress_min_download: self.progress_min_download,
            progress_fn: self.progress_fn.take(),
            verbose_fn: self.verbose_fn.take(),
            url: self.url.take(),
            output: Some(output.into()),
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
            self.speed_limit,
            #[cfg(feature = "memmap")]
            self.use_mmap,
            self.verbose_fn.take(),
            self.progress_fn
                .take()
                .map(|f| (self.progress_interval, self.progress_min_download, f)),
        )?)
    }
}

impl Default for DurlRequestBuilder<'static, Uninitialized> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum VerboseMessage {
    Text,
    OutgoingHeader,
    FirstIncomingHeader,
    IncomingHeader,
}

pub trait VerboseHandler {
    fn text(&mut self, data: &[u8]);

    fn outgoing_header(&mut self, data: &[u8]);

    fn incoming_header(&mut self, first: bool, data: &[u8]);
}

pub trait ProgressHandler {
    fn progress(&mut self, now: u64, total: u64, elapsed: Duration);

    fn done(&mut self, total: u64, elapsed: Duration);
}

pub(crate) struct DurlRequestHandler {
    first_header_in: bool,
    read_content_length: bool,
    is_redirect: bool,
    #[cfg(feature = "memmap")]
    use_mmap: bool,
    content_length: Option<NonZeroUsize>,
    error: Option<RequestError>,
    verbose: Option<Box<dyn VerboseHandler>>,
    progress: Option<ResponseProgress>,
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
    #[cfg(feature = "memmap")]
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
    last_progress_at: Instant,
    last_progress: u64,
    interval: Duration,
    min_progress: u64,
    progress_fn: Box<dyn ProgressHandler>,
}

struct ResponseOutput {
    path: Option<PathBuf>,
    file: File,
    #[cfg(feature = "memmap")]
    map: Option<MappedFile>,
}

#[cfg(feature = "memmap")]
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
        speed_limit: Option<NonZeroU64>,
        #[cfg(feature = "memmap")] use_mmap: bool,
        verbose_fn: Option<Box<dyn VerboseHandler>>,
        progress_fn: Option<(Duration, u64, Box<dyn ProgressHandler>)>,
    ) -> io::Result<Self> {
        let handle = DurlRequestHandler::new(
            url,
            path,
            overwrite_target,
            speed_limit,
            #[cfg(feature = "memmap")]
            use_mmap,
            verbose_fn,
            progress_fn,
        )?;
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
        speed_limit: Option<NonZeroU64>,
        #[cfg(feature = "memmap")] use_mmap: bool,
        verbose_fn: Option<Box<dyn VerboseHandler>>,
        progress_fn: Option<(Duration, u64, Box<dyn ProgressHandler>)>,
    ) -> io::Result<Easy2<Self>> {
        let verbose = verbose_fn.is_some();
        let progress = progress_fn.is_some();
        let output = ResponseOutput::new(path, overwrite_target)?;

        let handler = DurlRequestHandler {
            first_header_in: true,
            read_content_length: false,
            is_redirect: false,
            #[cfg(feature = "memmap")]
            use_mmap,
            content_length: None,
            error: None,
            verbose: verbose_fn,
            progress: progress_fn.map(ResponseProgress::new),
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

        if let Some(limit) = speed_limit {
            handle.max_recv_speed(limit.get())?;
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
        if let Some(progress) = handle.get_mut().progress.as_mut() {
            progress.start = Instant::now();
        }
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

    pub(crate) fn set_error(&mut self, error: Option<RequestError>) {
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
        Ok(ResponseOutput {
            path: Some(path),
            file,
            #[cfg(feature = "memmap")]
            map: None,
        })
    }

    fn open(
        &mut self,
        #[cfg(feature = "memmap")] use_mmap: bool,
        length: usize,
    ) -> Result<(), Option<RequestError>> {
        if let Err(error) = self.file.set_len(length as u64) {
            return Err(self.path.take().map(|path| RequestError::SetLengthFailed {
                length,
                path,
                error,
            }));
        }
        #[cfg(feature = "memmap")]
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

    #[cfg(not(feature = "memmap"))]
    fn write(&mut self, data: &[u8]) -> Result<usize, Option<RequestError>> {
        self.write_file(data)
    }

    #[cfg(feature = "memmap")]
    fn write(&mut self, data: &[u8]) -> Result<usize, Option<RequestError>> {
        self.write_mapped(data)
    }

    #[inline]
    fn write_file(&mut self, data: &[u8]) -> Result<usize, Option<RequestError>> {
        match self.file.write_all(data) {
            Ok(_) => Ok(data.len()),
            Err(error) => Err(self.path.take().map(|path| RequestError::FileWriteFailed {
                length: data.len(),
                path,
                error,
            })),
        }
    }

    #[cfg(feature = "memmap")]
    #[inline]
    fn write_mapped(&mut self, data: &[u8]) -> Result<usize, Option<RequestError>> {
        match &mut self.map {
            Some(output) => Ok(output.write(data)),
            None => self.write_file(data),
        }
    }
}

#[cfg(feature = "memmap")]
impl MappedFile {
    fn new(file: &File, len: usize) -> io::Result<Self> {
        Ok(MappedFile {
            map: unsafe { MmapMut::map_mut(file) }?,
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
    fn new(
        (interval, min_progress, progress_fn): (Duration, u64, Box<dyn ProgressHandler>),
    ) -> Self {
        let start = Instant::now();
        ResponseProgress {
            start,
            last_progress_at: start,
            last_progress: 0,
            interval,
            min_progress,
            progress_fn,
        }
    }

    fn progress(&mut self, now: u64, total: u64) {
        if now >= total {
            let elapsed = self.start.elapsed();
            self.progress_fn.done(total, elapsed);
        } else {
            let should_report = now > 0
                && (self.last_progress == 0
                    || (now - self.last_progress >= self.min_progress
                        && self.last_progress_at.elapsed() >= self.interval));
            if should_report {
                self.last_progress = now;
                self.last_progress_at = Instant::now();
                let elapsed = self.start.elapsed();
                self.progress_fn.progress(now, total, elapsed);
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
            if let Err(error) = self.output.open(
                #[cfg(feature = "memmap")]
                self.use_mmap,
                cl.get(),
            ) {
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
                Ok(0)
            }
        }
    }

    fn header(&mut self, data: &[u8]) -> bool {
        self.parse_status_line(data);
        true
    }

    fn progress(&mut self, dltotal: f64, dlnow: f64, _ultotal: f64, _ulnow: f64) -> bool {
        if dltotal > 0.0 {
            self.progress
                .as_mut()
                .expect("curl called progress without a progress_fn")
                .progress(dlnow as u64, dltotal as u64);
        }

        true
    }

    fn debug(&mut self, kind: InfoType, data: &[u8]) {
        match kind {
            InfoType::Text => self
                .verbose
                .as_mut()
                .expect("curl called debug without a verbose_fn")
                .text(data),
            InfoType::HeaderIn => self
                .verbose
                .as_mut()
                .expect("curl called debug without a verbose_fn")
                .incoming_header(mem::take(&mut self.first_header_in), data),
            InfoType::HeaderOut => {
                self.first_header_in = true;
                self.verbose
                    .as_mut()
                    .expect("curl called debug without a verbose_fn")
                    .outgoing_header(data)
            }
            _ => {}
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
            #[cfg(feature = "memmap")]
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
            #[cfg(feature = "memmap")]
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

struct VerboseHandlerFromFn<F>(F);

impl<F> VerboseHandler for VerboseHandlerFromFn<F>
where
    F: FnMut(VerboseMessage, &[u8]) + 'static,
{
    fn text(&mut self, data: &[u8]) {
        self.0(VerboseMessage::Text, data)
    }

    fn outgoing_header(&mut self, data: &[u8]) {
        self.0(VerboseMessage::OutgoingHeader, data)
    }

    fn incoming_header(&mut self, first: bool, data: &[u8]) {
        self.0(
            if first {
                VerboseMessage::FirstIncomingHeader
            } else {
                VerboseMessage::IncomingHeader
            },
            data,
        )
    }
}

struct ProgressHandlerFromFn<F>(F);

impl<F> ProgressHandler for ProgressHandlerFromFn<F>
where
    F: FnMut(u64, u64, Duration) + 'static,
{
    fn progress(&mut self, now: u64, total: u64, elapsed: Duration) {
        self.0(now, total, elapsed)
    }

    fn done(&mut self, total: u64, elapsed: Duration) {
        self.0(total, total, elapsed)
    }
}

#[cfg(feature = "progress_bar")]
mod progress_bar {
    use indicatif::{ProgressBar, ProgressStyle};
    use std::time::Duration;

    const DEFAULT_TEMPLATE: &str = "({eta_precise}) [{wide_bar}] {percent:>3}% {bytes}/{total_bytes} @{binary_bytes_per_sec} [{elapsed_precise}]";
    const DEFAULT_PROGRESS: &str = "=> ";

    pub(super) struct DurlProgress {
        pb: Option<ProgressBar>,
    }

    impl DurlProgress {
        pub(super) fn new() -> Self {
            DurlProgress { pb: None }
        }
    }

    impl super::ProgressHandler for DurlProgress {
        fn progress(&mut self, now: u64, total: u64, _elapsed: Duration) {
            let pb = self.pb.get_or_insert_with(|| {
                let pb = ProgressBar::new(total);
                pb.set_style(
                    ProgressStyle::default_bar()
                        .template(DEFAULT_TEMPLATE)
                        .progress_chars(DEFAULT_PROGRESS),
                );
                pb
            });
            pb.set_position(now);
        }

        fn done(&mut self, _total: u64, _elapsed: Duration) {
            if let Some(pb) = self.pb.take() {
                pb.finish_with_message("done");
            }
        }
    }
}
