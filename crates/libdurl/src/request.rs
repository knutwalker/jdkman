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

pub type DurlResult = Result<DurlResponse, RequestError>;

pub struct DurlRequest(pub(crate) Easy2<DurlRequestHandler>);

#[derive(Debug)]
pub struct DurlResponse {
    pub target: Target,
    pub timings: ResponseTimings,
}

pub enum Uninitialized {}
pub enum OutputStage {}
pub enum BuildStage {}

pub struct DurlRequestBuilder<'url, Stage> {
    speed_limit: Option<NonZeroU64>,

    progress_interval: Duration,
    progress_min_download: u64,
    progress_fn: Option<Box<dyn ProgressHandler>>,

    verbose_fn: Option<Box<dyn VerboseHandler>>,

    url: Option<&'url str>,
    target: Option<Target>,

    _stage: PhantomData<Stage>,
}

impl<S> DurlRequestBuilder<'_, S> {
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

    pub fn verbose_fn_if(
        &mut self,
        verbose_flag: bool,
        verbose: impl FnMut(VerboseMessage, &[u8]) + 'static,
    ) -> &mut Self {
        self.verbose_fn = verbose_flag
            .then(move || -> Box<dyn VerboseHandler> { Box::new(VerboseHandlerFromFn(verbose)) });
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
            speed_limit: None,
            progress_interval: Duration::from_secs(1),
            progress_min_download: 1 << 20,
            progress_fn: None,
            verbose_fn: None,
            url: None,
            target: None,
            _stage: PhantomData,
        }
    }

    pub fn url<'url>(&mut self, url: &'url str) -> DurlRequestBuilder<'url, OutputStage> {
        DurlRequestBuilder {
            speed_limit: self.speed_limit,
            progress_interval: self.progress_interval,
            progress_min_download: self.progress_min_download,
            progress_fn: self.progress_fn.take(),
            verbose_fn: self.verbose_fn.take(),
            url: Some(url),
            target: None,
            _stage: PhantomData,
        }
    }
}

impl<'url> DurlRequestBuilder<'url, OutputStage> {
    pub fn write_to_file(
        &mut self,
        overwrite_if_exists: bool,
        output: impl Into<PathBuf>,
    ) -> DurlRequestBuilder<'url, BuildStage> {
        self.target(Target::file(output, overwrite_if_exists))
    }

    #[cfg(feature = "memmap")]
    pub fn write_to_memory_mapped_file(
        &mut self,
        overwrite_if_exists: bool,
        output: impl Into<PathBuf>,
    ) -> DurlRequestBuilder<'url, BuildStage> {
        self.target(Target::memory_mapped_file(output, overwrite_if_exists))
    }

    pub fn write_to_stdout(&mut self) -> DurlRequestBuilder<'url, BuildStage> {
        self.target(Target::stdout())
    }

    pub fn write_to_sterr(&mut self) -> DurlRequestBuilder<'url, BuildStage> {
        self.target(Target::stderr())
    }

    pub fn return_as_bytes(&mut self) -> DurlRequestBuilder<'url, BuildStage> {
        self.append_to_bytes(Vec::new())
    }

    pub fn return_as_bytes_with_capacity(
        &mut self,
        capacity: usize,
    ) -> DurlRequestBuilder<'url, BuildStage> {
        self.append_to_bytes(Vec::with_capacity(capacity))
    }

    pub fn append_to_bytes(&mut self, bytes: Vec<u8>) -> DurlRequestBuilder<'url, BuildStage> {
        self.target(Target::ToBytes(bytes))
    }

    pub fn return_as_string(&mut self) -> DurlRequestBuilder<'url, BuildStage> {
        self.append_to_string(String::new())
    }

    pub fn return_as_string_with_capacity(
        &mut self,
        capacity: usize,
    ) -> DurlRequestBuilder<'url, BuildStage> {
        self.append_to_string(String::with_capacity(capacity))
    }

    pub fn append_to_string(&mut self, string: String) -> DurlRequestBuilder<'url, BuildStage> {
        self.target(Target::ToString(string))
    }

    pub fn write_to_writer(
        &mut self,
        writer: impl Write + Send + Sync + 'static,
    ) -> DurlRequestBuilder<'url, BuildStage> {
        self.target(Target::ToWriter(Box::new(writer)))
    }

    pub fn write_to_boxed_writer(
        &mut self,
        writer: impl Into<Box<dyn Write + Send + Sync>>,
    ) -> DurlRequestBuilder<'url, BuildStage> {
        self.target(Target::ToWriter(writer.into()))
    }

    pub fn target(&mut self, target: Target) -> DurlRequestBuilder<'url, BuildStage> {
        DurlRequestBuilder {
            speed_limit: self.speed_limit,
            progress_interval: self.progress_interval,
            progress_min_download: self.progress_min_download,
            progress_fn: self.progress_fn.take(),
            verbose_fn: self.verbose_fn.take(),
            url: self.url.take(),
            target: Some(target),
            _stage: PhantomData,
        }
    }
}

impl DurlRequestBuilder<'_, BuildStage> {
    pub fn build(&mut self) -> io::Result<DurlRequest> {
        DurlRequest::new(
            self.url.take().unwrap(),
            self.target.take().unwrap(),
            self.speed_limit,
            self.verbose_fn.take(),
            self.progress_fn
                .take()
                .map(|f| (self.progress_interval, self.progress_min_download, f)),
        )
    }
}

impl Default for DurlRequestBuilder<'static, Uninitialized> {
    fn default() -> Self {
        Self::new()
    }
}

pub enum Target {
    Drop,
    StdOut,
    StdErr,
    ToFile {
        path: PathBuf,
        overwrite_if_exists: bool,
        #[cfg(feature = "memmap")]
        use_mmap: bool,
    },
    ToBytes(Vec<u8>),
    ToString(String),
    ToWriter(Box<dyn Write + Send + Sync>),
}

impl Target {
    pub const fn stdout() -> Self {
        Self::StdOut
    }

    pub const fn stderr() -> Self {
        Self::StdErr
    }

    pub const fn bytes() -> Self {
        Self::ToBytes(Vec::new())
    }

    pub const fn string() -> Self {
        Self::ToString(String::new())
    }

    pub fn file(path: impl Into<PathBuf>, overwrite_if_exists: bool) -> Self {
        Self::ToFile {
            path: path.into(),
            overwrite_if_exists,
            #[cfg(feature = "memmap")]
            use_mmap: false,
        }
    }

    #[cfg(feature = "memmap")]
    pub fn memory_mapped_file(path: impl Into<PathBuf>, overwrite_if_exists: bool) -> Self {
        Self::ToFile {
            path: path.into(),
            overwrite_if_exists,
            use_mmap: true,
        }
    }

    pub fn writer(writer: impl Write + Send + Sync + 'static) -> Self {
        Self::ToWriter(Box::new(writer))
    }

    pub fn set_overwrite_if_exists(&mut self, overwrite_if_exists_value: bool) {
        if let Target::ToFile {
            overwrite_if_exists,
            ..
        } = self
        {
            *overwrite_if_exists = overwrite_if_exists_value;
        }
    }

    pub fn try_string(self) -> Result<String, Self> {
        match self {
            Target::ToString(s) => Ok(s),
            Target::ToBytes(bytes) => match String::from_utf8(bytes) {
                Ok(s) => Ok(s),
                Err(invalid) => Err(Target::ToBytes(invalid.into_bytes())),
            },
            otherwise => Err(otherwise),
        }
    }

    pub fn expect_string(self) -> String {
        self.try_string().unwrap_or_else(|otherwise| {
            panic!(
                "Unexpected target, required ToString but got {:?}",
                otherwise
            )
        })
    }

    pub fn try_bytes(self) -> Result<Vec<u8>, Self> {
        match self {
            Target::ToString(s) => Ok(s.into_bytes()),
            Target::ToBytes(bytes) => Ok(bytes),
            otherwise => Err(otherwise),
        }
    }

    pub fn expect_bytes(self) -> Vec<u8> {
        self.try_bytes().unwrap_or_else(|otherwise| {
            panic!(
                "Unexpected target, required ToBytes or ToString but got {:?}",
                otherwise
            )
        })
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
    content_length: Option<NonZeroUsize>,
    error: Option<InnerRequestError>,
    verbose: Option<Box<dyn VerboseHandler>>,
    progress: Option<ResponseProgress>,
    target: InnerTarget,
    curl_handle: *mut CURL,
}

#[derive(Debug, Default)]
pub struct ResponseTimings {
    pub namelookup: Duration,
    pub connect: Duration,
    pub appconnect: Duration,
    pub pretransfer: Duration,
    pub starttransfer: Duration,
    pub redirect: Duration,
    pub total: Duration,
}

#[derive(Debug)]
pub struct RequestError {
    pub error: InnerRequestError,
    pub target: Target,
}

#[derive(Debug)]
pub enum InnerRequestError {
    SetLengthFailed {
        length: usize,
        error: io::Error,
    },
    FileWriteFailed {
        length: usize,
        error: io::Error,
    },
    ResponseNotUtf8 {
        source: std::str::Utf8Error,
    },
    #[cfg(feature = "memmap")]
    MemoryMapFailed {
        length: usize,
        error: io::Error,
    },
    CurlError(curl::Error),
    MultiError(Vec<InnerRequestError>),
}

struct ResponseProgress {
    start: Instant,
    last_progress_at: Instant,
    last_progress: u64,
    interval: Duration,
    min_progress: u64,
    progress_fn: Box<dyn ProgressHandler>,
}

enum InnerTarget {
    Drop,
    StdOut,
    StdErr,
    ToFile {
        path: PathBuf,
        file: File,
        #[cfg(feature = "memmap")]
        use_mmap: bool,
    },
    ToBytes(Vec<u8>),
    ToString(Vec<u8>),
    ToWriter(Box<dyn Write + Send + Sync>),
    #[cfg(feature = "memmap")]
    ToMMap {
        path: PathBuf,
        map: MappedFile,
    },
}

#[cfg(feature = "memmap")]
pub struct MappedFile {
    map: MmapMut,
    offset: usize,
    len: usize,
}

impl DurlRequest {
    pub fn new(
        url: &str,
        target: Target,
        speed_limit: Option<NonZeroU64>,
        verbose_fn: Option<Box<dyn VerboseHandler>>,
        progress_fn: Option<(Duration, u64, Box<dyn ProgressHandler>)>,
    ) -> io::Result<Self> {
        let handle = DurlRequestHandler::new(url, target, speed_limit, verbose_fn, progress_fn)?;
        Ok(DurlRequest(handle))
    }

    pub fn perform(self) -> DurlResult {
        DurlRequestHandler::perform(self.0)
    }
}

impl DurlRequestHandler {
    pub(crate) fn new(
        url: &str,
        target: Target,
        speed_limit: Option<NonZeroU64>,
        verbose_fn: Option<Box<dyn VerboseHandler>>,
        progress_fn: Option<(Duration, u64, Box<dyn ProgressHandler>)>,
    ) -> io::Result<Easy2<Self>> {
        let target = target.prepare()?;
        let verbose = verbose_fn.is_some();
        let progress = progress_fn.is_some();

        let handler = DurlRequestHandler {
            first_header_in: true,
            read_content_length: false,
            is_redirect: false,
            content_length: None,
            error: None,
            verbose: verbose_fn,
            progress: progress_fn.map(ResponseProgress::new),
            target,
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
        match handle.perform() {
            Ok(()) => {}
            Err(err) => handle
                .get_mut()
                .set_error(Some(InnerRequestError::CurlError(err))),
        }
        Self::finish_response(handle)
    }

    pub(crate) fn finish_response(mut handle: Easy2<Self>) -> DurlResult {
        let target = mem::replace(&mut handle.get_mut().target, InnerTarget::Drop);
        let target = match target.into_target() {
            Ok(target) => target,
            Err(err) => {
                handle.get_mut().set_error(Some(err.error));
                err.target
            }
        };

        if let Some(error) = handle.get_mut().error.take() {
            return Err(RequestError { error, target });
        }

        let timings = ResponseTimings {
            namelookup: handle.namelookup_time().unwrap_or_default(),
            connect: handle.connect_time().unwrap_or_default(),
            appconnect: handle.appconnect_time().unwrap_or_default(),
            pretransfer: handle.pretransfer_time().unwrap_or_default(),
            starttransfer: handle.starttransfer_time().unwrap_or_default(),
            redirect: handle.redirect_time().unwrap_or_default(),
            total: handle.total_time().unwrap_or_default(),
        };

        Ok(DurlResponse { target, timings })
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

    pub(crate) fn set_error(&mut self, error: Option<InnerRequestError>) {
        if let Some(error) = error {
            let error = match self.error.take() {
                Some(previous) => match previous {
                    InnerRequestError::MultiError(mut errors) => {
                        errors.push(error);
                        InnerRequestError::MultiError(errors)
                    }
                    _ => InnerRequestError::MultiError(vec![previous, error]),
                },
                None => error,
            };
            self.error = Some(error);
        }
    }
}

impl Target {
    fn prepare(self) -> io::Result<InnerTarget> {
        let target = match self {
            Target::Drop => InnerTarget::Drop,
            Target::StdOut => InnerTarget::StdOut,
            Target::StdErr => InnerTarget::StdErr,
            Target::ToFile {
                path,
                overwrite_if_exists,
                #[cfg(feature = "memmap")]
                use_mmap,
            } => {
                let mut options = OpenOptions::new();
                options.read(true).write(true);

                if overwrite_if_exists {
                    options.create(true);
                } else {
                    options.create_new(true);
                };

                let file = options.open(&path)?;
                InnerTarget::ToFile {
                    path,
                    file,
                    #[cfg(feature = "memmap")]
                    use_mmap,
                }
            }
            Target::ToBytes(bytes) => InnerTarget::ToBytes(bytes),
            Target::ToString(s) => InnerTarget::ToString(s.into()),
            Target::ToWriter(w) => InnerTarget::ToWriter(w),
        };

        Ok(target)
    }
}

impl InnerTarget {
    fn open(&mut self, length: usize) -> Result<(), Option<InnerRequestError>> {
        match self {
            InnerTarget::ToFile {
                path: _path,
                file,
                #[cfg(feature = "memmap")]
                use_mmap,
            } => {
                if let Err(error) = file.set_len(length as u64) {
                    return Err(Some(InnerRequestError::SetLengthFailed { length, error }));
                }

                #[cfg(feature = "memmap")]
                if *use_mmap {
                    let map = match MappedFile::new(file, length) {
                        Ok(output) => output,
                        Err(error) => {
                            return Err(Some(InnerRequestError::MemoryMapFailed { length, error }));
                        }
                    };
                    *self = InnerTarget::ToMMap {
                        path: mem::take(_path),
                        map,
                    };
                }
            }
            InnerTarget::ToBytes(b) | InnerTarget::ToString(b) => {
                b.reserve_exact(length);
            }
            _ => {}
        };

        Ok(())
    }

    fn write(&mut self, data: &[u8]) -> Result<usize, Option<InnerRequestError>> {
        match self {
            InnerTarget::Drop => Ok(data.len()),
            InnerTarget::StdOut => Self::write_to_write(data, std::io::stdout().lock()),
            InnerTarget::StdErr => Self::write_to_write(data, std::io::stderr().lock()),
            InnerTarget::ToFile { file, .. } => Self::write_to_write(data, file),
            #[cfg(feature = "memmap")]
            InnerTarget::ToMMap { map, .. } => Ok(map.write(data)),
            InnerTarget::ToBytes(b) | InnerTarget::ToString(b) => {
                b.extend_from_slice(data);
                Ok(data.len())
            }
            InnerTarget::ToWriter(w) => Self::write_to_write(data, w),
        }
    }

    fn write_to_write(
        data: &[u8],
        mut target: impl Write,
    ) -> Result<usize, Option<InnerRequestError>> {
        match target.write_all(data) {
            Ok(_) => Ok(data.len()),
            Err(error) => Err(Some(InnerRequestError::FileWriteFailed {
                length: data.len(),
                error,
            })),
        }
    }

    fn into_target(self) -> Result<Target, RequestError> {
        let target = match self {
            InnerTarget::Drop => Target::Drop,
            InnerTarget::StdOut => Target::StdOut,
            InnerTarget::StdErr => Target::StdErr,
            InnerTarget::ToFile {
                path,
                file: _,
                #[cfg(feature = "memmap")]
                use_mmap,
            } => Target::ToFile {
                path,
                overwrite_if_exists: false,
                #[cfg(feature = "memmap")]
                use_mmap,
            },
            InnerTarget::ToBytes(b) => Target::ToBytes(b),
            InnerTarget::ToString(b) => match String::from_utf8(b) {
                Ok(s) => Target::ToString(s),
                Err(error) => {
                    let source = error.utf8_error();
                    return Err(RequestError {
                        error: InnerRequestError::ResponseNotUtf8 { source },
                        target: Target::ToBytes(error.into_bytes()),
                    });
                }
            },
            InnerTarget::ToWriter(w) => Target::ToWriter(w),
            #[cfg(feature = "memmap")]
            InnerTarget::ToMMap { path, .. } => Target::ToFile {
                path,
                overwrite_if_exists: false,
                use_mmap: true,
            },
        };
        Ok(target)
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
            if let Err(error) = self.target.open(cl.get()) {
                self.set_error(error);
                // signal error to curl
                return Ok(0);
            }
        }

        match self.target.write(data) {
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
        let (plain, file_path);
        let path: &dyn Display = match &self.target {
            Target::Drop => {
                plain = "drop";
                &plain
            }
            Target::StdOut => {
                plain = "stdout";
                &plain
            }
            Target::StdErr => {
                plain = "stderr";
                &plain
            }
            Target::ToFile { path, .. } => {
                file_path = path.display();
                &file_path
            }
            Target::ToBytes(_) => {
                plain = "bytes";
                &plain
            }
            Target::ToString(_) => {
                plain = "string";
                &plain
            }
            Target::ToWriter(_) => {
                plain = "writer";
                &plain
            }
        };

        fn fmt_err(
            f: &mut std::fmt::Formatter<'_>,
            path: &dyn Display,
            err: &InnerRequestError,
        ) -> std::fmt::Result {
            match err {
                InnerRequestError::SetLengthFailed { length, error } => f.write_fmt(format_args!(
                    "could not set the file size on [{}] to {} bytes: {}",
                    path, length, error
                )),
                InnerRequestError::FileWriteFailed { length, error } => f.write_fmt(format_args!(
                    "could not write to [{}], num_bytes={}: {}",
                    path, length, error
                )),
                InnerRequestError::ResponseNotUtf8 { source } => f.write_fmt(format_args!(
                    "could not decode the response as UTF-8: {}",
                    source
                )),
                #[cfg(feature = "memmap")]
                InnerRequestError::MemoryMapFailed { length, error } => f.write_fmt(format_args!(
                    "could not memory-map [{}] for writing {} bytes: {}",
                    path, length, error
                )),
                InnerRequestError::CurlError(error) => {
                    f.write_fmt(format_args!("Underlying curl error: {}", error))
                }
                InnerRequestError::MultiError(errors) => match &errors[..] {
                    [] => f.write_str("Empty errors"),
                    [error] => fmt_err(f, path, error),
                    [init @ .., last] => {
                        f.write_str("Multiple errors: [")?;
                        for error in init {
                            fmt_err(f, path, error)?;
                            f.write_str(", ")?;
                        }
                        fmt_err(f, path, last)?;
                        f.write_str("]")
                    }
                },
            }
        }

        fmt_err(f, path, &self.error)
    }
}

impl std::error::Error for RequestError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        fn get_source(err: &InnerRequestError) -> Option<&(dyn std::error::Error + 'static)> {
            match err {
                InnerRequestError::SetLengthFailed { error, .. } => Some(error),
                InnerRequestError::FileWriteFailed { error, .. } => Some(error),
                InnerRequestError::ResponseNotUtf8 { source, .. } => Some(source),
                #[cfg(feature = "memmap")]
                InnerRequestError::MemoryMapFailed { error, .. } => Some(error),
                InnerRequestError::CurlError(error) => Some(error),
                InnerRequestError::MultiError(errors) => errors.iter().find_map(get_source),
            }
        }
        get_source(&self.error)
    }
}

impl From<curl::Error> for InnerRequestError {
    fn from(val: curl::Error) -> Self {
        InnerRequestError::CurlError(val)
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

impl std::fmt::Debug for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            &Target::Drop => f.debug_tuple("Drop").finish(),
            Target::StdOut => f.debug_tuple("StdOut").finish(),
            Target::StdErr => f.debug_tuple("StdErr").finish(),
            Target::ToFile {
                path,
                overwrite_if_exists,
                ..
            } => f
                .debug_struct("ToFile")
                .field("path", path)
                .field("overwrite_if_exists", overwrite_if_exists)
                .finish_non_exhaustive(),
            Target::ToBytes(_) => f.debug_tuple("ToBytes").field(&"...").finish(),
            Target::ToString(_) => f.debug_tuple("ToString").field(&"...").finish(),
            Target::ToWriter(_) => f.debug_tuple("ToWriter").field(&"...").finish(),
        }
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
