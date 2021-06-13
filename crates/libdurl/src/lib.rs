use console::Color;
use curl::{
    easy::{Auth, Easy2, Handler, InfoType},
    multi::{Easy2Handle, Events, Multi, Socket, SocketEvents},
};
use curl_sys::CURL;
use libc::c_double;
use libjdkman::{eprintln_color, eprintln_green, eprintln_red};
use memmap::MmapMut;
use nohash_hasher::{IntMap, IntSet};
use polling::{Event, Poller};
use slab::Slab;
use std::{
    collections::hash_map::Entry,
    fmt::Debug,
    fs::{File, OpenOptions},
    io::{self, Write},
    marker::PhantomData,
    mem,
    num::NonZeroUsize,
    path::PathBuf,
    sync::{
        atomic::{AtomicI64, Ordering},
        mpsc::{self, Receiver},
        Arc,
    },
    time::{Duration, Instant},
};

type Result<A, E = Box<dyn std::error::Error>> = std::result::Result<A, E>;

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
struct ResponseError(Option<curl::Error>);

struct CurlHandler {
    first_header_in: bool,
    read_content_length: bool,
    is_redirect: bool,
    start: Instant,
    next_progress_at: f64,
    last_logged: Option<Instant>,
    content_length: Option<NonZeroUsize>,
    output_file: File,
    output_path: PathBuf,
    use_mmap: bool,
    output: Option<Output>,
    curl_handle: *mut CURL,
}

struct Output {
    map: MmapMut,
    offset: usize,
    len: usize,
}

impl Output {
    fn new(file: &File, len: usize) -> io::Result<Self> {
        let map = unsafe { MmapMut::map_mut(file)? };
        Ok(Self {
            map,
            offset: 0,
            len,
        })
    }

    fn write(self: &mut Self, mut data: &[u8]) -> io::Result<usize> {
        let start = self.offset;
        let mut end = start + data.len();

        if end > self.len {
            end = self.len;
            data = &data[..(end - start)];
        }

        (&mut self.map[start..end]).copy_from_slice(data);
        // self.map.flush_async_range(start, data.len())?;
        self.offset = end;
        Ok(data.len())
    }
}

struct OutputFile {
    path: PathBuf,
    overwrite: bool,
    use_mmap: bool,
}

impl OutputFile {
    fn open_file(&self) -> io::Result<File> {
        let mut options = OpenOptions::new();
        options.read(true).write(true);

        if self.overwrite {
            options.create(true);
        } else {
            options.create_new(true);
        }

        options.open(&self.path)
    }
}

impl CurlHandler {
    const PROGRESS_INTERVAL: f64 = (1 << 24) as f64;

    fn easy(output: OutputFile) -> io::Result<Easy2<Self>> {
        let file = output.open_file()?;
        let handler = Self::new(output.use_mmap, output.path, file);
        let mut handle = Easy2::new(handler);
        let curl_handle = handle.raw();
        handle.get_mut().set_handle(curl_handle);
        Ok(handle)
    }

    fn perform(mut handle: Easy2<Self>) -> DurlResult {
        handle.get_mut().start = Instant::now();
        handle.perform()?;
        Self::finish_response(&mut handle)
    }

    fn finish_response(handle: &mut Easy2<Self>) -> DurlResult {
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

    fn new(use_mmap: bool, output_path: PathBuf, output_file: File) -> Self {
        Self {
            first_header_in: true,
            read_content_length: false,
            is_redirect: false,
            start: Instant::now(),
            next_progress_at: Self::PROGRESS_INTERVAL,
            last_logged: None,
            content_length: None,
            output_file,
            output_path,
            use_mmap,
            output: None,
            curl_handle: std::ptr::null_mut(),
        }
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
}

impl Handler for CurlHandler {
    fn write(&mut self, data: &[u8]) -> Result<usize, curl::easy::WriteError> {
        if self.is_redirect {
            return Ok(0);
        }
        if data.is_empty() {
            return Ok(0);
        }
        // read content length at first data packet and open file for writing
        if let Some(cl) = self.try_find_content_length() {
            if let Err(err) = self.output_file.set_len(cl.get() as u64) {
                eprintln_red!(
                    "could not set the file size on [{}] to {} bytes: {}",
                    self.output_path.display(),
                    cl.get(),
                    err
                );
                // signal error to curl
                return Ok(0);
            }
            if self.use_mmap {
                let output = match Output::new(&self.output_file, cl.get()) {
                    Ok(output) => output,
                    Err(err) => {
                        eprintln_red!(
                            "could not memory-map [{}] for writing {} bytes: {}",
                            self.output_path.display(),
                            cl.get(),
                            err
                        );
                        // signal error to curl
                        return Ok(0);
                    }
                };
                self.output = Some(output);
            }
        }

        match &mut self.output {
            Some(output) => match output.write(data) {
                Ok(size) => Ok(size),
                Err(err) => {
                    eprintln_red!(
                        "could write to [{}] at offset={}, len={}, num_bytes={}: {}",
                        self.output_path.display(),
                        output.offset,
                        output.len,
                        data.len(),
                        err
                    );
                    Ok(0)
                }
            },
            None => match self.output_file.write_all(data) {
                Ok(_) => Ok(data.len()),
                Err(err) => {
                    eprintln_red!(
                        "could write to [{}], num_bytes={}: {}",
                        self.output_path.display(),
                        data.len(),
                        err
                    );
                    Ok(0)
                }
            },
        }
    }

    fn header(&mut self, data: &[u8]) -> bool {
        self.parse_status_line(data);
        true
    }

    fn progress(&mut self, dltotal: f64, dlnow: f64, _ultotal: f64, _ulnow: f64) -> bool {
        if dltotal > 0.0 {
            if dltotal == dlnow {
                let elapsed = self.start.elapsed();
                eprintln_color!(
                    console::Color::Blue,
                    "[{:?}] done: download {}",
                    elapsed,
                    ubyte::ByteUnit::Byte(dltotal as u64)
                );
            } else if dlnow > self.next_progress_at {
                self.next_progress_at += Self::PROGRESS_INTERVAL;
                if self
                    .last_logged
                    .map_or(true, |ll| ll.elapsed() > Duration::from_millis(500))
                {
                    self.last_logged = Some(Instant::now());
                    let elapsed = self.start.elapsed();
                    let msg = if dltotal > 0.0 {
                        format!(
                            "[{:?}] download: {:5.2}% {}/{}",
                            elapsed,
                            dlnow / dltotal * 100.0,
                            ubyte::ByteUnit::Byte(dlnow as u64),
                            ubyte::ByteUnit::Byte(dltotal as u64),
                        )
                    } else {
                        format!("[{:?}] progress: download: {}", elapsed, dlnow)
                    };
                    println!("{}", console::style(msg).blue());
                }
            }
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

pub struct DurlRequest(Easy2<CurlHandler>);

pub enum Uninitialized {}
pub enum UrlStage {}
pub enum OutputStage {}

pub struct DurlRequestBuilder<'url, Stage> {
    url: Option<&'url str>,
    output: Option<PathBuf>,
    overwrite_target: bool,
    use_mmap: bool,
    verbose: bool,
    progress: bool,
    _stage: PhantomData<Stage>,
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

    pub fn url<'url>(self, url: &'url str) -> DurlRequestBuilder<'url, UrlStage> {
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

impl<'url> DurlRequestBuilder<'url, UrlStage> {
    pub fn output(self, output: impl Into<PathBuf>) -> DurlRequestBuilder<'url, OutputStage> {
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

impl DurlRequestBuilder<'_, OutputStage> {
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

    pub fn build(&mut self) -> Result<DurlRequest> {
        let url = self.url.take().unwrap();
        let output = self.output.take().unwrap();

        // output file
        let output = OutputFile {
            path: output,
            overwrite: self.overwrite_target,
            use_mmap: self.use_mmap,
        };

        // new curl easy that connects with the handler
        let mut handle = CurlHandler::easy(output)?;

        // debug info
        let curl_version = curl::Version::get();

        // user-agent is based on the linked libcurl
        handle.useragent(&format!("libcurl/{}", curl_version.version()))?;

        // connect to this url
        handle.url(url)?;

        // force GET request (not really required)
        handle.get(true)?;

        // enable verbose logging
        if self.verbose {
            handle.verbose(true)?;
        }

        // enable progress logging
        if self.progress {
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

        Ok(DurlRequest(handle))
    }
}

impl DurlRequest {
    pub fn perform(self) -> DurlResult {
        Ok(CurlHandler::perform(self.0)?)
    }
}

#[derive(Debug, Clone, Copy)]
struct SocketInterest {
    socket: Socket,
    read: bool,
    write: bool,
    remove: bool,
    new: bool,
    update: bool,
    dead: bool,
}

impl SocketInterest {
    fn socket(self) -> Socket {
        self.socket
    }

    fn as_new(self) -> Self {
        Self { new: true, ..self }
    }

    fn as_known(self) -> Self {
        Self { new: false, ..self }
    }

    fn as_dead(self) -> Self {
        Self { dead: true, ..self }
    }

    fn to_fired(&mut self) {
        *self = Self {
            update: true,
            ..*self
        }
    }

    /// not commutative, expected result of applying two
    fn merge(&mut self, rhs: Self) {
        *self = Self::merged(*self, rhs);
    }

    /// not commutative, expected result of applying two
    fn merged(lhs: Self, rhs: Self) -> Self {
        debug_assert!(
            lhs.socket() == rhs.socket(),
            "Cannot merge for different sockets, got left={} and right={}",
            lhs.socket(),
            rhs.socket()
        );
        if lhs.dead {
            return lhs;
        }
        if rhs.dead {
            return rhs;
        }
        // if lhs=remove rhs either re-enables it or we keep the remove from rhs
        // if rhs=remove it doesn't matter if lhs did register anything
        // if neither are remove, keep the latest
        Self {
            update: true,
            ..rhs
        }
    }
}

impl From<(Socket, SocketEvents)> for SocketInterest {
    fn from((socket, events): (Socket, SocketEvents)) -> Self {
        if events.remove() {
            SocketInterest {
                socket,
                read: false,
                write: false,
                remove: true,
                new: false,
                update: false,
                dead: false,
            }
        } else {
            SocketInterest {
                socket,
                read: events.input(),
                write: events.output(),
                remove: false,
                new: false,
                update: false,
                dead: false,
            }
        }
    }
}

impl From<SocketInterest> for Events {
    fn from(val: SocketInterest) -> Self {
        let mut events = Events::new();
        events.input(val.read);
        events.output(val.write);
        events
    }
}

impl From<polling::Event> for SocketInterest {
    fn from(val: polling::Event) -> Self {
        SocketInterest {
            socket: val.key as Socket,
            read: val.readable,
            write: val.writable,
            remove: false,
            new: false,
            update: false,
            // alive: false,
            dead: false,
        }
    }
}

impl From<SocketInterest> for polling::Event {
    fn from(val: SocketInterest) -> Self {
        Self {
            key: val.socket() as usize,
            readable: val.read,
            writable: val.write,
        }
    }
}

struct ActiveSockets {
    socket_updates: Receiver<SocketInterest>,
    sockets: IntMap<Socket, SocketInterest>,
    poller: Poller,
    events: Vec<Event>,
}

impl ActiveSockets {
    fn new(socket_updates: Receiver<SocketInterest>) -> Result<Self> {
        let poller = Poller::new()?;
        Ok(Self {
            socket_updates,
            sockets: Default::default(),
            poller,
            events: Default::default(),
        })
    }

    fn update(&mut self) {
        loop {
            match self.socket_updates.try_recv() {
                Ok(sock) => match self.sockets.entry(sock.socket()) {
                    Entry::Vacant(lhs) => {
                        lhs.insert(sock.as_new());
                    }
                    Entry::Occupied(mut lhs) => {
                        lhs.get_mut().merge(sock);
                    }
                },
                Err(mpsc::TryRecvError::Empty) => break,
                Err(_) => {
                    // disconnect (client dropped), register all sockets for removal
                    for socket in self.sockets.values_mut() {
                        *socket = socket.as_dead()
                    }
                    return;
                }
            };
        }
    }

    fn apply(&mut self) -> Result<()> {
        for socket in self.sockets.values_mut() {
            if socket.dead {
                continue;
            }
            if socket.remove {
                Self::remove(&self.poller, socket)?
            } else if socket.new {
                Self::ok_or_kill(Self::add_socket(&self.poller, *socket), socket)?;
            } else if socket.update {
                Self::ok_or_kill(Self::modify_socket(&self.poller, *socket), socket)?;
            }
        }

        Ok(())
    }

    fn ok_or_kill(result: io::Result<()>, socket: &mut SocketInterest) -> io::Result<()> {
        match result {
            Ok(_) => {
                *socket = socket.as_known();
                Ok(())
            }
            Err(e) if Self::is_bad_socket_error(&e) => {
                *socket = socket.as_dead();
                Ok(())
            }
            result => result,
        }
    }

    fn remove(poller: &Poller, socket: &mut SocketInterest) -> io::Result<()> {
        if let Err(e) = poller.delete(socket.socket()) {
            if !Self::is_bad_socket_error(&e) && e.kind() != io::ErrorKind::PermissionDenied {
                return Err(e);
            }
        }
        *socket = socket.as_dead();
        Ok(())
    }

    fn add_socket(poller: &Poller, socket: SocketInterest) -> io::Result<()> {
        if let Err(e) = poller.add(socket.socket(), socket.into()) {
            if let Err(_) = poller.modify(socket.socket(), socket.into()) {
                return Err(e);
            }
        }

        Ok(())
    }

    fn modify_socket(poller: &Poller, socket: SocketInterest) -> io::Result<()> {
        if let Err(e) = poller.modify(socket.socket(), socket.into()) {
            if let Err(_) = poller.add(socket.socket(), socket.into()) {
                return Err(e);
            }
        }

        Ok(())
    }

    #[must_use]
    fn poll(
        &mut self,
        timeout: Duration,
    ) -> io::Result<Option<impl Iterator<Item = SocketInterest> + Debug + '_>> {
        Self::inner_poll(&self.poller, &mut self.events, &mut self.sockets, timeout)
    }

    #[must_use]
    fn inner_poll<'a>(
        poller: &'a Poller,
        events: &'a mut Vec<Event>,
        sockets: &'a mut IntMap<Socket, SocketInterest>,
        timeout: Duration,
    ) -> io::Result<Option<impl Iterator<Item = SocketInterest> + Debug + 'a>> {
        match poller.wait(events, Some(timeout)) {
            Ok(0) => Ok(None),
            Err(e) if e.kind() == io::ErrorKind::Interrupted => Ok(None),
            Err(e) => Err(e),
            Ok(_) => Ok(Some(events.drain(..).map(move |e| {
                sockets.get_mut(&(e.key as Socket)).unwrap().to_fired();
                e.into()
            }))),
        }
    }

    // copied from isahc
    fn is_bad_socket_error(error: &io::Error) -> bool {
        // OS-specific error codes that aren't mapped to an `std::io::ErrorKind`.
        const EBADF: i32 = 9;
        const ERROR_INVALID_HANDLE: i32 = 6;
        const ERROR_NOT_FOUND: i32 = 1168;

        match error.kind() {
            // Common error codes std understands.
            io::ErrorKind::NotFound | io::ErrorKind::InvalidInput => true,

            // Check for OS-specific error codes.
            _ => match error.raw_os_error() {
                // kqueue likes to return EBADF, especially on removal, since it
                // automatically removes sockets when they are closed.
                Some(EBADF) if cfg!(unix) => true,

                // IOCP can return these in rare circumstances. Typically these just
                // indicate that the socket is no longer registered with the
                // completion port or was already closed.
                Some(ERROR_INVALID_HANDLE) | Some(ERROR_NOT_FOUND) if cfg!(windows) => true,

                _ => false,
            },
        }
    }
}

pub type DurlResult = Result<ResponseTimings, curl::Error>;
type DurlResultHandler = dyn FnOnce(DurlResult);

struct RequestHandle {
    handle: Easy2Handle<CurlHandler>,
    error: Option<curl::Error>,
    callback: Option<Box<DurlResultHandler>>,
}

pub struct DurlClient {
    multi: Multi,
    requests: Slab<RequestHandle>,
    finished: IntSet<usize>,
    timeout_micros: Arc<AtomicI64>,
    active_sockets: ActiveSockets,
}

impl DurlClient {
    pub fn new() -> Result<Self> {
        let mut multi = Multi::new();

        let (sockets_writer, sockets_reader) = mpsc::channel();
        multi.socket_function(move |socket, events, _key| {
            let socket = SocketInterest::from((socket, events));
            sockets_writer.send(socket).unwrap();
        })?;

        let timeout_micros = Arc::new(AtomicI64::new(-1));
        let timer = Arc::clone(&timeout_micros);
        multi.timer_function(move |timeout| {
            let timeout = match timeout {
                Some(timeout) => timeout.as_micros().min(i64::MAX as u128) as i64,
                None => -1,
            };
            timer.store(timeout, Ordering::Release);
            true
        })?;

        Ok(Self {
            multi,
            requests: Default::default(),
            finished: Default::default(),
            timeout_micros,
            active_sockets: ActiveSockets::new(sockets_reader)?,
        })
    }

    /// print version line
    pub fn print_version() {
        let curl_version = curl::Version::get();
        print!("Curl: {}, Protocols:", curl::Version::num());
        for proto in curl_version.protocols() {
            print!(" {}", proto);
        }
        println!();
    }

    pub fn fire_and_forget_request<F>(&mut self, req: DurlRequest) -> Result<()> {
        self.register_request(req, None)
    }

    pub fn add_request<F>(&mut self, req: DurlRequest, action: F) -> Result<()>
    where
        F: FnOnce(DurlResult) + 'static,
    {
        self.register_request(req, Some(Box::new(action)))
    }

    fn register_request(
        &mut self,
        req: DurlRequest,
        callback: Option<Box<DurlResultHandler>>,
    ) -> Result<()> {
        let entry = self.requests.vacant_entry();
        let id = entry.key();

        let mut handle = self.multi.add2(req.0)?;
        handle.set_token(id)?;

        let handle = RequestHandle {
            handle,
            callback,
            error: None,
        };

        entry.insert(handle);
        Ok(())
    }

    pub fn send_all(&mut self) -> Result<()> {
        loop {
            let alive = self.next_action()?;
            self.handle_completed()?;
            self.register_sockets()?;
            if alive == 0 {
                return Ok(());
            }
        }
    }

    fn next_action(&mut self) -> Result<u32> {
        const MAX_WAIT_TIME: Duration = Duration::from_secs(1);

        let timeout = self.timeout_micros.load(Ordering::Acquire);

        if timeout != 0 {
            let timeout = if timeout > 0 {
                Duration::from_micros(timeout as u64).min(MAX_WAIT_TIME)
            } else {
                MAX_WAIT_TIME
            };

            if let Some(sockets) = self.active_sockets.poll(timeout)? {
                let mut num_alive = 0;

                for socket in sockets {
                    let events = socket.into();
                    num_alive = self.multi.action(socket.socket(), &events)?;
                }

                return Ok(num_alive);
            };
        }

        Ok(self.multi.timeout()?)
    }

    fn register_sockets(&mut self) -> Result<()> {
        self.active_sockets.update();
        self.active_sockets.apply()?;
        Ok(())
    }

    fn handle_completed(&mut self) -> Result<()> {
        let requests = &mut self.requests;
        let finished = &mut self.finished;
        self.multi.messages(|message| {
            if let Ok(token) = message.token() {
                let req = match requests.get_mut(token) {
                    Some(req) => req,
                    None => {
                        eprintln!("unknown token {}", token);
                        return;
                    }
                };
                if let Some(done) = message.result_for2(&req.handle) {
                    req.error = done.err();
                    finished.insert(token);
                }
            }
        });

        for token in finished.drain() {
            let RequestHandle {
                handle,
                error,
                callback,
            } = requests.remove(token);

            let mut handle = self.multi.remove2(handle)?;
            if let Some(callback) = callback {
                let result = match error {
                    Some(err) => Err(err),
                    None => CurlHandler::finish_response(&mut handle),
                };
                callback(result);
            }
        }

        Ok(())
    }
}
