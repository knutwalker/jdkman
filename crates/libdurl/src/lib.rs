use curl::multi::{Easy2Handle, Multi};
use nohash_hasher::IntSet;
use request::DurlRequestHandler;
use slab::Slab;
use std::{
    sync::{
        atomic::{AtomicI64, Ordering},
        mpsc, Arc,
    },
    time::Duration,
};

mod request;
mod selector;

pub use request::{
    DurlRequest, DurlRequestBuilder, DurlResult, ProgressHandler, RequestError, ResponseTimings,
    VerboseHandler, VerboseMessage,
};

pub type Result<A, E = Box<dyn std::error::Error>> = std::result::Result<A, E>;
type DurlResultCallback = dyn FnOnce(DurlResult);

struct RequestHandle {
    handle: Easy2Handle<DurlRequestHandler>,
    error: Option<curl::Error>,
    callback: Option<Box<DurlResultCallback>>,
}

pub struct DurlClient {
    multi: Multi,
    requests: Slab<RequestHandle>,
    finished: IntSet<usize>,
    timeout_micros: Arc<AtomicI64>,
    active_sockets: selector::ActiveSockets,
}

impl DurlClient {
    pub fn new() -> Result<Self> {
        let mut multi = Multi::new();

        let (sockets_writer, sockets_reader) = mpsc::channel();
        multi.socket_function(move |socket, events, _key| {
            let socket = selector::SocketInterest::from((socket, events));
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
            active_sockets: selector::ActiveSockets::new(sockets_reader)?,
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
        callback: Option<Box<DurlResultCallback>>,
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
        self.active_sockets.update_registry()
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

            let handle = self.multi.remove2(handle)?;
            if let Some(callback) = callback {
                let result = match error {
                    Some(err) => Err(RequestError::CurlError(err)),
                    None => DurlRequestHandler::finish_response(handle),
                };
                callback(result);
            }
        }

        Ok(())
    }
}
