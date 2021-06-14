use crate::Result;
use curl::multi::{Events, Socket, SocketEvents};
use nohash_hasher::IntMap;
use polling::{Event, Poller};
use std::{
    collections::hash_map::Entry,
    fmt::Debug,
    io,
    sync::mpsc::{self, Receiver},
    time::Duration,
};

#[derive(Debug, Clone, Copy)]
pub(crate) struct SocketInterest {
    socket: Socket,
    read: bool,
    write: bool,
    remove: bool,
    new: bool,
    update: bool,
    dead: bool,
}

impl SocketInterest {
    pub(crate) fn socket(self) -> Socket {
        self.socket
    }

    fn to_new(self) -> Self {
        Self { new: true, ..self }
    }

    fn to_known(self) -> Self {
        Self { new: false, ..self }
    }

    fn to_dead(self) -> Self {
        Self { dead: true, ..self }
    }

    fn as_fired(&mut self) {
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

pub(crate) struct ActiveSockets {
    socket_updates: Receiver<SocketInterest>,
    sockets: IntMap<Socket, SocketInterest>,
    poller: Poller,
    events: Vec<Event>,
}

impl ActiveSockets {
    pub(crate) fn new(socket_updates: Receiver<SocketInterest>) -> Result<Self> {
        let poller = Poller::new()?;
        Ok(Self {
            socket_updates,
            sockets: Default::default(),
            poller,
            events: Default::default(),
        })
    }

    pub(crate) fn update_registry(&mut self) -> Result<()> {
        self.update();
        self.apply()
    }

    fn update(&mut self) {
        loop {
            match self.socket_updates.try_recv() {
                Ok(sock) => match self.sockets.entry(sock.socket()) {
                    Entry::Vacant(lhs) => {
                        lhs.insert(sock.to_new());
                    }
                    Entry::Occupied(mut lhs) => {
                        lhs.get_mut().merge(sock);
                    }
                },
                Err(mpsc::TryRecvError::Empty) => break,
                Err(_) => {
                    // disconnect (client dropped), register all sockets for removal
                    for socket in self.sockets.values_mut() {
                        *socket = socket.to_dead()
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
                *socket = socket.to_known();
                Ok(())
            }
            Err(e) if Self::is_bad_socket_error(&e) => {
                *socket = socket.to_dead();
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
        *socket = socket.to_dead();
        Ok(())
    }

    fn add_socket(poller: &Poller, socket: SocketInterest) -> io::Result<()> {
        if let Err(e) = poller.add(socket.socket(), socket.into()) {
            if poller.modify(socket.socket(), socket.into()).is_err() {
                return Err(e);
            }
        }

        Ok(())
    }

    fn modify_socket(poller: &Poller, socket: SocketInterest) -> io::Result<()> {
        if let Err(e) = poller.modify(socket.socket(), socket.into()) {
            if poller.add(socket.socket(), socket.into()).is_err() {
                return Err(e);
            }
        }

        Ok(())
    }

    pub(crate) fn poll(
        &mut self,
        timeout: Duration,
    ) -> io::Result<Option<impl Iterator<Item = SocketInterest> + Debug + '_>> {
        Self::inner_poll(&self.poller, &mut self.events, &mut self.sockets, timeout)
    }

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
                sockets.get_mut(&(e.key as Socket)).unwrap().as_fired();
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
