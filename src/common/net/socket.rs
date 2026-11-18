//! `QSocket`, a reliable/unreliable message channel over UDP.

use std::{
    collections::VecDeque,
    io::{BufReader, Cursor, Read, Write},
    net::{SocketAddr, UdpSocket},
    time::Duration,
};

use bevy::prelude::*;
use byteorder::{NetworkEndian, ReadBytesExt, WriteBytesExt};
use num::FromPrimitive;
use num_derive::FromPrimitive;

use super::*;

// the original engine treats these as bitflags, but all of them are mutually exclusive except for
// NETFLAG_DATA (reliable message) and NETFLAG_EOM (end of reliable message).
#[derive(Debug, Eq, FromPrimitive, PartialEq)]
pub enum MsgKind {
    Reliable = 0x0001,
    Ack = 0x0002,
    ReliableEom = 0x0009,
    Unreliable = 0x0010,
    Ctl = 0x8000,
}

#[derive(PartialEq)]
pub enum BlockingMode {
    Blocking,
    NonBlocking,
    Timeout(Duration),
}

// TODO: Should not clone
#[derive(Resource)]
pub struct QSocket {
    socket: UdpSocket,
    remote: SocketAddr,

    unreliable_send_sequence: u32,
    unreliable_recv_sequence: u32,

    ack_sequence: u32,

    send_sequence: u32,
    send_queue: VecDeque<Box<[u8]>>,
    send_cache: Box<[u8]>,
    send_next: bool,
    send_count: usize,
    resend_count: usize,

    recv_sequence: u32,
    recv_buf: [u8; MAX_MESSAGE],
}

impl QSocket {
    pub fn new(socket: UdpSocket, remote: SocketAddr) -> QSocket {
        QSocket {
            socket,
            remote,

            unreliable_send_sequence: 0,
            unreliable_recv_sequence: 0,

            ack_sequence: 0,

            send_sequence: 0,
            send_queue: VecDeque::new(),
            send_cache: Box::new([]),
            send_count: 0,
            send_next: false,
            resend_count: 0,

            recv_sequence: 0,
            recv_buf: [0; MAX_MESSAGE],
        }
    }

    pub fn can_send(&self) -> bool {
        self.send_queue.is_empty() && self.send_cache.is_empty()
    }

    /// Begin sending a reliable message over this socket.
    pub fn begin_send_msg(&mut self, msg: &[u8]) -> Result<(), NetError> {
        // make sure all reliable messages have been ACKed in their entirety
        if !self.send_queue.is_empty() {
            return Err(NetError::with_msg("begin_send_msg: previous message unacknowledged"));
        }

        // empty messages are an error
        if msg.is_empty() {
            return Err(NetError::with_msg("begin_send_msg: Input data has zero length"));
        }

        // check upper message length bound
        if msg.len() > MAX_MESSAGE {
            return Err(NetError::with_msg("begin_send_msg: Input data exceeds MAX_MESSAGE"));
        }

        // split the message into chunks and enqueue them
        for chunk in msg.chunks(MAX_DATAGRAM) {
            self.send_queue.push_back(chunk.to_owned().into_boxed_slice());
        }

        // send the first chunk
        self.send_msg_next()?;

        Ok(())
    }

    /// Resend the last reliable message packet.
    pub fn resend_msg(&mut self) -> Result<(), NetError> {
        if self.send_cache.is_empty() {
            Err(NetError::with_msg("Attempted resend with empty send cache"))
        } else {
            self.socket.send_to(&self.send_cache, self.remote)?;
            self.resend_count += 1;

            Ok(())
        }
    }

    /// Send the next segment of a reliable message.
    pub fn send_msg_next(&mut self) -> Result<(), NetError> {
        // grab the first chunk in the queue
        let content = self.send_queue.pop_front().expect("Send queue is empty (this is a bug)");

        // if this was the last chunk, set the EOM flag
        let msg_kind = match self.send_queue.is_empty() {
            true => MsgKind::ReliableEom,
            false => MsgKind::Reliable,
        };

        // compose the packet
        let mut compose = Vec::with_capacity(MAX_PACKET);
        compose.write_u16::<NetworkEndian>(msg_kind as u16)?;
        compose.write_u16::<NetworkEndian>((HEADER_SIZE + content.len()) as u16)?;
        compose.write_u32::<NetworkEndian>(self.send_sequence)?;
        compose.write_all(&content)?;

        // store packet to send cache
        self.send_cache = compose.into_boxed_slice();

        // increment send sequence
        self.send_sequence += 1;

        // send the composed packet
        self.socket.send_to(&self.send_cache, self.remote)?;

        // TODO: update send time
        // bump send count
        self.send_count += 1;

        // don't send the next chunk until this one gets ACKed
        self.send_next = false;

        Ok(())
    }

    pub fn send_msg_unreliable(&mut self, content: &[u8]) -> Result<(), NetError> {
        if content.is_empty() {
            return Err(NetError::with_msg("Unreliable message has zero length"));
        }

        if content.len() > MAX_DATAGRAM {
            return Err(NetError::with_msg("Unreliable message length exceeds MAX_DATAGRAM"));
        }

        let packet_len = HEADER_SIZE + content.len();

        // compose the packet
        let mut packet = Vec::with_capacity(MAX_PACKET);
        packet.write_u16::<NetworkEndian>(MsgKind::Unreliable as u16)?;
        packet.write_u16::<NetworkEndian>(packet_len as u16)?;
        packet.write_u32::<NetworkEndian>(self.unreliable_send_sequence)?;
        packet.write_all(content)?;

        // increment unreliable send sequence
        self.unreliable_send_sequence += 1;

        // send the message
        self.socket.send_to(&packet, self.remote)?;

        // bump send count
        self.send_count += 1;

        Ok(())
    }

    /// Receive a message on this socket.
    // TODO: the flow control in this function is completely baffling, make it a little less awful
    pub fn recv_msg(&mut self, block: BlockingMode) -> Result<Vec<u8>, NetError> {
        let mut msg = Vec::new();

        match block {
            BlockingMode::Blocking => {
                self.socket.set_nonblocking(false)?;
                self.socket.set_read_timeout(None)?;
            }

            BlockingMode::NonBlocking => {
                self.socket.set_nonblocking(true)?;
                self.socket.set_read_timeout(None)?;
            }

            BlockingMode::Timeout(d) => {
                self.socket.set_nonblocking(false)?;
                self.socket.set_read_timeout(Some(d))?;
            }
        }

        loop {
            let (packet_len, src_addr) = match self.socket.recv_from(&mut self.recv_buf) {
                Ok(x) => x,
                Err(e) => {
                    use std::io::ErrorKind;
                    match e.kind() {
                        // these errors are expected in nonblocking mode
                        ErrorKind::WouldBlock | ErrorKind::TimedOut => return Ok(Vec::new()),
                        _ => return Err(NetError::from(e)),
                    }
                }
            };

            if src_addr != self.remote {
                // this packet didn't come from remote, drop it
                debug!("forged packet (src_addr was {}, should be {})", src_addr, self.remote);
                continue;
            }

            let mut reader = BufReader::new(Cursor::new(&self.recv_buf[..packet_len]));

            let msg_kind_code = reader.read_u16::<NetworkEndian>()?;
            let msg_kind = match MsgKind::from_u16(msg_kind_code) {
                Some(f) => f,
                None => {
                    return Err(NetError::invalid_data(format!(
                        "Invalid message kind: {msg_kind_code}"
                    )));
                }
            };

            if packet_len < HEADER_SIZE {
                // TODO: increment short packet count
                debug!("short packet");
                continue;
            }

            let field_len = reader.read_u16::<NetworkEndian>()?;
            if field_len as usize != packet_len {
                return Err(NetError::invalid_data(format!(
                    "Length field and actual length differ ({field_len} != {packet_len})"
                )));
            }

            let sequence =
                if msg_kind != MsgKind::Ctl { reader.read_u32::<NetworkEndian>()? } else { 0 };

            match msg_kind {
                // ignore control messages
                MsgKind::Ctl => {}

                MsgKind::Unreliable => {
                    // we've received a newer datagram, ignore
                    if sequence < self.unreliable_recv_sequence {
                        println!("Stale datagram with sequence # {sequence}");
                        break;
                    }

                    // we've skipped some datagrams, count them as dropped
                    if sequence > self.unreliable_recv_sequence {
                        let drop_count = sequence - self.unreliable_recv_sequence;
                        println!(
                            "Dropped {} packet(s) ({} -> {})",
                            drop_count, sequence, self.unreliable_recv_sequence
                        );
                    }

                    self.unreliable_recv_sequence = sequence + 1;

                    // copy the rest of the packet into the message buffer and return
                    reader.read_to_end(&mut msg)?;
                    return Ok(msg);
                }

                MsgKind::Ack => {
                    if sequence != self.send_sequence - 1 {
                        println!("Stale ACK received");
                    } else if sequence != self.ack_sequence {
                        println!("Duplicate ACK received");
                    } else {
                        self.ack_sequence += 1;
                        if self.ack_sequence != self.send_sequence {
                            return Err(NetError::with_msg("ACK sequencing error"));
                        }

                        // our last reliable message has been acked
                        if self.send_queue.is_empty() {
                            // the whole message is through, clear the send cache
                            self.send_cache = Box::new([]);
                        } else {
                            // send the next chunk before returning
                            self.send_next = true;
                        }
                    }
                }

                // TODO: once we start reading a reliable message, don't allow other packets until
                // we have the whole thing
                MsgKind::Reliable | MsgKind::ReliableEom => {
                    // send ack message and increment self.recv_sequence
                    let mut ack_buf: [u8; HEADER_SIZE] = [0; HEADER_SIZE];
                    let mut ack_curs = Cursor::new(&mut ack_buf[..]);
                    ack_curs.write_u16::<NetworkEndian>(MsgKind::Ack as u16)?;
                    ack_curs.write_u16::<NetworkEndian>(HEADER_SIZE as u16)?;
                    ack_curs.write_u32::<NetworkEndian>(sequence)?;
                    self.socket.send_to(ack_curs.into_inner(), self.remote)?;

                    // if this was a duplicate, drop it
                    if sequence != self.recv_sequence {
                        println!("Duplicate message received");
                        continue;
                    }

                    self.recv_sequence += 1;
                    reader.read_to_end(&mut msg)?;

                    // if this is the last chunk of a reliable message, break out and return
                    if msg_kind == MsgKind::ReliableEom {
                        break;
                    }
                }
            }
        }

        if self.send_next {
            self.send_msg_next()?;
        }

        Ok(msg)
    }
}

#[cfg(test)]
mod tests {
    use seismon_utils::QString;

    use super::*;

    fn gen_qsocket_pair() -> (QSocket, QSocket) {
        let src_udp = UdpSocket::bind("localhost:0").unwrap();
        let src_addr = src_udp.local_addr().unwrap();

        let dst_udp = UdpSocket::bind("localhost:0").unwrap();
        let dst_addr = dst_udp.local_addr().unwrap();

        (QSocket::new(src_udp, dst_addr), QSocket::new(dst_udp, src_addr))
    }

    #[test]
    fn test_qsocket_send_msg_short() {
        let (mut src, mut dst) = gen_qsocket_pair();

        let message = QString::from("test message").raw;
        src.begin_send_msg(&message).unwrap();
        let received = dst.recv_msg(BlockingMode::Blocking).unwrap();
        assert_eq!(message, received);

        // TODO: assert can_send == true, send_next == false, etc
    }

    #[test]
    fn test_qsocket_send_msg_unreliable_recv_msg_eq() {
        let (mut src, mut dst) = gen_qsocket_pair();

        let message = QString::from("test message").raw;
        src.send_msg_unreliable(&message).unwrap();
        let received = dst.recv_msg(BlockingMode::Blocking).unwrap();
        assert_eq!(message, received);
    }

    #[test]
    #[should_panic]
    fn test_qsocket_send_msg_unreliable_zero_length_fails() {
        let (mut src, _) = gen_qsocket_pair();

        let message = [];
        src.send_msg_unreliable(&message).unwrap();
    }

    #[test]
    #[should_panic]
    fn test_qsocket_send_msg_unreliable_exceeds_max_length_fails() {
        let (mut src, _) = gen_qsocket_pair();

        let message = [0; MAX_DATAGRAM + 1];
        src.send_msg_unreliable(&message).unwrap();
    }
}
