// Copyright © 2018 Cormac O'Brien
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

pub mod connect;

mod client_cmd;
mod codec;
mod consts;
mod entity;
mod error;
mod flags;
mod server_cmd;
mod socket;
mod temp_entity;

pub use self::{
    client_cmd::*, codec::*, consts::*, entity::*, error::*, flags::*, server_cmd::*, socket::*,
    temp_entity::*,
};

use std::sync::Arc;

use bevy::prelude::*;

#[derive(Clone, Message, Debug)]
pub struct ServerMessage {
    pub client_id: usize,
    pub packet: Arc<[u8]>,
}

#[derive(PartialEq, Eq, Copy, Clone, Hash, Default, Debug)]
pub enum MessageKind {
    #[default]
    Reliable,
    Unreliable,
}

#[derive(Message, Default, Clone, Debug)]
pub struct ClientMessage {
    pub client_id: usize,
    pub packet: Arc<[u8]>,
    pub kind: MessageKind,
}
