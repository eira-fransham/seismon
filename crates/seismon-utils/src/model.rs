// Copyright © 2018 Cormac O'Brien
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software
// and associated documentation files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
// BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

use bitflags::bitflags;
use num_derive::FromPrimitive;

#[derive(Debug, FromPrimitive)]
pub enum SyncType {
    Sync = 0,
    Rand = 1,
}

bitflags! {
    #[derive(Default, Copy, Clone, Ord, Debug, Eq, PartialOrd, PartialEq)]
    pub struct ModelFlags: u8 {
        const ROCKET  = 0b00000001;
        const GRENADE = 0b00000010;
        const GIB     = 0b00000100;
        const ROTATE  = 0b00001000;
        const TRACER  = 0b00010000;
        const ZOMGIB  = 0b00100000;
        const TRACER2 = 0b01000000;
        const TRACER3 = 0b10000000;
    }
}
