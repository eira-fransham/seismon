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
