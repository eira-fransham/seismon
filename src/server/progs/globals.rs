// Copyright © 2018 Cormac O'Brien.
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

use std::{error::Error, fmt, sync::Arc};

use crate::server::progs::{EntityId, FieldAddr, FunctionId, GlobalDef, Opcode, StringId, Type};

use bevy::math::{EulerRot, Mat3, Vec3, Vec3Swizzles as _};
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use num_derive::FromPrimitive;

use super::StringTable;

pub const GLOBAL_STATIC_START: usize = 28;
pub const GLOBAL_DYNAMIC_START: usize = 64;

pub const GLOBAL_STATIC_COUNT: usize = GLOBAL_DYNAMIC_START - GLOBAL_STATIC_START;

#[allow(dead_code)]
pub const GLOBAL_ADDR_NULL: usize = 0;
pub const GLOBAL_ADDR_RETURN: usize = 1;
pub const GLOBAL_ADDR_ARG_0: usize = 4;
pub const GLOBAL_ADDR_ARG_1: usize = 7;
pub const GLOBAL_ADDR_ARG_2: usize = 10;
pub const GLOBAL_ADDR_ARG_3: usize = 13;
#[allow(dead_code)]
pub const GLOBAL_ADDR_ARG_4: usize = 16;
#[allow(dead_code)]
pub const GLOBAL_ADDR_ARG_5: usize = 19;
#[allow(dead_code)]
pub const GLOBAL_ADDR_ARG_6: usize = 22;
#[allow(dead_code)]
pub const GLOBAL_ADDR_ARG_7: usize = 25;

macro_rules! log_op {
    ($this:expr; $dest:ident = $op:ident ( $arg0:expr )) => {
        log_op!($this; $dest = $op($arg0, ""))
    };
    ($this:expr; $dest:ident = $op:ident ( $arg0:expr, $arg1:expr )) => {{
        let dest = $dest;
        match $this.defs.binary_search_by(|def| (def.offset as i16).cmp(&dest)) {
            Ok(def) =>
                bevy::log::debug!("{} = {:<12} {:>5} {:>5}", &$this.defs[def].debug_name, Opcode::$op, $arg0, $arg1),
            Err(def_after) => {
                let def = def_after.saturating_sub(1);
                let def_ref = &$this.defs[def];
                bevy::log::debug!(
                    "{}[{}] = {:<12} {:>5} {:>5}",
                    &def_ref.debug_name,
                    dest - def_ref.offset as i16,
                    Opcode::$op,
                    $arg0,
                    $arg1,
                );
            }
        }
    }};
}

#[derive(Debug)]
pub enum GlobalsError {
    Io(::std::io::Error),
    Address(isize),
    Other(String),
}

impl GlobalsError {
    pub fn with_msg<S>(msg: S) -> Self
    where
        S: AsRef<str>,
    {
        GlobalsError::Other(msg.as_ref().to_owned())
    }
}

impl fmt::Display for GlobalsError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            GlobalsError::Io(ref err) => {
                write!(f, "I/O error: ")?;
                err.fmt(f)
            }
            GlobalsError::Address(val) => write!(f, "Invalid address ({val})"),
            GlobalsError::Other(ref msg) => write!(f, "{msg}"),
        }
    }
}

impl Error for GlobalsError {}

impl From<::std::io::Error> for GlobalsError {
    fn from(error: ::std::io::Error) -> Self {
        GlobalsError::Io(error)
    }
}

pub trait GlobalAddr {
    /// The type of value referenced by this address.
    type Value;

    /// Loads the value at this address.
    fn load(&self, globals: &Globals) -> Result<Self::Value, GlobalsError>;

    /// Stores a value at this address.
    fn store(&self, globals: &mut Globals, value: Self::Value) -> Result<(), GlobalsError>;
}

#[derive(Copy, Clone, FromPrimitive)]
pub enum GlobalAddrFloat {
    Time = 31,
    FrameTime = 32,
    ForceRetouch = 33,
    Deathmatch = 35,
    Coop = 36,
    TeamPlay = 37,
    ServerFlags = 38,
    TotalSecrets = 39,
    TotalMonsters = 40,
    FoundSecrets = 41,
    KilledMonsters = 42,
    Arg0 = 43,
    Arg1 = 44,
    Arg2 = 45,
    Arg3 = 46,
    Arg4 = 47,
    Arg5 = 48,
    Arg6 = 49,
    Arg7 = 50,
    Arg8 = 51,
    Arg9 = 52,
    Arg10 = 53,
    Arg11 = 54,
    Arg12 = 55,
    Arg13 = 56,
    Arg14 = 57,
    Arg15 = 58,
    VForwardX = 59,
    VForwardY = 60,
    VForwardZ = 61,
    VUpX = 62,
    VUpY = 63,
    VUpZ = 64,
    VRightX = 65,
    VRightY = 66,
    VRightZ = 67,
    TraceAllSolid = 68,
    TraceStartSolid = 69,
    TraceFraction = 70,
    TraceEndPosX = 71,
    TraceEndPosY = 72,
    TraceEndPosZ = 73,
    TracePlaneNormalX = 74,
    TracePlaneNormalY = 75,
    TracePlaneNormalZ = 76,
    TracePlaneDist = 77,
    TraceInOpen = 79,
    TraceInWater = 80,
}

impl GlobalAddr for GlobalAddrFloat {
    type Value = f32;

    #[inline]
    fn load(&self, globals: &Globals) -> Result<Self::Value, GlobalsError> {
        globals.get_float(*self as i16)
    }

    #[inline]
    fn store(&self, globals: &mut Globals, value: Self::Value) -> Result<(), GlobalsError> {
        globals.put_float(value, *self as i16)
    }
}

#[derive(Copy, Clone, FromPrimitive)]
pub enum GlobalAddrVector {
    VForward = 59,
    VUp = 62,
    VRight = 65,
    TraceEndPos = 71,
    TracePlaneNormal = 74,
}

impl GlobalAddr for GlobalAddrVector {
    type Value = Vec3;

    #[inline]
    fn load(&self, globals: &Globals) -> Result<Self::Value, GlobalsError> {
        globals.get_vector(*self as i16)
    }

    #[inline]
    fn store(&self, globals: &mut Globals, value: Self::Value) -> Result<(), GlobalsError> {
        globals.put_vector(value, *self as i16)
    }
}

#[derive(FromPrimitive)]
pub enum GlobalAddrString {
    MapName = 34,
}

#[derive(Copy, Clone, FromPrimitive)]
pub enum GlobalAddrEntity {
    Self_ = 28,
    Other = 29,
    World = 30,
    TraceEntity = 78,
    MsgEntity = 81,
}

impl GlobalAddr for GlobalAddrEntity {
    type Value = EntityId;

    #[inline]
    fn load(&self, globals: &Globals) -> Result<Self::Value, GlobalsError> {
        globals.get_entity_id(*self as i16)
    }

    #[inline]
    fn store(&self, globals: &mut Globals, value: Self::Value) -> Result<(), GlobalsError> {
        globals.put_entity_id(value, *self as i16)
    }
}

#[derive(FromPrimitive)]
pub enum GlobalAddrField {}

#[derive(FromPrimitive, Copy, Clone, PartialEq, Eq)]
pub enum GlobalAddrFunction {
    Main = 82,
    StartFrame = 83,
    PlayerPreThink = 84,
    PlayerPostThink = 85,
    ClientKill = 86,
    ClientConnect = 87,
    PutClientInServer = 88,
    ClientDisconnect = 89,
    SetNewArgs = 90,
    SetChangeArgs = 91,
}

impl GlobalAddr for GlobalAddrFunction {
    type Value = FunctionId;

    #[inline]
    fn load(&self, globals: &Globals) -> Result<Self::Value, GlobalsError> {
        globals.get_function_id(*self as i16)
    }

    #[inline]
    fn store(&self, globals: &mut Globals, value: Self::Value) -> Result<(), GlobalsError> {
        globals.put_function_id(value, *self as i16)
    }
}

#[derive(Debug, Clone)]
pub struct Globals {
    defs: Arc<[GlobalDef]>,
    addrs: Box<[[u8; 4]]>,
}

impl Globals {
    /// Constructs a new `Globals` object.
    pub fn new<D: Into<Arc<[GlobalDef]>>>(defs: D, addrs: Box<[[u8; 4]]>) -> Globals {
        Globals { defs: defs.into(), addrs }
    }

    /// Performs a type check at `addr` with type `type_`.
    ///
    /// The type check allows checking `QFloat` against `QVector` and vice-versa, since vectors have
    /// overlapping definitions with their x-components (e.g. a vector `origin` and its x-component
    /// `origin_X` will have the same address).
    pub fn type_check(&self, addr: usize, type_: Type) -> Result<(), GlobalsError> {
        match self.defs.binary_search_by(|def| (def.offset as usize).cmp(&addr)) {
            Ok(i) => {
                let d = &self.defs[i];
                if type_ == d.type_
                    || (type_ == Type::QFloat && d.type_ == Type::QVector)
                    || (type_ == Type::QVector && d.type_ == Type::QFloat)
                {
                    Ok(())
                } else {
                    Err(GlobalsError::with_msg(format!(
                        "Type check failed (expected {}, found {})",
                        type_, d.type_
                    )))
                }
            }
            Err(_) => Ok(()),
        }
    }

    /// Converts a virtual address to an index into `addrs`, checking that it is in bounds.
    fn inbounds(&self, addr: i16) -> Result<usize, GlobalsError> {
        usize::try_from(addr)
            .ok()
            .filter(|&a| a < self.addrs.len())
            .ok_or(GlobalsError::Address(addr as isize))
    }

    /// Returns a reference to the memory at the given address.
    pub fn get_addr(&self, addr: i16) -> Result<&[u8], GlobalsError> {
        Ok(&self.addrs[self.inbounds(addr)?])
    }

    /// Returns a mutable reference to the memory at the given address.
    pub fn get_addr_mut(&mut self, addr: i16) -> Result<&mut [u8], GlobalsError> {
        let index = self.inbounds(addr)?;
        Ok(&mut self.addrs[index])
    }

    /// Returns a copy of the memory at the given address.
    pub fn get_bytes(&self, addr: i16) -> Result<[u8; 4], GlobalsError> {
        Ok(self.addrs[self.inbounds(addr)?])
    }

    /// Writes the provided data to the memory at the given address.
    ///
    /// This can be used to circumvent the type checker in cases where an operation is not dependent
    /// of the type of the data.
    pub fn put_bytes(&mut self, val: [u8; 4], addr: i16) -> Result<(), GlobalsError> {
        let index = self.inbounds(addr)?;
        self.addrs[index] = val;
        Ok(())
    }

    /// Loads an `i32` from the given virtual address.
    pub fn get_int(&self, addr: i16) -> Result<i32, GlobalsError> {
        Ok(self.get_addr(addr)?.read_i32::<LittleEndian>()?)
    }

    /// Loads an `i32` from the given virtual address.
    pub fn put_int(&mut self, val: i32, addr: i16) -> Result<(), GlobalsError> {
        self.get_addr_mut(addr)?.write_i32::<LittleEndian>(val)?;
        Ok(())
    }

    /// Loads an `f32` from the given virtual address.
    pub fn get_float(&self, addr: i16) -> Result<f32, GlobalsError> {
        self.type_check(addr as usize, Type::QFloat)?;
        self.get_float_untyped(addr)
    }

    /// Loads an `f32` from the given virtual address.
    pub fn get_float_untyped(&self, addr: i16) -> Result<f32, GlobalsError> {
        Ok(self.get_addr(addr)?.read_f32::<LittleEndian>()?)
    }

    /// Stores an `f32` at the given virtual address.
    pub fn put_float(&mut self, val: f32, addr: i16) -> Result<(), GlobalsError> {
        self.type_check(addr as usize, Type::QFloat)?;
        self.get_addr_mut(addr)?.write_f32::<LittleEndian>(val)?;
        Ok(())
    }

    /// Loads an `[f32; 3]` from the given virtual address.
    pub fn get_vector(&self, addr: i16) -> Result<Vec3, GlobalsError> {
        self.type_check(addr as usize, Type::QVector)?;

        self.get_vector_untyped(addr)
    }

    /// Loads an `[f32; 3]` from the given virtual address.
    pub fn get_vector_untyped(&self, addr: i16) -> Result<Vec3, GlobalsError> {
        let mut v = [0.; 3];
        for (i, v) in v.iter_mut().enumerate() {
            *v = self.get_float_untyped(addr + i as i16)?;
        }

        Ok(v.into())
    }

    /// Stores an `[f32; 3]` at the given virtual address.
    pub fn put_vector(&mut self, val: Vec3, addr: i16) -> Result<(), GlobalsError> {
        self.type_check(addr as usize, Type::QVector)?;

        for i in 0..3 {
            self.put_float(val[i], addr + i as i16)?;
        }

        Ok(())
    }

    /// Loads a `StringId` from the given virtual address.
    pub fn string_id(&self, addr: i16) -> Result<StringId, GlobalsError> {
        self.type_check(addr as usize, Type::QString)?;

        Ok(StringId(self.get_addr(addr)?.read_i32::<LittleEndian>()? as usize))
    }

    /// Stores a `StringId` at the given virtual address.
    pub fn put_string_id(&mut self, val: StringId, addr: i16) -> Result<(), GlobalsError> {
        self.type_check(addr as usize, Type::QString)?;

        self.get_addr_mut(addr)?.write_i32::<LittleEndian>(val.try_into().unwrap())?;
        Ok(())
    }

    /// Loads an `EntityId` from the given virtual address.
    pub fn get_entity_id(&self, addr: i16) -> Result<EntityId, GlobalsError> {
        self.type_check(addr as usize, Type::QEntity)?;

        Ok(EntityId(self.get_addr(addr)?.read_i32::<LittleEndian>()?))
    }

    /// Stores an `EntityId` at the given virtual address.
    pub fn put_entity_id(&mut self, val: EntityId, addr: i16) -> Result<(), GlobalsError> {
        self.type_check(addr as usize, Type::QEntity)?;

        self.get_addr_mut(addr)?.write_i32::<LittleEndian>(val.0)?;
        Ok(())
    }

    /// Loads a `FieldAddr` from the given virtual address.
    pub fn get_field_addr(&self, addr: i16) -> Result<FieldAddr, GlobalsError> {
        self.type_check(addr as usize, Type::QField)?;

        match self.get_int(addr)? {
            f if f < 0 => Err(GlobalsError::with_msg(format!("Negative field address ({f})"))),
            f => Ok(FieldAddr(f as usize)),
        }
    }

    /// Stores a `FieldAddr` at the given virtual address.
    pub fn put_field_addr(&mut self, val: FieldAddr, addr: i16) -> Result<(), GlobalsError> {
        self.type_check(addr as usize, Type::QField)?;
        self.get_addr_mut(addr)?.write_i32::<LittleEndian>(val.0 as i32)?;
        Ok(())
    }

    /// Loads a `FunctionId` from the given virtual address.
    pub fn get_function_id(&self, addr: i16) -> Result<FunctionId, GlobalsError> {
        self.type_check(addr as usize, Type::QFunction)?;
        Ok(FunctionId(self.get_addr(addr)?.read_i32::<LittleEndian>()? as usize))
    }

    /// Stores a `FunctionId` at the given virtual address.
    pub fn put_function_id(&mut self, val: FunctionId, addr: i16) -> Result<(), GlobalsError> {
        self.type_check(addr as usize, Type::QFunction)?;
        self.get_addr_mut(addr)?.write_i32::<LittleEndian>(val.try_into().unwrap())?;
        Ok(())
    }

    // TODO: typecheck these with QPointer?

    pub fn get_entity_field(&self, addr: i16) -> Result<i32, GlobalsError> {
        self.get_int(addr)
    }

    pub fn put_entity_field(&mut self, val: i32, addr: i16) -> Result<(), GlobalsError> {
        self.put_int(val, addr)?;
        Ok(())
    }

    pub fn load<A: GlobalAddr>(&self, addr: A) -> Result<A::Value, GlobalsError> {
        addr.load(self)
    }

    pub fn store<A: GlobalAddr>(&mut self, addr: A, value: A::Value) -> Result<(), GlobalsError> {
        addr.store(self, value)
    }

    /// Copies the data at `src_addr` to `dst_addr` without type checking.
    pub fn untyped_copy(&mut self, src_addr: i16, dst_addr: i16) -> Result<(), GlobalsError> {
        let src = self.get_addr(src_addr)?.to_owned();
        let dst = self.get_addr_mut(dst_addr)?;

        dst[..4].copy_from_slice(&src[..4]);

        Ok(())
    }

    // QuakeC instructions =====================================================

    pub fn op_mul_f(&mut self, f1_id: i16, f2_id: i16, prod_id: i16) -> Result<(), GlobalsError> {
        let f1 = self.get_float(f1_id)?;
        let f2 = self.get_float(f2_id)?;

        log_op!(self; prod_id = MulF(f1, f2));

        self.put_float(f1 * f2, prod_id)?;

        Ok(())
    }

    // MUL_V: Vector dot-product
    pub fn op_mul_v(&mut self, v1_id: i16, v2_id: i16, dot_id: i16) -> Result<(), GlobalsError> {
        let v1 = self.get_vector(v1_id)?;
        let v2 = self.get_vector(v2_id)?;

        log_op!(self; dot_id = MulV(v1, v2));

        self.put_float(v1.dot(v2), dot_id)?;

        Ok(())
    }

    // MUL_FV: Component-wise multiplication of vector by scalar
    pub fn op_mul_fv(&mut self, f_id: i16, v_id: i16, prod_id: i16) -> Result<(), GlobalsError> {
        let f = self.get_float(f_id)?;
        let v = self.get_vector(v_id)?;

        log_op!(self; prod_id = MulFV(f, v));

        self.put_vector(f * v, prod_id)?;

        Ok(())
    }

    // MUL_VF: Component-wise multiplication of vector by scalar
    pub fn op_mul_vf(&mut self, v_id: i16, f_id: i16, prod_id: i16) -> Result<(), GlobalsError> {
        let v = self.get_vector(v_id)?;
        let f = self.get_float(f_id)?;

        log_op!(self; prod_id = MulVF(v, f));

        self.put_vector(f * v, prod_id)?;

        Ok(())
    }

    // DIV: Float division
    pub fn op_div(&mut self, f1_id: i16, f2_id: i16, quot_id: i16) -> Result<(), GlobalsError> {
        let f1 = self.get_float(f1_id)?;
        let f2 = self.get_float(f2_id)?;

        log_op!(self; quot_id = Div(f1, f2));

        self.put_float(f1 / f2, quot_id)?;

        Ok(())
    }

    // ADD_F: Float addition
    pub fn op_add_f(&mut self, f1_ofs: i16, f2_ofs: i16, sum_ofs: i16) -> Result<(), GlobalsError> {
        let f1 = self.get_float(f1_ofs)?;
        let f2 = self.get_float(f2_ofs)?;

        log_op!(self; sum_ofs = AddF(f1, f2));

        self.put_float(f1 + f2, sum_ofs)?;

        Ok(())
    }

    // ADD_V: Vector addition
    pub fn op_add_v(&mut self, v1_id: i16, v2_id: i16, sum_id: i16) -> Result<(), GlobalsError> {
        let v1 = self.get_vector(v1_id)?;
        let v2 = self.get_vector(v2_id)?;

        log_op!(self; sum_id = AddV(v1, v2));

        self.put_vector(v1 + v2, sum_id)?;

        Ok(())
    }

    // SUB_F: Float subtraction
    pub fn op_sub_f(&mut self, f1_id: i16, f2_id: i16, diff_id: i16) -> Result<(), GlobalsError> {
        let f1 = self.get_float(f1_id)?;
        let f2 = self.get_float(f2_id)?;

        log_op!(self; diff_id = SubF(f1, f2));

        self.put_float(f1 - f2, diff_id)?;

        Ok(())
    }

    // SUB_V: Vector subtraction
    pub fn op_sub_v(&mut self, v1_id: i16, v2_id: i16, diff_id: i16) -> Result<(), GlobalsError> {
        let v1 = self.get_vector(v1_id)?;
        let v2 = self.get_vector(v2_id)?;

        log_op!(self; diff_id = SubV(v1, v2));

        self.put_vector(v1 - v2, diff_id)?;

        Ok(())
    }

    // EQ_F: Test equality of two floats
    pub fn op_eq_f(&mut self, f1_id: i16, f2_id: i16, eq_id: i16) -> Result<(), GlobalsError> {
        let f1 = self.get_float(f1_id)?;
        let f2 = self.get_float(f2_id)?;

        log_op!(self; eq_id = EqF(f1, f2));

        self.put_float(
            match f1 == f2 {
                true => 1.0,
                false => 0.0,
            },
            eq_id,
        )?;

        Ok(())
    }

    // EQ_V: Test equality of two vectors
    pub fn op_eq_v(&mut self, v1_id: i16, v2_id: i16, eq_id: i16) -> Result<(), GlobalsError> {
        let v1 = self.get_vector(v1_id)?;
        let v2 = self.get_vector(v2_id)?;

        log_op!(self; eq_id = EqV(v1, v2));

        self.put_float(
            match v1 == v2 {
                true => 1.0,
                false => 0.0,
            },
            eq_id,
        )?;

        Ok(())
    }

    // EQ_S: Test equality of two strings
    pub fn op_eq_s(
        &mut self,
        string_table: &StringTable,
        s1_ofs: i16,
        s2_ofs: i16,
        eq_ofs: i16,
    ) -> Result<(), GlobalsError> {
        if s1_ofs < 0 || s2_ofs < 0 {
            return Err(GlobalsError::with_msg("eq_s: negative string offset"));
        }

        let s1 = self.string_id(s1_ofs)?;
        let s2 = self.string_id(s2_ofs)?;

        let s1 =
            string_table.get(s1).ok_or_else(|| GlobalsError::Other(format!("No string {s1}")))?;
        let s2 =
            string_table.get(s2).ok_or_else(|| GlobalsError::Other(format!("No string {s2}")))?;

        log_op!(self; eq_ofs = EqS(s1, s2));

        if s1_ofs == s2_ofs || s1 == s2 {
            self.put_float(1.0, eq_ofs)?;
        } else {
            self.put_float(0.0, eq_ofs)?;
        }

        Ok(())
    }

    // EQ_ENT: Test equality of two entities (by identity)
    pub fn op_eq_ent(&mut self, e1_ofs: i16, e2_ofs: i16, eq_ofs: i16) -> Result<(), GlobalsError> {
        let e1 = self.get_entity_id(e1_ofs)?;
        let e2 = self.get_entity_id(e2_ofs)?;

        log_op!(self; eq_ofs = EqEnt(e1, e2));

        self.put_float(
            match e1 == e2 {
                true => 1.0,
                false => 0.0,
            },
            eq_ofs,
        )?;

        Ok(())
    }

    // EQ_FNC: Test equality of two functions (by identity)
    pub fn op_eq_fnc(&mut self, f1_ofs: i16, f2_ofs: i16, eq_ofs: i16) -> Result<(), GlobalsError> {
        let f1 = self.get_function_id(f1_ofs)?;
        let f2 = self.get_function_id(f2_ofs)?;

        log_op!(self; eq_ofs = EqFnc(f1, f2));

        self.put_float(
            match f1 == f2 {
                true => 1.0,
                false => 0.0,
            },
            eq_ofs,
        )?;

        Ok(())
    }

    // NE_F: Test inequality of two floats
    pub fn op_ne_f(&mut self, f1_ofs: i16, f2_ofs: i16, ne_ofs: i16) -> Result<(), GlobalsError> {
        let f1 = self.get_float(f1_ofs)?;
        let f2 = self.get_float(f2_ofs)?;

        log_op!(self; ne_ofs = NeF(f1, f2));

        self.put_float(
            match f1 != f2 {
                true => 1.0,
                false => 0.0,
            },
            ne_ofs,
        )?;

        Ok(())
    }

    // NE_V: Test inequality of two vectors
    pub fn op_ne_v(&mut self, v1_ofs: i16, v2_ofs: i16, ne_ofs: i16) -> Result<(), GlobalsError> {
        let v1 = self.get_vector(v1_ofs)?;
        let v2 = self.get_vector(v2_ofs)?;

        log_op!(self; ne_ofs = NeV(v1, v2));

        self.put_float(
            match v1 != v2 {
                true => 1.0,
                false => 0.0,
            },
            ne_ofs,
        )?;

        Ok(())
    }

    // NE_S: Test inequality of two strings
    pub fn op_ne_s(
        &mut self,
        string_table: &StringTable,
        s1_ofs: i16,
        s2_ofs: i16,
        ne_ofs: i16,
    ) -> Result<(), GlobalsError> {
        if s1_ofs < 0 || s2_ofs < 0 {
            return Err(GlobalsError::with_msg("eq_s: negative string offset"));
        }

        let s1 = self.string_id(s1_ofs)?;
        let s2 = self.string_id(s2_ofs)?;

        let s1 =
            string_table.get(s1).ok_or_else(|| GlobalsError::Other(format!("No string {s1}")))?;
        let s2 =
            string_table.get(s2).ok_or_else(|| GlobalsError::Other(format!("No string {s2}")))?;

        log_op!(self; ne_ofs = NeS(s1, s2));

        if s1_ofs != s2_ofs && s1 != s2 {
            self.put_float(1.0, ne_ofs)?;
        } else {
            self.put_float(0.0, ne_ofs)?;
        }

        Ok(())
    }

    pub fn op_ne_ent(&mut self, e1_ofs: i16, e2_ofs: i16, ne_ofs: i16) -> Result<(), GlobalsError> {
        let e1 = self.get_entity_id(e1_ofs)?;
        let e2 = self.get_entity_id(e2_ofs)?;

        log_op!(self; ne_ofs = NeEnt(e1, e2));

        self.put_float(
            match e1 != e2 {
                true => 1.0,
                false => 0.0,
            },
            ne_ofs,
        )?;

        Ok(())
    }

    pub fn op_ne_fnc(&mut self, f1_ofs: i16, f2_ofs: i16, ne_ofs: i16) -> Result<(), GlobalsError> {
        let f1 = self.get_function_id(f1_ofs)?;
        let f2 = self.get_function_id(f2_ofs)?;

        log_op!(self; ne_ofs = NeFnc(f1, f2));

        self.put_float(
            match f1 != f2 {
                true => 1.0,
                false => 0.0,
            },
            ne_ofs,
        )?;

        Ok(())
    }

    // LE: Less than or equal to comparison
    pub fn op_le(&mut self, f1_ofs: i16, f2_ofs: i16, le_ofs: i16) -> Result<(), GlobalsError> {
        let f1 = self.get_float(f1_ofs)?;
        let f2 = self.get_float(f2_ofs)?;

        log_op!(self; le_ofs = Le(f1, f2));

        self.put_float(
            match f1 <= f2 {
                true => 1.0,
                false => 0.0,
            },
            le_ofs,
        )?;

        Ok(())
    }

    // GE: Greater than or equal to comparison
    pub fn op_ge(&mut self, f1_ofs: i16, f2_ofs: i16, ge_ofs: i16) -> Result<(), GlobalsError> {
        let f1 = self.get_float(f1_ofs)?;
        let f2 = self.get_float(f2_ofs)?;

        log_op!(self; ge_ofs = Ge(f1, f2));

        self.put_float(
            match f1 >= f2 {
                true => 1.0,
                false => 0.0,
            },
            ge_ofs,
        )?;

        Ok(())
    }

    // LT: Less than comparison
    pub fn op_lt(&mut self, f1_ofs: i16, f2_ofs: i16, lt_ofs: i16) -> Result<(), GlobalsError> {
        let f1 = self.get_float(f1_ofs)?;
        let f2 = self.get_float(f2_ofs)?;

        log_op!(self; lt_ofs = Lt(f1, f2));

        self.put_float(
            match f1 < f2 {
                true => 1.0,
                false => 0.0,
            },
            lt_ofs,
        )?;

        Ok(())
    }

    // GT: Greater than comparison
    pub fn op_gt(&mut self, f1_ofs: i16, f2_ofs: i16, gt_ofs: i16) -> Result<(), GlobalsError> {
        let f1 = self.get_float(f1_ofs)?;
        let f2 = self.get_float(f2_ofs)?;

        log_op!(self; gt_ofs = Gt(f1, f2));

        self.put_float(
            match f1 > f2 {
                true => 1.0,
                false => 0.0,
            },
            gt_ofs,
        )?;

        Ok(())
    }

    // STORE_F
    pub fn op_store_f(
        &mut self,
        src_ofs: i16,
        dest_ofs: i16,
        unused: i16,
    ) -> Result<(), GlobalsError> {
        if unused != 0 {
            return Err(GlobalsError::with_msg("Nonzero arg3 to STORE_F"));
        }

        let f = self.get_float(src_ofs)?;

        log_op!(self; dest_ofs = StoreF(f));

        self.put_float(f, dest_ofs)?;

        Ok(())
    }

    // STORE_V
    pub fn op_store_v(
        &mut self,
        src_ofs: i16,
        dest_ofs: i16,
        unused: i16,
    ) -> Result<(), GlobalsError> {
        if unused != 0 {
            return Err(GlobalsError::with_msg("Nonzero arg3 to STORE_V"));
        }

        if (0..GLOBAL_STATIC_START as i16).contains(&dest_ofs) {
            let [a, b, c] = [0, 1, 2].map(|i| self.get_int(src_ofs + i).unwrap());
            log_op!(self; dest_ofs = StoreV(format_args!("[{a}, {b}, {c}]")));
            // Untyped copy is required because STORE_V is used to copy function arguments into the
            // global argument slots.
            //
            // See https://github.com/id-Software/Quake-Tools/blob/master/qcc/pr_comp.c#L362
            for c in 0..3 {
                self.untyped_copy(src_ofs + c as i16, dest_ofs + c as i16)?;
            }
        } else {
            let v = self.get_vector(src_ofs)?;
            log_op!(self; dest_ofs = StoreV(v));
            self.put_vector(v, dest_ofs)?;
        }

        Ok(())
    }

    pub fn op_store_s(
        &mut self,
        src_ofs: i16,
        dest_ofs: i16,
        unused: i16,
    ) -> Result<(), GlobalsError> {
        if unused != 0 {
            return Err(GlobalsError::with_msg("Nonzero arg3 to STORE_S"));
        }

        let s = self.string_id(src_ofs)?;

        log_op!(self; dest_ofs = StoreS(s));

        self.put_string_id(s, dest_ofs)?;

        Ok(())
    }

    pub fn op_store_ent(
        &mut self,
        src_ofs: i16,
        dest_ofs: i16,
        unused: i16,
    ) -> Result<(), GlobalsError> {
        if unused != 0 {
            return Err(GlobalsError::with_msg("Nonzero arg3 to STORE_ENT"));
        }

        let ent = self.get_entity_id(src_ofs)?;

        log_op!(self; dest_ofs = StoreEnt(ent));

        self.put_entity_id(ent, dest_ofs)?;

        Ok(())
    }

    pub fn op_store_fld(
        &mut self,
        src_ofs: i16,
        dest_ofs: i16,
        unused: i16,
    ) -> Result<(), GlobalsError> {
        if unused != 0 {
            return Err(GlobalsError::with_msg("Nonzero arg3 to STORE_FLD"));
        }

        let fld = self.get_field_addr(src_ofs)?;

        log_op!(self; dest_ofs = StoreFld(fld));

        self.put_field_addr(fld, dest_ofs)?;

        Ok(())
    }

    pub fn op_store_fnc(
        &mut self,
        src_ofs: i16,
        dest_ofs: i16,
        unused: i16,
    ) -> Result<(), GlobalsError> {
        if unused != 0 {
            return Err(GlobalsError::with_msg("Nonzero arg3 to STORE_FNC"));
        }

        let fnc = self.get_function_id(src_ofs)?;

        log_op!(self; dest_ofs = StoreFnc(fnc));

        self.put_function_id(fnc, dest_ofs)?;

        Ok(())
    }

    // NOT_F: Compare float to 0.0
    pub fn op_not_f(&mut self, f_id: i16, unused: i16, not_id: i16) -> Result<(), GlobalsError> {
        if unused != 0 {
            return Err(GlobalsError::with_msg("Nonzero arg2 to NOT_F"));
        }

        let f = self.get_float(f_id)?;

        log_op!(self; not_id = NotF(f));

        self.put_float(
            match f == 0.0 {
                true => 1.0,
                false => 0.0,
            },
            not_id,
        )?;

        Ok(())
    }

    // NOT_V: Compare vec to { 0.0, 0.0, 0.0 }
    pub fn op_not_v(&mut self, v_id: i16, unused: i16, not_id: i16) -> Result<(), GlobalsError> {
        if unused != 0 {
            return Err(GlobalsError::with_msg("Nonzero arg2 to NOT_V"));
        }

        let v = self.get_vector(v_id)?;

        log_op!(self; not_id = NotV(v));

        let zero_vec = Vec3::ZERO;
        self.put_vector(
            match v == zero_vec {
                true => Vec3::ONE,
                false => zero_vec,
            },
            not_id,
        )?;

        Ok(())
    }

    // NOT_S: Compare string to null string
    pub fn op_not_s(&mut self, s_ofs: i16, unused: i16, not_ofs: i16) -> Result<(), GlobalsError> {
        if unused != 0 {
            return Err(GlobalsError::with_msg("Nonzero arg2 to NOT_S"));
        }

        if s_ofs < 0 {
            return Err(GlobalsError::with_msg("not_s: negative string offset"));
        }

        let s = self.string_id(s_ofs)?;

        log_op!(self; not_ofs = NotS(s));

        if s_ofs == 0 || s.0 == 0 {
            self.put_float(1.0, not_ofs)?;
        } else {
            self.put_float(0.0, not_ofs)?;
        }

        Ok(())
    }

    // NOT_FNC: Compare function to null function (0)
    pub fn op_not_fnc(
        &mut self,
        fnc_id_ofs: i16,
        unused: i16,
        not_ofs: i16,
    ) -> Result<(), GlobalsError> {
        if unused != 0 {
            return Err(GlobalsError::with_msg("Nonzero arg2 to NOT_FNC"));
        }

        let fnc_id = self.get_function_id(fnc_id_ofs)?;

        log_op!(self; not_ofs = NotFnc(fnc_id));

        self.put_float(
            match fnc_id {
                FunctionId(0) => 1.0,
                _ => 0.0,
            },
            not_ofs,
        )?;

        Ok(())
    }

    // NOT_ENT: Compare entity to null entity (0)
    pub fn op_not_ent(
        &mut self,
        ent_ofs: i16,
        unused: i16,
        not_ofs: i16,
    ) -> Result<(), GlobalsError> {
        if unused != 0 {
            return Err(GlobalsError::with_msg("Nonzero arg2 to NOT_ENT"));
        }

        let ent = self.get_entity_id(ent_ofs)?;

        log_op!(self; not_ofs = NotEnt(ent));

        self.put_float(
            match ent {
                EntityId(0) => 1.0,
                _ => 0.0,
            },
            not_ofs,
        )?;

        Ok(())
    }

    // AND: Logical AND
    pub fn op_and(&mut self, f1_id: i16, f2_id: i16, and_id: i16) -> Result<(), GlobalsError> {
        let f1 = self.get_float(f1_id)?;
        let f2 = self.get_float(f2_id)?;

        log_op!(self; and_id = And(f1, f2));

        self.put_float(
            match f1 != 0.0 && f2 != 0.0 {
                true => 1.0,
                false => 0.0,
            },
            and_id,
        )?;

        Ok(())
    }

    // OR: Logical OR
    pub fn op_or(&mut self, f1_id: i16, f2_id: i16, or_id: i16) -> Result<(), GlobalsError> {
        let f1 = self.get_float(f1_id)?;
        let f2 = self.get_float(f2_id)?;

        log_op!(self; or_id = Or(f1, f2));

        self.put_float(
            match f1 != 0.0 || f2 != 0.0 {
                true => 1.0,
                false => 0.0,
            },
            or_id,
        )?;

        Ok(())
    }

    // BIT_AND: Bitwise AND
    pub fn op_bit_and(
        &mut self,
        f1_ofs: i16,
        f2_ofs: i16,
        bit_and_ofs: i16,
    ) -> Result<(), GlobalsError> {
        let f1 = self.get_float(f1_ofs)? as i32;
        let f2 = self.get_float(f2_ofs)? as i32;

        log_op!(self; bit_and_ofs = BitAnd(f1, f2));

        self.put_float((f1 & f2) as f32, bit_and_ofs)?;

        Ok(())
    }

    // BIT_OR: Bitwise OR
    pub fn op_bit_or(
        &mut self,
        f1_ofs: i16,
        f2_ofs: i16,
        bit_or_ofs: i16,
    ) -> Result<(), GlobalsError> {
        let f1 = self.get_float(f1_ofs)? as i32;
        let f2 = self.get_float(f2_ofs)? as i32;

        log_op!(self; bit_or_ofs = BitOr(f1, f2));

        self.put_float((f1 | f2) as f32, bit_or_ofs)?;

        Ok(())
    }

    // QuakeC built-in functions ===============================================

    #[inline]
    pub fn builtin_random(
        &mut self,
        rng: &mut crate::server::rng::ServerRng,
    ) -> Result<(), GlobalsError> {
        let r = rng.random_qc();
        self.put_float(r, GLOBAL_ADDR_RETURN as i16)
    }

    /// Calculate `v_forward`, `v_right` and `v_up` from `angles`.
    ///
    /// See [`angle_vectors`] for more information.
    ///
    /// Equivalent to `PF_makevectors`
    pub fn make_vectors(&mut self) -> Result<(), GlobalsError> {
        let angles = self.get_vector(GLOBAL_ADDR_ARG_0 as i16)?;

        let AngleVectors { forward, right, up } = angle_vectors(angles);

        self.put_vector(forward, GlobalAddrVector::VForward as i16)?;
        self.put_vector(right, GlobalAddrVector::VRight as i16)?;
        self.put_vector(up, GlobalAddrVector::VUp as i16)?;

        Ok(())
    }

    /// Calculate the magnitude of a vector.
    ///
    /// Loads the vector from `GLOBAL_ADDR_ARG_0` and stores its magnitude at
    /// `GLOBAL_ADDR_RETURN`.
    pub fn builtin_v_len(&mut self) -> Result<(), GlobalsError> {
        let v = self.get_vector(GLOBAL_ADDR_ARG_0 as i16)?;
        self.put_float(v.length(), GLOBAL_ADDR_RETURN as i16)?;
        Ok(())
    }

    /// Calculate a yaw angle from a direction vector.
    ///
    /// Loads the direction vector from `GLOBAL_ADDR_ARG_0` and stores the yaw value at
    /// `GLOBAL_ADDR_RETURN`.
    ///
    /// Equivalent to `PF_vectoyaw`
    pub fn builtin_vec_to_yaw(&mut self) -> Result<(), GlobalsError> {
        let v = self.get_vector(GLOBAL_ADDR_ARG_0 as i16)?;
        self.put_float(vec_to_angles(v).y, GLOBAL_ADDR_RETURN as i16)?;
        Ok(())
    }

    /// Calculate `[pitch, yaw, 0]` angle vector from a direction vector.
    ///
    /// Loads the direction vector from `GLOBAL_ADDR_ARG_0` and stores the angles at
    /// `GLOBAL_ADDR_RETURN`.
    ///
    /// Equivalent to `PF_vectoangles`.
    pub fn builtin_vec_to_angles(&mut self) -> Result<(), GlobalsError> {
        let v = self.get_vector(GLOBAL_ADDR_ARG_0 as i16)?;
        self.put_vector(vec_to_angles(v), GLOBAL_ADDR_RETURN as i16)?;
        Ok(())
    }

    /// Round a float to the nearest integer.
    ///
    /// Loads the float from `GLOBAL_ADDR_ARG_0` and stores the rounded value at
    /// `GLOBAL_ADDR_RETURN`.
    pub fn builtin_r_int(&mut self) -> Result<(), GlobalsError> {
        let f = self.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        self.put_float(f.round(), GLOBAL_ADDR_RETURN as i16)?;
        Ok(())
    }

    /// Round a float to the nearest integer less than or equal to it.
    ///
    /// Loads the float from `GLOBAL_ADDR_ARG_0` and stores the rounded value at
    /// `GLOBAL_ADDR_RETURN`.
    pub fn builtin_floor(&mut self) -> Result<(), GlobalsError> {
        let f = self.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        self.put_float(f.floor(), GLOBAL_ADDR_RETURN as i16)?;
        Ok(())
    }

    /// Round a float to the nearest integer greater than or equal to it.
    ///
    /// Loads the float from `GLOBAL_ADDR_ARG_0` and stores the rounded value at
    /// `GLOBAL_ADDR_RETURN`.
    pub fn builtin_ceil(&mut self) -> Result<(), GlobalsError> {
        let f = self.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        self.put_float(f.ceil(), GLOBAL_ADDR_RETURN as i16)?;
        Ok(())
    }

    /// Calculate the absolute value of a float.
    ///
    /// Loads the float from `GLOBAL_ADDR_ARG_0` and stores its absolute value at
    /// `GLOBAL_ADDR_RETURN`.
    pub fn builtin_f_abs(&mut self) -> Result<(), GlobalsError> {
        let f = self.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        self.put_float(f.abs(), GLOBAL_ADDR_RETURN as i16)?;
        Ok(())
    }
}

/// Calculates the rotation matrix for Quake `[pitch, yaw, roll]` angles, in degrees.
///
/// Quake's coordinate system has `x` forward, `y` left and `z` up. Yaw rotates about `z`, pitch
/// about `y` (positive pitch looks *down*) and roll about `x`, applied in that order. The columns
/// of the result are the forward, left and up vectors of the rotated frame. Note that QuakeC's
/// `v_right` is the *negation* of the second column; see [`angle_vectors`].
pub fn make_vectors<V: Into<Vec3>>(angles: V) -> Mat3 {
    let angles = angles.into();
    let pitch = angles[0].to_radians();
    let yaw = angles[1].to_radians();
    let roll = angles[2].to_radians();

    Mat3::from_euler(EulerRot::ZYX, yaw, pitch, roll)
}

/// The forward, right and up vectors for a set of angles.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct AngleVectors {
    pub forward: Vec3,
    pub right: Vec3,
    pub up: Vec3,
}

/// Calculates the forward, right and up vectors for Quake `[pitch, yaw, roll]` angles in degrees.
///
/// Equivalen to `AngleVectors` (`makevectors` in QuakeC).
pub fn angle_vectors<V: Into<Vec3>>(angles: V) -> AngleVectors {
    let rotation = make_vectors(angles);

    AngleVectors { forward: rotation.x_axis, right: -rotation.y_axis, up: rotation.z_axis }
}

/// Calculates `[pitch, yaw, 0]` angles (in degrees) pointing along `v`.
///
/// Angles are truncated to whole degrees and wrapped to `[0, 360)`.
///
/// Equivalent to `PF_vectoangles`.
pub fn vec_to_angles(v: Vec3) -> Vec3 {
    let (pitch, yaw);
    if v.x == 0. && v.y == 0. {
        yaw = 0.;
        pitch = if v.z > 0. { 90. } else { 270. };
    } else {
        let mut y = v.y.atan2(v.x).to_degrees().trunc();
        if y < 0. {
            y += 360.;
        }

        let forward = v.xy().length();
        let mut p = v.z.atan2(forward).to_degrees().trunc();
        if p < 0. {
            p += 360.;
        }

        yaw = y;
        pitch = p;
    }

    Vec3::new(pitch, yaw, 0.)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_addr_bounds() {
        let mut globals = Globals::new(Vec::new(), vec![[0; 4]; 4].into_boxed_slice());

        assert!(globals.get_bytes(3).is_ok());
        assert!(globals.put_bytes([1; 4], 3).is_ok());
        assert_eq!(globals.get_bytes(3).unwrap(), [1; 4]);

        // `addr == len` is one past the end.
        assert!(matches!(globals.get_addr(4), Err(GlobalsError::Address(4))));
        assert!(matches!(globals.get_addr_mut(4), Err(GlobalsError::Address(4))));
        assert!(matches!(globals.get_bytes(4), Err(GlobalsError::Address(4))));
        assert!(matches!(globals.put_bytes([0; 4], 4), Err(GlobalsError::Address(4))));
        assert!(matches!(globals.get_bytes(-1), Err(GlobalsError::Address(-1))));
    }

    /// Direct, literal port of `AngleVectors`, kept as close as possible to the original C.
    #[expect(clippy::neg_multiply, reason = "Direct port of `AngleVectors`")]
    fn quake_angle_vectors(angles: [f32; 3]) -> AngleVectors {
        let [pitch, yaw, roll] = angles.map(f32::to_radians);
        let (sy, cy) = yaw.sin_cos();
        let (sp, cp) = pitch.sin_cos();
        let (sr, cr) = roll.sin_cos();

        AngleVectors {
            forward: Vec3::new(cp * cy, cp * sy, -sp),
            right: Vec3::new(
                -1. * sr * sp * cy + -1. * cr * -sy,
                -1. * sr * sp * sy + -1. * cr * cy,
                -1. * sr * cp,
            ),
            up: Vec3::new(cr * sp * cy + -sr * -sy, cr * sp * sy + -sr * cy, cr * cp),
        }
    }

    fn assert_vec_approx_eq(a: Vec3, b: Vec3, what: &str) {
        assert!(a.abs_diff_eq(b, 1e-5), "{what}: {a} != {b}");
    }

    #[test]
    fn test_make_vectors_no_rotation() {
        let angles_zero = [0.0; 3];
        let result = make_vectors(angles_zero);
        assert_eq!(Mat3::IDENTITY, result);
    }

    #[test]
    fn test_angle_vectors_matches_quake() {
        let cases = [
            [0., 0., 0.],
            [90., 0., 0.],
            [-90., 0., 0.],
            [0., 90., 0.],
            [0., 0., 90.],
            [30., 45., 10.],
            [-20., 200., -35.],
            [10., -135., 5.],
            [80., 300., 170.],
        ];

        for angles in cases {
            let expected = quake_angle_vectors(angles);
            let actual = angle_vectors(angles);
            assert_vec_approx_eq(actual.forward, expected.forward, &format!("forward {angles:?}"));
            assert_vec_approx_eq(actual.right, expected.right, &format!("right {angles:?}"));
            assert_vec_approx_eq(actual.up, expected.up, &format!("up {angles:?}"));
        }
    }

    #[test]
    fn test_angle_vectors_conventions() {
        // Looking level at yaw 90 faces +Y, with right pointing along +X.
        let v = angle_vectors([0., 90., 0.]);
        assert_vec_approx_eq(v.forward, Vec3::Y, "forward");
        assert_vec_approx_eq(v.right, Vec3::X, "right");
        assert_vec_approx_eq(v.up, Vec3::Z, "up");

        // Positive pitch looks down.
        let v = angle_vectors([90., 0., 0.]);
        assert_vec_approx_eq(v.forward, -Vec3::Z, "forward");
    }

    #[test]
    fn test_vec_to_angles() {
        assert_eq!(vec_to_angles(Vec3::X), Vec3::new(0., 0., 0.));
        assert_eq!(vec_to_angles(Vec3::Y), Vec3::new(0., 90., 0.));
        assert_eq!(vec_to_angles(-Vec3::Y), Vec3::new(0., 270., 0.));
        assert_eq!(vec_to_angles(Vec3::Z), Vec3::new(90., 0., 0.));
        assert_eq!(vec_to_angles(-Vec3::Z), Vec3::new(270., 0., 0.));
        // Pitch is measured against the horizontal length, not just `y`.
        assert_eq!(vec_to_angles(Vec3::new(1., 0., 1.)), Vec3::new(45., 0., 0.));
        assert_eq!(vec_to_angles(Vec3::new(1., 0., -1.)), Vec3::new(315., 0., 0.));
        // Angles are truncated to whole degrees.
        assert_eq!(vec_to_angles(Vec3::new(1., 0.5, 0.)), Vec3::new(0., 26., 0.));
    }
}
