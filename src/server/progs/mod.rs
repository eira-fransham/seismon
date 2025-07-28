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

//! QuakeC bytecode interpreter
//!
//! # Loading
//!
//! QuakeC bytecode is typically loaded from `progs.dat` or `qwprogs.dat`. Bytecode files begin with
//! a brief header with an `i32` format version number (which must equal VERSION) and an `i32` CRC
//! checksum to ensure the correct bytecode is being loaded.
//!
//! ```text
//! version: i32,
//! crc: i32,
//! ```
//!
//! This is followed by a series of six lumps acting as a directory into the file data. Each lump
//! consists of an `i32` byte offset into the file data and an `i32` element count.
//!
//! ```text
//! statement_offset: i32,
//! statement_count: i32,
//!
//! globaldef_offset: i32,
//! globaldef_count: i32,
//!
//! fielddef_offset: i32,
//! fielddef_count: i32,
//!
//! function_offset: i32,
//! function_count: i32,
//!
//! string_offset: i32,
//! string_count: i32,
//!
//! global_offset: i32,
//! global_count: i32,
//! ```
//!
//! These offsets are not guaranteed to be in order, and in fact `progs.dat` usually has the string
//! section first. Offsets are in bytes from the beginning of the file.
//!
//! ## String data
//!
//! The string data block is located at the offset given by `string_offset` and consists of a series
//! of null-terminated ASCII strings laid end-to-end. The first string is always the empty string,
//! i.e. the first byte is always the null byte. The total size in bytes of the string data is given
//! by `string_count`.
//!
//! ## Statements
//!
//! The statement table is located at the offset given by `statement_offset` and consists of
//! `statement_count` 8-byte instructions of the form
//!
//! ```text
//! opcode: u16,
//! arg1: i16,
//! arg2: i16,
//! arg3: i16,
//! ```
//!
//! Not every opcode uses three arguments, but all statements have space for three arguments anyway,
//! probably for simplicity. The semantics of these arguments differ depending on the opcode.
//!
//! ## Function Definitions
//!
//! Function definitions contain both high-level information about the function (name and source
//! file) and low-level information necessary to execute it (entry point, argument count, etc).
//! Functions are stored on disk as follows:
//!
//! ```text
//! statement_id: i32,     // index of first statement; negatives are built-in functions
//! arg_start: i32,        // address to store/load first argument
//! local_count: i32,      // number of local variables on the stack
//! profile: i32,          // incremented every time function called
//! fnc_name_ofs: i32,     // offset of function name in string table
//! srcfile_name_ofs: i32, // offset of source file name in string table
//! arg_count: i32,        // number of arguments (max. 8)
//! arg_sizes: [u8; 8],    // sizes of each argument
//! ```

pub mod functions;
pub mod globals;
mod ops;
mod string_table;

use std::{
    fmt,
    io::{Read, Seek, SeekFrom},
    iter,
    result::Result as StdResult,
};

use crate::{
    common::{bsp::BspError, console::ConsoleError, net::NetError},
    server::world::{EntityError, EntityTypeDef},
};

use bevy::prelude::*;
use byteorder::{LittleEndian, ReadBytesExt};
use num::FromPrimitive;
use num_derive::FromPrimitive;
use snafu::{Backtrace, prelude::*};

use self::{
    functions::{BuiltinFunctionId, FunctionDef, FunctionKind, MAX_ARGS, Statement},
    globals::{GLOBAL_ADDR_ARG_0, GLOBAL_STATIC_COUNT},
};
pub use self::{
    functions::{FunctionId, Functions},
    globals::{
        GlobalAddrEntity, GlobalAddrFloat, GlobalAddrFunction, GlobalAddrVector, Globals,
        GlobalsError,
    },
    ops::Opcode,
    string_table::StringTable,
};

const VERSION: i32 = 6;
const CRC: i32 = 5927;
const MAX_CALL_STACK_DEPTH: usize = 32;
const MAX_LOCAL_STACK_DEPTH: usize = 2048;
const LUMP_COUNT: usize = 6;
const SAVE_GLOBAL: u16 = 1 << 15;

// the on-disk size of a bytecode statement
const STATEMENT_SIZE: usize = 8;

// the on-disk size of a function declaration
const FUNCTION_SIZE: usize = 36;

// the on-disk size of a global or field definition
const DEF_SIZE: usize = 8;

pub type Result<T> = StdResult<T, ProgsError>;

#[derive(Snafu, Debug)]
pub enum ProgsError {
    #[snafu(context(false), display("{source:?}"))]
    Io {
        source: ::std::io::Error,
        backtrace: Backtrace,
    },
    #[snafu(context(false))]
    Globals {
        source: GlobalsError,
        backtrace: Backtrace,
    },
    #[snafu(context(false))]
    Net {
        source: NetError,
        backtrace: Backtrace,
    },
    #[snafu(context(false))]
    Console {
        source: ConsoleError,
        backtrace: Backtrace,
    },
    #[snafu(context(false))]
    Entity {
        source: EntityError,
        backtrace: Backtrace,
    },
    #[snafu(context(false))]
    Bsp {
        source: BspError,
        backtrace: Backtrace,
    },
    CallStackOverflow {
        backtrace: Backtrace,
    },
    LocalStackOverflow {
        backtrace: Backtrace,
    },
    #[snafu(display("{message}"))]
    Other {
        message: String,
        backtrace: Backtrace,
    },
}

impl ProgsError {
    pub fn with_msg<S>(msg: S) -> Self
    where
        S: Into<String>,
    {
        ProgsError::Other {
            message: msg.into(),
            backtrace: Backtrace::capture(),
        }
    }
}

#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
#[repr(C)]
pub struct StringId(pub usize);

impl StringId {
    pub fn is_none(&self) -> bool {
        self.0 == 0
    }
}

impl TryInto<i32> for StringId {
    type Error = ProgsError;

    fn try_into(self) -> StdResult<i32, Self::Error> {
        if self.0 > i32::MAX as usize {
            Err(ProgsError::with_msg("string id out of i32 range"))
        } else {
            Ok(self.0 as i32)
        }
    }
}

impl StringId {
    pub fn new() -> StringId {
        StringId(0)
    }
}

impl fmt::Display for StringId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "s{}", self.0)
    }
}

#[derive(Copy, Clone, Debug, Default, Eq, Hash, PartialEq)]
#[repr(C)]
pub struct EntityId(pub i32);

impl fmt::Display for EntityId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "e{}", self.0)
    }
}

impl EntityId {
    pub const NONE: Self = Self(0);

    pub fn is_none(&self) -> bool {
        self.0 <= 0
    }
}

impl From<EntityId> for u16 {
    fn from(other: EntityId) -> Self {
        other.0 as _
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
#[repr(C)]
pub struct FieldAddr(pub usize);

impl fmt::Display for FieldAddr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fld{}", self.0)
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
#[repr(C)]
pub struct EntityFieldAddr {
    pub entity_id: EntityId,
    pub field_addr: FieldAddr,
}

enum LumpId {
    Statements = 0,
    GlobalDefs = 1,
    Fielddefs = 2,
    Functions = 3,
    Strings = 4,
    Globals = 5,
}

#[derive(Copy, Clone, Debug, FromPrimitive, PartialEq)]
#[repr(u16)]
pub enum Type {
    QVoid = 0,
    QString = 1,
    QFloat = 2,
    QVector = 3,
    QEntity = 4,
    QField = 5,
    QFunction = 6,
    QPointer = 7,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::QVoid => write!(f, "void"),
            Self::QString => write!(f, "string"),
            Self::QFloat => write!(f, "float"),
            Self::QVector => write!(f, "vector"),
            Self::QEntity => write!(f, "entity"),
            Self::QField => write!(f, "field"),
            Self::QFunction => write!(f, "function"),
            Self::QPointer => write!(f, "pointer"),
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct Lump {
    offset: usize,
    count: usize,
}

#[derive(Debug, Clone)]
pub struct GlobalDef {
    // TODO: Implement this
    _save: bool,
    type_: Type,
    offset: u16,
    name_id: StringId,
    /// Dereferenced version of `name_id`, for debuggging purposes
    debug_name: String,
}

impl GlobalDef {
    pub fn format(&self, f: &mut fmt::Formatter, string_table: &StringTable) -> fmt::Result {
        let name = string_table.get(self.name_id).unwrap();
        write!(f, "{} {} = {}", self.type_, name, self.offset)
    }

    pub fn display<'a>(&'a self, string_table: &'a StringTable) -> impl fmt::Display + use<'a> {
        GlobalDefFormatter {
            def: self,
            string_table,
        }
    }
}

pub struct GlobalDefFormatter<'a> {
    def: &'a GlobalDef,
    string_table: &'a StringTable,
}

impl fmt::Display for GlobalDefFormatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.def.format(f, self.string_table)
    }
}

/// An entity field definition.
///
/// These definitions can be used to look up entity fields by name. This is
/// required for custom fields defined in QuakeC code; their offsets are not
/// known at compile time.
#[derive(Debug, Copy, Clone)]
pub struct FieldDef {
    pub type_: Type,
    pub offset: u16,
    pub name_id: StringId,
}

impl FieldDef {
    pub fn format(&self, f: &mut fmt::Formatter, string_table: &StringTable) -> fmt::Result {
        let name = string_table.get(self.name_id).unwrap();
        write!(f, "{} {} = {}", self.type_, name, self.offset)
    }

    pub fn display<'a>(&self, string_table: &'a StringTable) -> impl fmt::Display + use<'a> {
        FieldDefFormatter {
            def: *self,
            string_table,
        }
    }
}

pub struct FieldDefFormatter<'a> {
    def: FieldDef,
    string_table: &'a StringTable,
}

impl fmt::Display for FieldDefFormatter<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.def.format(f, self.string_table)
    }
}

/// The values returned by loading a `progs.dat` file.
pub struct LoadProgs {
    pub cx: ExecutionContext,
    pub globals: Globals,
    pub entity_def: EntityTypeDef,
    pub string_table: StringTable,
}

/// Loads all data from a `progs.dat` file.
///
/// This returns objects representing the necessary context to execute QuakeC bytecode.
pub fn load<R>(mut src: R) -> Result<LoadProgs>
where
    R: Read + Seek,
{
    assert!(src.read_i32::<LittleEndian>()? == VERSION);
    assert!(src.read_i32::<LittleEndian>()? == CRC);

    let mut lumps = [Lump {
        offset: 0,
        count: 0,
    }; LUMP_COUNT];

    for (i, lump) in lumps.iter_mut().enumerate() {
        *lump = Lump {
            offset: src.read_i32::<LittleEndian>()? as usize,
            count: src.read_i32::<LittleEndian>()? as usize,
        };

        debug!("{:?}: {:?}", i, lump);
    }

    let ent_addr_count = src.read_i32::<LittleEndian>()? as usize;
    debug!("Field count: {}", ent_addr_count);

    // Read string data and construct StringTable

    let string_lump = &lumps[LumpId::Strings as usize];
    src.seek(SeekFrom::Start(string_lump.offset as u64))?;
    let mut strings = Vec::new();
    (&mut src)
        .take(string_lump.count as u64)
        .read_to_end(&mut strings)?;
    let string_table = StringTable::new(strings);

    assert_eq!(
        src.stream_position()?,
        src.seek(SeekFrom::Start(
            (string_lump.offset + string_lump.count) as u64,
        ))?
    );

    // Read function definitions and statements and construct Functions

    let function_lump = &lumps[LumpId::Functions as usize];
    src.seek(SeekFrom::Start(function_lump.offset as u64))?;
    let mut function_defs = Vec::with_capacity(function_lump.count);
    for i in 0..function_lump.count {
        assert_eq!(
            src.stream_position()?,
            src.seek(SeekFrom::Start(
                (function_lump.offset + i * FUNCTION_SIZE) as u64,
            ))?
        );

        let kind = match src.read_i32::<LittleEndian>()? {
            x if x < 0 => match BuiltinFunctionId::from_i32(-x) {
                Some(f) => FunctionKind::BuiltIn(f),
                None => {
                    return Err(ProgsError::with_msg(format!(
                        "Invalid built-in function ID {}",
                        -x
                    )));
                }
            },
            x => FunctionKind::QuakeC(x as usize),
        };

        let arg_start = src.read_i32::<LittleEndian>()?;
        let locals = src.read_i32::<LittleEndian>()?;

        // throw away profile variable
        let _ = src.read_i32::<LittleEndian>()?;

        let name_id = string_table.id_from_i32(src.read_i32::<LittleEndian>()?)?;
        let srcfile_id = string_table.id_from_i32(src.read_i32::<LittleEndian>()?)?;

        let argc = src.read_i32::<LittleEndian>()?;
        let mut argsz = [0; MAX_ARGS];
        src.read_exact(&mut argsz)?;

        function_defs.push(FunctionDef {
            kind,
            arg_start: arg_start as usize,
            locals: locals as usize,
            name_id,
            srcfile_id,
            argc: argc as usize,
            argsz,
        });
    }

    assert_eq!(
        src.stream_position()?,
        src.seek(SeekFrom::Start(
            (function_lump.offset + function_lump.count * FUNCTION_SIZE) as u64,
        ))?
    );

    let statement_lump = &lumps[LumpId::Statements as usize];
    src.seek(SeekFrom::Start(statement_lump.offset as u64))?;
    let mut statements = Vec::with_capacity(statement_lump.count);
    for _ in 0..statement_lump.count {
        statements.push(Statement::new(
            src.read_i16::<LittleEndian>()?,
            src.read_i16::<LittleEndian>()?,
            src.read_i16::<LittleEndian>()?,
            src.read_i16::<LittleEndian>()?,
        )?);
    }

    assert_eq!(
        src.stream_position()?,
        src.seek(SeekFrom::Start(
            (statement_lump.offset + statement_lump.count * STATEMENT_SIZE) as u64,
        ))?
    );

    let functions = Functions {
        defs: function_defs.into_boxed_slice(),
        statements: statements.into_boxed_slice(),
    };

    let globaldef_lump = &lumps[LumpId::GlobalDefs as usize];
    src.seek(SeekFrom::Start(globaldef_lump.offset as u64))?;
    let mut globaldefs = Vec::new();
    for _ in 0..globaldef_lump.count {
        let type_ = src.read_u16::<LittleEndian>()?;
        let offset = src.read_u16::<LittleEndian>()?;
        let name_id = string_table.id_from_i32(src.read_i32::<LittleEndian>()?)?;
        let debug_name = format!("{}", string_table.get(name_id).unwrap_or_default());
        globaldefs.push(GlobalDef {
            _save: type_ & SAVE_GLOBAL != 0,
            type_: Type::from_u16(type_ & !SAVE_GLOBAL).unwrap(),
            offset,
            name_id,
            debug_name,
        });
    }

    globaldefs.sort_by_key(|def| def.offset);

    assert_eq!(
        src.stream_position()?,
        src.seek(SeekFrom::Start(
            (globaldef_lump.offset + globaldef_lump.count * DEF_SIZE) as u64,
        ))?
    );

    let fielddef_lump = &lumps[LumpId::Fielddefs as usize];
    src.seek(SeekFrom::Start(fielddef_lump.offset as u64))?;
    let mut field_defs = Vec::new();
    for _ in 0..fielddef_lump.count {
        let type_ = src.read_u16::<LittleEndian>()?;
        let offset = src.read_u16::<LittleEndian>()?;
        let name_id = string_table.id_from_i32(src.read_i32::<LittleEndian>()?)?;

        if type_ & SAVE_GLOBAL != 0 {
            return Err(ProgsError::with_msg(
                "Save flag not allowed in field definitions",
            ));
        }
        field_defs.push(FieldDef {
            type_: Type::from_u16(type_).unwrap(),
            offset,
            name_id,
        });
    }

    assert_eq!(
        src.stream_position()?,
        src.seek(SeekFrom::Start(
            (fielddef_lump.offset + fielddef_lump.count * DEF_SIZE) as u64,
        ))?
    );

    let globals_lump = &lumps[LumpId::Globals as usize];
    src.seek(SeekFrom::Start(globals_lump.offset as u64))?;

    if globals_lump.count < GLOBAL_STATIC_COUNT {
        return Err(ProgsError::with_msg(
            "Global count lower than static global count",
        ));
    }

    let mut addrs = Vec::with_capacity(globals_lump.count);
    for _ in 0..globals_lump.count {
        let mut block = [0; 4];
        src.read_exact(&mut block)?;

        // TODO: handle endian conversions (BigEndian systems should use BigEndian internally)
        addrs.push(block);
    }

    assert_eq!(
        src.stream_position()?,
        src.seek(SeekFrom::Start(
            (globals_lump.offset + globals_lump.count * 4) as u64,
        ))?
    );

    let cx = ExecutionContext::create(functions);

    let globals = Globals::new(globaldefs.into_boxed_slice(), addrs.into_boxed_slice());

    let entity_def = EntityTypeDef::new(ent_addr_count, field_defs.into_boxed_slice().into())?;

    Ok(LoadProgs {
        cx,
        globals,
        entity_def,
        string_table,
    })
}

#[derive(Debug)]
struct StackFrame {
    instr_id: usize,
    func_id: FunctionId,
}

/// A QuakeC VM context.
#[derive(Debug)]
pub struct ExecutionContext {
    functions: Functions,
    pc: usize,
    current_function: FunctionId,
    call_stack: Vec<StackFrame>,
    local_stack: Vec<[u8; 4]>,
}

impl ExecutionContext {
    pub fn create(functions: Functions) -> ExecutionContext {
        ExecutionContext {
            functions,
            pc: 0,
            current_function: FunctionId(0),
            call_stack: Vec::with_capacity(MAX_CALL_STACK_DEPTH),
            local_stack: Vec::with_capacity(MAX_LOCAL_STACK_DEPTH),
        }
    }

    pub fn call_stack_depth(&self) -> usize {
        self.call_stack.len()
    }

    pub fn backtrace(&self) -> impl Iterator<Item = FunctionId> + '_ {
        self.call_stack
            .iter()
            .map(|StackFrame { func_id, .. }| *func_id)
            .chain(iter::once(self.current_function))
    }

    pub fn print_backtrace(&self, string_table: &StringTable, force: bool) {
        let backtrace_var =
            std::env::var("RUST_LIB_BACKTRACE").or_else(|_| std::env::var("RUST_BACKTRACE"));
        let backtrace_enabled = matches!(backtrace_var.as_deref(), Err(_) | Ok("0"));
        if force || backtrace_enabled {
            for (depth, id) in self.backtrace().enumerate() {
                let def = self.function_def(id).unwrap();

                let name_id = def.name_id;
                let name = string_table.get(name_id).unwrap();

                println!("{}: {} - {:?}", depth, name, def.kind);
            }
        }
    }

    pub fn current_function(&self) -> FunctionId {
        self.current_function
    }

    /// Should only be used to reset the current function if progs are called (e.g. in `builtin_walk_move`).
    pub fn set_current_function(&mut self, func: FunctionId) {
        self.current_function = func;
    }

    pub fn find_function_by_name<S: AsRef<str>>(
        &mut self,
        string_table: &StringTable,
        name: S,
    ) -> Result<FunctionId> {
        self.functions.find_function_by_name(string_table, name)
    }

    pub fn exists(&self, id: FunctionId) -> bool {
        self.functions.exists(id)
    }

    pub fn function_def(&self, id: FunctionId) -> Result<&FunctionDef> {
        self.functions.get_def(id)
    }

    pub fn reset(&mut self) {
        self.call_stack.clear();
        self.local_stack.clear();
        self.current_function = FunctionId(0);
    }

    pub fn enter_function(
        &mut self,
        string_table: &StringTable,
        globals: &mut Globals,
        f: FunctionId,
    ) -> Result<()> {
        let def = self.functions.get_def(f)?;
        debug!(
            "Calling QuakeC function {}",
            string_table.get(def.name_id).unwrap()
        );

        // save stack frame
        self.call_stack.push(StackFrame {
            instr_id: self.pc,
            func_id: self.current_function,
        });

        // check call stack overflow
        if self.call_stack.len() >= MAX_CALL_STACK_DEPTH {
            return Err(ProgsError::CallStackOverflow {
                backtrace: Backtrace::capture(),
            });
        }

        // preemptively check local stack overflow
        if self.local_stack.len() + def.locals > MAX_LOCAL_STACK_DEPTH {
            return Err(ProgsError::LocalStackOverflow {
                backtrace: Backtrace::capture(),
            });
        }

        // save locals to stack
        for i in 0..def.locals {
            self.local_stack
                .push(globals.get_bytes((def.arg_start + i) as i16)?);
        }

        let mut dest = def.arg_start;
        for arg in 0..def.argc {
            for component in 0..def.argsz[arg] as usize {
                let val = globals.get_bytes((GLOBAL_ADDR_ARG_0 + arg * 3 + component) as i16)?;
                globals.put_bytes(val, dest as i16)?;
                dest += 1;
            }
        }

        self.current_function = f;

        match def.kind {
            FunctionKind::BuiltIn(_) => {
                panic!("built-in functions should not be called with enter_function()")
            }
            FunctionKind::QuakeC(pc) => self.pc = pc,
        }

        Ok(())
    }

    pub fn leave_function(
        &mut self,
        string_table: &StringTable,
        globals: &mut Globals,
    ) -> Result<()> {
        let def = self.functions.get_def(self.current_function)?;
        debug!(
            "Returning from QuakeC function {}",
            string_table.get(def.name_id).unwrap()
        );

        for i in (0..def.locals).rev() {
            globals.put_bytes(
                self.local_stack
                    .pop()
                    .ok_or_else(|| ProgsError::with_msg("local stack underflow"))?,
                (def.arg_start + i) as i16,
            )?;
        }

        let frame = self
            .call_stack
            .pop()
            .ok_or_else(|| ProgsError::with_msg("call stack underflow"))?;

        self.current_function = frame.func_id;
        self.pc = frame.instr_id;

        Ok(())
    }

    pub fn load_statement(&self) -> Statement {
        self.functions.statements[self.pc].clone()
    }

    /// Performs an unconditional relative jump.
    pub fn jump_relative(&mut self, rel: i16) {
        self.pc = (self.pc as isize + rel as isize) as usize;
    }
}
