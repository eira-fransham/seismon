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

use std::{fmt, io::SeekFrom, iter, result::Result as StdResult};

use bevy_mod_commands::ConsoleError;

use crate::{
    common::net::NetError,
    server::{
        precache::PrecacheError,
        world::{EntityError, EntityTypeDef, hull::HullError},
    },
};

use bevy::{
    asset::{AssetLoader, LoadContext, io::Reader},
    prelude::*,
};
use futures::{AsyncRead, AsyncReadExt as _, AsyncSeek, AsyncSeekExt as _};
use futures_byteorder::{AsyncReadBytes, LittleEndian};
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
/// Used to ensure that the entity field layout matches the ones we expect for the builtins.
///
/// In the future, this could potentially be used to switch between different sets of builtins
/// that expect different entity fields.
const CRC: i32 = 0x1727;
const MAX_CALL_STACK_DEPTH: usize = 32;
/// Maximum number of statements executed by a single call to `execute_program` before it is
/// considered an infinite loop (see `PR_ExecuteProgram` in Quake).
pub const MAX_OPS_SINGLE_CALL: u32 = 100_000;
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
    Hull {
        source: HullError,
        backtrace: Backtrace,
    },
    #[snafu(context(false))]
    Precache {
        source: PrecacheError,
        backtrace: Backtrace,
    },
    CallStackOverflow {
        backtrace: Backtrace,
    },
    LocalStackOverflow {
        backtrace: Backtrace,
    },
    #[snafu(display("runaway loop error: more than {MAX_OPS_SINGLE_CALL} statements executed"))]
    RunawayLoop {
        backtrace: Backtrace,
    },
    #[snafu(display("progs version {found} not supported (expected {VERSION})"))]
    BadVersion {
        found: i32,
        backtrace: Backtrace,
    },
    #[snafu(display(
        "progs CRC {found} does not match the expected CRC {CRC}: this progs.dat uses an \
         incompatible entity field layout"
    ))]
    BadCrc {
        found: i32,
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
        ProgsError::Other { message: msg.into(), backtrace: Backtrace::capture() }
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
        GlobalDefFormatter { def: self, string_table }
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
        FieldDefFormatter { def: *self, string_table }
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

/// A parsed `progs.dat` file.
///
/// Levels should start from a clone of this, so that each level gets a fresh VM state (as Quake
/// does in `SV_SpawnServer`).
#[derive(Asset, TypePath, Clone)]
pub struct ProgsAsset {
    pub cx: ExecutionContext,
    pub globals: Globals,
    pub entity_def: EntityTypeDef,
    pub string_table: StringTable,
}

impl ProgsAsset {
    /// Loads all data from a `progs.dat` file.
    ///
    /// This returns objects representing the necessary context to execute QuakeC bytecode.
    async fn load<R>(src: &mut R) -> Result<ProgsAsset>
    where
        R: AsyncRead + AsyncSeek + Unpin,
    {
        let mut src = AsyncReadBytes::new(src);

        let version = src.read_i32::<LittleEndian>().await?;
        if version != VERSION {
            return Err(ProgsError::BadVersion { found: version, backtrace: Backtrace::capture() });
        }

        // Quake only uses the CRC to check compatibility with `progdefs.h`. We hard-code the
        // field offsets from `progdefs.h` (see the `FieldAddr*` enums), so a mismatch really does
        // mean that this `progs.dat` is incompatible.
        let crc = src.read_i32::<LittleEndian>().await?;
        if crc != CRC {
            return Err(ProgsError::BadCrc { found: crc, backtrace: Backtrace::capture() });
        }

        let mut lumps = [Lump { offset: 0, count: 0 }; LUMP_COUNT];

        for (i, lump) in lumps.iter_mut().enumerate() {
            *lump = Lump {
                offset: src.read_i32::<LittleEndian>().await? as usize,
                count: src.read_i32::<LittleEndian>().await? as usize,
            };

            debug!("{:?}: {:?}", i, lump);
        }

        let ent_addr_count = src.read_i32::<LittleEndian>().await? as usize;
        debug!("Field count: {}", ent_addr_count);

        // Read string data and construct StringTable

        let string_lump = &lumps[LumpId::Strings as usize];
        src.seek(SeekFrom::Start(string_lump.offset as u64)).await?;
        let mut strings = Vec::new();
        (&mut *src).take(string_lump.count as u64).read_to_end(&mut strings).await?;
        let string_table = StringTable::new(strings);

        assert_eq!(
            src.stream_position().await?,
            src.seek(SeekFrom::Start((string_lump.offset + string_lump.count) as u64,)).await?
        );

        // Read function definitions and statements and construct Functions

        let function_lump = &lumps[LumpId::Functions as usize];
        src.seek(SeekFrom::Start(function_lump.offset as u64)).await?;
        let mut function_defs = Vec::with_capacity(function_lump.count);
        for i in 0..function_lump.count {
            assert_eq!(
                src.stream_position().await?,
                src.seek(SeekFrom::Start((function_lump.offset + i * FUNCTION_SIZE) as u64,))
                    .await?
            );

            let kind = match src.read_i32::<LittleEndian>().await? {
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

            let arg_start = src.read_i32::<LittleEndian>().await?;
            let locals = src.read_i32::<LittleEndian>().await?;

            // throw away profile variable
            let _ = src.read_i32::<LittleEndian>().await?;

            let name_id = string_table.id_from_i32(src.read_i32::<LittleEndian>().await?)?;
            let srcfile_id = string_table.id_from_i32(src.read_i32::<LittleEndian>().await?)?;

            let argc = src.read_i32::<LittleEndian>().await?;
            let mut argsz = [0; MAX_ARGS];
            src.read_exact(&mut argsz).await?;

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
            src.stream_position().await?,
            src.seek(SeekFrom::Start(
                (function_lump.offset + function_lump.count * FUNCTION_SIZE) as u64,
            ))
            .await?
        );

        let statement_lump = &lumps[LumpId::Statements as usize];
        src.seek(SeekFrom::Start(statement_lump.offset as u64)).await?;
        let mut statements = Vec::with_capacity(statement_lump.count);
        for _ in 0..statement_lump.count {
            statements.push(Statement::new(
                src.read_i16::<LittleEndian>().await?,
                src.read_i16::<LittleEndian>().await?,
                src.read_i16::<LittleEndian>().await?,
                src.read_i16::<LittleEndian>().await?,
            )?);
        }

        assert_eq!(
            src.stream_position().await?,
            src.seek(SeekFrom::Start(
                (statement_lump.offset + statement_lump.count * STATEMENT_SIZE) as u64,
            ))
            .await?
        );

        let functions = Functions {
            defs: function_defs.into_boxed_slice(),
            statements: statements.into_boxed_slice(),
        };

        let globaldef_lump = &lumps[LumpId::GlobalDefs as usize];
        src.seek(SeekFrom::Start(globaldef_lump.offset as u64)).await?;
        let mut globaldefs = Vec::new();
        for _ in 0..globaldef_lump.count {
            let type_ = src.read_u16::<LittleEndian>().await?;
            let offset = src.read_u16::<LittleEndian>().await?;
            let name_id = string_table.id_from_i32(src.read_i32::<LittleEndian>().await?)?;
            let debug_name = format!("{}", string_table.get(name_id).unwrap_or_default());
            globaldefs.push(GlobalDef {
                _save: type_ & SAVE_GLOBAL != 0,
                type_: Type::from_u16(type_ & !SAVE_GLOBAL).ok_or_else(|| {
                    ProgsError::with_msg(format!("invalid global definition type {type_}"))
                })?,
                offset,
                name_id,
                debug_name,
            });
        }

        globaldefs.sort_by_key(|def| def.offset);

        assert_eq!(
            src.stream_position().await?,
            src.seek(SeekFrom::Start(
                (globaldef_lump.offset + globaldef_lump.count * DEF_SIZE) as u64,
            ))
            .await?
        );

        let fielddef_lump = &lumps[LumpId::Fielddefs as usize];
        src.seek(SeekFrom::Start(fielddef_lump.offset as u64)).await?;
        let mut field_defs = Vec::new();
        for _ in 0..fielddef_lump.count {
            let type_ = src.read_u16::<LittleEndian>().await?;
            let offset = src.read_u16::<LittleEndian>().await?;
            let name_id = string_table.id_from_i32(src.read_i32::<LittleEndian>().await?)?;

            if type_ & SAVE_GLOBAL != 0 {
                return Err(ProgsError::with_msg("Save flag not allowed in field definitions"));
            }
            let type_ = Type::from_u16(type_).ok_or_else(|| {
                ProgsError::with_msg(format!("invalid field definition type {type_}"))
            })?;
            field_defs.push(FieldDef { type_, offset, name_id });
        }

        assert_eq!(
            src.stream_position().await?,
            src.seek(SeekFrom::Start(
                (fielddef_lump.offset + fielddef_lump.count * DEF_SIZE) as u64,
            ))
            .await?
        );

        let globals_lump = &lumps[LumpId::Globals as usize];
        src.seek(SeekFrom::Start(globals_lump.offset as u64)).await?;

        if globals_lump.count < GLOBAL_STATIC_COUNT {
            return Err(ProgsError::with_msg("Global count lower than static global count"));
        }

        let mut addrs = Vec::with_capacity(globals_lump.count);
        for _ in 0..globals_lump.count {
            let mut block = [0; 4];
            src.read_exact(&mut block).await?;

            // TODO: handle endian conversions (BigEndian systems should use BigEndian internally)
            addrs.push(block);
        }

        assert_eq!(
            src.stream_position().await?,
            src.seek(SeekFrom::Start((globals_lump.offset + globals_lump.count * 4) as u64,))
                .await?
        );

        let cx = ExecutionContext::create(functions);

        let globals = Globals::new(globaldefs.into_boxed_slice(), addrs.into_boxed_slice());

        let entity_def = EntityTypeDef::new(ent_addr_count, field_defs.into_boxed_slice())?;

        Ok(ProgsAsset { cx, globals, entity_def, string_table })
    }
}

/// Loads `progs.dat` as a [`ProgsAsset`].
#[derive(Default, TypePath)]
pub struct ProgsLoader;

impl AssetLoader for ProgsLoader {
    type Asset = ProgsAsset;
    type Settings = ();
    type Error = ProgsError;

    async fn load(
        &self,
        reader: &mut dyn Reader,
        _settings: &Self::Settings,
        _load_context: &mut LoadContext<'_>,
    ) -> Result<Self::Asset> {
        // `ProgsAsset::load` needs to seek in order to follow lump offsets.
        match reader.seekable() {
            Ok(mut reader) => ProgsAsset::load(&mut reader).await,
            Err(_) => {
                let mut bytes = Vec::new();
                reader.read_to_end(&mut bytes).await?;
                ProgsAsset::load(&mut futures::io::Cursor::new(bytes)).await
            }
        }
    }

    fn extensions(&self) -> &[&str] {
        &["dat"]
    }
}

#[derive(Debug, Clone)]
struct StackFrame {
    instr_id: usize,
    func_id: FunctionId,
}

/// A QuakeC VM context.
#[derive(Debug, Clone)]
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

    /// Logs the QuakeC call stack, if `force` is set or backtraces are enabled with
    /// `RUST_LIB_BACKTRACE`/`RUST_BACKTRACE`.
    pub fn print_backtrace(&self, string_table: &StringTable, force: bool) {
        let backtrace_var =
            std::env::var("RUST_LIB_BACKTRACE").or_else(|_| std::env::var("RUST_BACKTRACE"));
        let backtrace_enabled = matches!(backtrace_var.as_deref(), Ok(v) if v != "0");
        if force || backtrace_enabled {
            // This runs on error paths, so it must not fail itself.
            let frames = self
                .backtrace()
                .enumerate()
                .map(|(depth, id)| match self.function_def(id) {
                    Ok(def) => {
                        let name = string_table.get(def.name_id);
                        let name = name.as_ref().map(|n| n.to_str());
                        let name = name.as_deref().unwrap_or("<unknown>");
                        format!("{depth}: {name} - {:?}", def.kind)
                    }
                    Err(_) => format!("{depth}: <unknown> - {id:?}"),
                })
                .collect::<Vec<_>>()
                .join("\n");
            warn!("QuakeC backtrace:\n{frames}");
        }
    }

    pub fn current_function(&self) -> FunctionId {
        self.current_function
    }

    #[cfg(test)]
    pub(crate) fn pc(&self) -> usize {
        self.pc
    }

    /// Should only be used to reset the current function if progs are called (e.g. in
    /// `builtin_walk_move`).
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

    /// Enters a QuakeC function, saving its locals and copying its arguments into place.
    ///
    /// On error, the VM state (call stack, local stack and globals) is left unchanged.
    pub fn enter_function(
        &mut self,
        string_table: &StringTable,
        globals: &mut Globals,
        f: FunctionId,
    ) -> Result<()> {
        let def = self.functions.get_def(f)?;
        debug!("Calling QuakeC function {}", string_table.get(def.name_id).unwrap_or_default());

        let FunctionKind::QuakeC(entry_pc) = def.kind else {
            return Err(ProgsError::with_msg(
                "built-in functions should not be called with enter_function()",
            ));
        };

        // Check call stack overflow
        // TODO: Why `>=` here?
        if self.call_stack.len() + 1 >= MAX_CALL_STACK_DEPTH {
            return Err(ProgsError::CallStackOverflow { backtrace: Backtrace::capture() });
        }

        // preemptively check local stack overflow
        if self.local_stack.len() + def.locals > MAX_LOCAL_STACK_DEPTH {
            return Err(ProgsError::LocalStackOverflow { backtrace: Backtrace::capture() });
        }

        // save locals to stack
        let local_stack_len = self.local_stack.len();
        let saved = (0..def.locals).try_for_each(|i| {
            self.local_stack.push(globals.get_bytes((def.arg_start + i) as i16)?);
            Ok::<_, GlobalsError>(())
        });
        if let Err(e) = saved {
            self.local_stack.truncate(local_stack_len);
            return Err(e.into());
        }

        // copy arguments into locals
        let mut dest = def.arg_start;
        let copied =
            def.argsz[..def.argc.min(MAX_ARGS)].iter().enumerate().try_for_each(|(arg, &argsz)| {
                for component in 0..argsz as usize {
                    let val =
                        globals.get_bytes((GLOBAL_ADDR_ARG_0 + arg * 3 + component) as i16)?;
                    globals.put_bytes(val, dest as i16)?;
                    dest += 1;
                }
                Ok::<_, GlobalsError>(())
            });
        if let Err(e) = copied {
            // Restore the locals we may have partially overwritten.
            for (i, val) in self.local_stack.drain(local_stack_len..).enumerate() {
                let _ = globals.put_bytes(val, (def.arg_start + i) as i16);
            }
            return Err(e.into());
        }

        // save stack frame
        self.call_stack.push(StackFrame { instr_id: self.pc, func_id: self.current_function });
        self.current_function = f;
        self.pc = entry_pc;

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
            string_table.get(def.name_id).unwrap_or_default()
        );

        for i in (0..def.locals).rev() {
            globals.put_bytes(
                self.local_stack
                    .pop()
                    .ok_or_else(|| ProgsError::with_msg("local stack underflow"))?,
                (def.arg_start + i) as i16,
            )?;
        }

        let frame =
            self.call_stack.pop().ok_or_else(|| ProgsError::with_msg("call stack underflow"))?;

        self.current_function = frame.func_id;
        self.pc = frame.instr_id;

        Ok(())
    }

    /// Pops call frames until the call stack is `depth` frames deep, restoring each frame's
    /// locals exactly as [`ExecutionContext::leave_function`] does.
    ///
    /// This is used to clean up after a QuakeC error, so that the globals don't keep the locals
    /// of the function that failed.
    pub fn unwind_to(
        &mut self,
        string_table: &StringTable,
        globals: &mut Globals,
        depth: usize,
    ) -> Result<()> {
        while self.call_stack.len() > depth {
            self.leave_function(string_table, globals)?;
        }

        Ok(())
    }

    pub fn load_statement(&self) -> Result<Statement> {
        self.functions.statements.get(self.pc).cloned().ok_or_else(|| {
            ProgsError::with_msg(format!("program counter out of range ({})", self.pc))
        })
    }

    /// Performs an unconditional relative jump.
    pub fn jump_relative(&mut self, rel: i16) {
        self.pc = (self.pc as isize + rel as isize) as usize;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn def(arg_start: usize, locals: usize, argc: usize) -> FunctionDef {
        let mut argsz = [0; MAX_ARGS];
        argsz[..argc].fill(1);
        FunctionDef {
            kind: FunctionKind::QuakeC(0),
            arg_start,
            locals,
            name_id: StringId(0),
            srcfile_id: StringId(0),
            argc,
            argsz,
        }
    }

    fn setup() -> (ExecutionContext, Globals, StringTable) {
        let functions = Functions {
            defs: vec![def(0, 0, 0), def(70, 2, 1), def(72, 1, 0), def(99, 2, 0)]
                .into_boxed_slice(),
            statements: Box::new([]),
        };
        let mut globals = Globals::new(Vec::new(), vec![[0; 4]; 100].into_boxed_slice());
        globals.put_bytes([1; 4], 70).unwrap();
        globals.put_bytes([2; 4], 71).unwrap();
        globals.put_bytes([3; 4], 72).unwrap();
        globals.put_bytes([9; 4], GLOBAL_ADDR_ARG_0 as i16).unwrap();
        (ExecutionContext::create(functions), globals, StringTable::new(vec![0]))
    }

    #[test]
    fn unwind_restores_locals() {
        let (mut cx, mut globals, strs) = setup();

        cx.enter_function(&strs, &mut globals, FunctionId(1)).unwrap();
        // The argument was copied into the first local.
        assert_eq!(globals.get_bytes(70).unwrap(), [9; 4]);
        cx.enter_function(&strs, &mut globals, FunctionId(2)).unwrap();
        assert_eq!(cx.call_stack_depth(), 2);

        // Simulate the callees clobbering their locals before an error.
        for addr in 70..73 {
            globals.put_bytes([0xff; 4], addr).unwrap();
        }

        cx.unwind_to(&strs, &mut globals, 0).unwrap();

        assert_eq!(cx.call_stack_depth(), 0);
        assert_eq!(cx.current_function(), FunctionId(0));
        assert_eq!(globals.get_bytes(70).unwrap(), [1; 4]);
        assert_eq!(globals.get_bytes(71).unwrap(), [2; 4]);
        assert_eq!(globals.get_bytes(72).unwrap(), [3; 4]);
    }

    #[test]
    fn failed_enter_leaves_state_unchanged() {
        let (mut cx, mut globals, strs) = setup();

        cx.enter_function(&strs, &mut globals, FunctionId(1)).unwrap();
        // Function 3's locals run off the end of the globals.
        assert!(cx.enter_function(&strs, &mut globals, FunctionId(3)).is_err());
        assert_eq!(cx.call_stack_depth(), 1);
        assert_eq!(cx.current_function(), FunctionId(1));

        // Unwinding still restores function 1's locals correctly.
        cx.unwind_to(&strs, &mut globals, 0).unwrap();
        assert_eq!(globals.get_bytes(70).unwrap(), [1; 4]);
        assert_eq!(globals.get_bytes(71).unwrap(), [2; 4]);
    }
}
