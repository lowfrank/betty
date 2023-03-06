use std::convert::From;
use std::fmt;
use std::path::PathBuf;

use super::object::Object;
use super::token::TokenKind;
use super::type_alias::Line;

const DECODING_ERROR_CHARACTER: char = char::REPLACEMENT_CHARACTER;

#[derive(Default, Clone, Debug)]
pub struct Ctx {
    pub filename: Option<PathBuf>,
    pub fun_name: Option<String>,
    pub line: Line,
    pub parent: Option<Box<Ctx>>,
}

impl Ctx {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_filename(&mut self, filename: Option<PathBuf>) -> Self {
        self.filename = filename;
        self.clone()
    }

    pub fn set_fun_name(&mut self, fun_name: Option<String>) -> Self {
        self.fun_name = fun_name;
        self.clone()
    }

    pub fn set_line(&mut self, line: Line) -> Self {
        self.line = line;
        self.clone()
    }

    pub fn set_parent(&mut self, parent: Ctx) -> Self {
        self.parent = Some(Box::new(parent));
        self.clone()
    }

    pub fn display_filename(&self) -> String {
        match self.filename.as_ref() {
            Some(filename) => filename
                .canonicalize()
                .unwrap_or_else(|_| PathBuf::from(DECODING_ERROR_CHARACTER.to_string()))
                .display()
                .to_string(),
            None => "<repl>".into(),
        }
    }

    pub fn build_report(&self, tail: String) -> String {
        let mut report = tail;
        report = self.push_report_stack(report);
        let mut ctx = self.clone();

        while let Some(parent) = ctx.parent {
            ctx = *parent;
            report = ctx.push_report_stack(report);
        }
        format!("Traceback (most recent call last):{}", report)
    }

    fn push_report_stack(&self, old: String) -> String {
        let mut buf = format!(
            "\n    In file {}, line {}",
            self.display_filename(),
            self.line,
        );
        if let Some(ref fun_name) = self.fun_name {
            buf = format!("{}, function {}", buf, fun_name);
        }
        format!("{}{}", buf, old)
    }
}

pub struct BuiltinErrors;
impl BuiltinErrors {
    pub const VALUE: &'static str = "ValueError";
    pub const TYPE: &'static str = "TypeError";
    pub const UNKNOWN_IDENTIFIER: &'static str = "UnknownIdentifierError";
    pub const OVERFLOW: &'static str = "OverflowError";
    pub const DIVISION_BY_ZERO: &'static str = "DivisionByZeroError";
    pub const INDEX_OUT_OF_BOUNDS: &'static str = "IndexOutOfBoundsError";
    pub const WRONG_ARGUMENTS_NUMBER: &'static str = "WrongArgumentsNumberError";
    pub const FILE_IO: &'static str = "FileIOError";
    pub const ASSERTION: &'static str = "AssertionError";
    pub const VECTOR_MUTATION: &'static str = "VectorMutationError";
    pub const MODULE_IMPORT: &'static str = "ModuleImportError";
    const NAMES: [&'static str; 11] = [
        Self::VALUE,
        Self::TYPE,
        Self::UNKNOWN_IDENTIFIER,
        Self::OVERFLOW,
        Self::DIVISION_BY_ZERO,
        Self::WRONG_ARGUMENTS_NUMBER,
        Self::INDEX_OUT_OF_BOUNDS,
        Self::FILE_IO,
        Self::ASSERTION,
        Self::VECTOR_MUTATION,
        Self::MODULE_IMPORT,
    ];
}

impl fmt::Display for BuiltinErrors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            Self::NAMES
                .iter()
                .map(|name| format!("'{}'", name))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Eq)]
pub enum ErrorKind {
    Syntax, // Uncatchable
    Value,
    Type,
    UnknownIdentifier,
    Overflow,
    DivisionByZero,
    IndexOutOfBounds,
    WrongArgumentsNumber,
    FileIO,
    Assertion,
    VectorMutation,
    ModuleImport,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}Error", self)
    }
}

impl TryFrom<String> for ErrorKind {
    type Error = (ErrorKind, String);

    fn try_from(string: String) -> Result<Self, Self::Error> {
        match string.as_str() {
            BuiltinErrors::VALUE => Ok(Self::Value),
            BuiltinErrors::TYPE => Ok(Self::Type),
            BuiltinErrors::UNKNOWN_IDENTIFIER => Ok(Self::UnknownIdentifier),
            BuiltinErrors::OVERFLOW => Ok(Self::Overflow),
            BuiltinErrors::DIVISION_BY_ZERO => Ok(Self::DivisionByZero),
            BuiltinErrors::WRONG_ARGUMENTS_NUMBER => Ok(Self::WrongArgumentsNumber),
            BuiltinErrors::INDEX_OUT_OF_BOUNDS => Ok(Self::IndexOutOfBounds),
            BuiltinErrors::FILE_IO => Ok(Self::FileIO),
            BuiltinErrors::ASSERTION => Ok(Self::Assertion),
            BuiltinErrors::VECTOR_MUTATION => Ok(Self::VectorMutation),
            BuiltinErrors::MODULE_IMPORT => Ok(Self::ModuleImport),
            _ => Err((
                ErrorKind::Syntax,
                format!("Expected one of {}, got '{}'", BuiltinErrors, string),
            )),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Error {
    pub kind: ErrorKind,
    msg: String,
    pub ctx: Ctx,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.make_report())
    }
}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.msg == other.msg // TODO: What about ctx?
    }
}

impl From<(CFError, Ctx)> for Error {
    fn from(args: (CFError, Ctx)) -> Self {
        let (e, ctx) = args;
        Self {
            kind: e.0,
            msg: e.1,
            ctx,
        }
    }
}

impl Error {
    pub fn new(kind: ErrorKind, msg: impl Into<String>, ctx: Ctx) -> Self {
        Self {
            kind,
            msg: msg.into(),
            ctx,
        }
    }

    pub fn syntax(msg: impl Into<String>, ctx: Ctx) -> Self {
        Self::new(ErrorKind::Syntax, msg, ctx)
    }

    pub fn value(msg: impl Into<String>, ctx: Ctx) -> Self {
        Self::new(ErrorKind::Value, msg, ctx)
    }

    fn make_report(&self) -> String {
        let tail = format!("\n    {}", self.short_msg());
        self.ctx.build_report(tail)
    }

    pub fn short_msg(&self) -> String {
        if self.msg.is_empty() {
            format!("{}", self.kind)
        } else {
            format!("{}: {}", self.kind, self.msg)
        }
    }

    pub fn traceback(&self) -> String {
        self.make_report()
    }
}

pub struct CFError(pub ErrorKind, pub String);

pub fn invalid_op_err_msg(left: Object, op: TokenKind, right: Object) -> String {
    format!(
        "Cannot apply {} to {} and {}",
        op,
        left.kind(),
        right.kind()
    )
}

pub fn expected_value_err_msg(
    what: impl Into<String>,
    nth: u8,
    fn_name: &str,
    typ: impl Into<String>,
) -> String {
    format!(
        "Expected {} as argument n. {} of builtin function '{}', got {}",
        what.into(),
        nth,
        fn_name,
        typ.into()
    )
}
