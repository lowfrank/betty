//! An [`Error`] is composed of a message, a kind ([`ErrorKind`]) and a context ([`Ctx`]),
//! which represents the traceback.

use std::convert::From;
use std::fmt;
use std::path::PathBuf;

use super::object::Object;
use super::token::TokenKind;
use super::type_alias::Line;

/// This character is used to replace filenames that are non valid UTF-8
const DECODING_ERROR_CHARACTER: char = char::REPLACEMENT_CHARACTER;

#[derive(Default, Clone, Debug)]
pub struct Ctx {
    // If there is no filename, then we are in the repl
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

    /// This function creates the full traceback and returns it as [`String`]
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

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum ErrorKind {
    Syntax, // Uncatchable, not a RuntimeError
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
    Custom(String), // User defined errors with newerror statement
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Custom(err) => write!(f, "{}", err),
            _ => write!(f, "{:?}Error", self),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub msg: Option<String>,
    pub ctx: Option<Ctx>,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.make_report())
    }
}

impl PartialEq for Error {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl From<(CFError, Ctx)> for Error {
    fn from(args: (CFError, Ctx)) -> Self {
        let (e, ctx) = args;
        // msg and ctx will never be None, because a CFError is always thrown
        // internally and therefore it must have some context associated to it
        Self {
            kind: e.0,
            msg: if e.1.is_empty() { None } else { Some(e.1) },
            ctx: Some(ctx),
        }
    }
}

impl Error {
    pub fn new(kind: ErrorKind, msg: Option<impl Into<String>>, ctx: Option<Ctx>) -> Self {
        Self {
            kind,
            msg: msg.map(Into::into),
            ctx,
        }
    }

    /// Shorthand to declare a SyntaxError
    pub fn syntax(msg: impl Into<String>, ctx: Ctx) -> Self {
        Self::new(ErrorKind::Syntax, Some(msg), Some(ctx))
    }

    /// Shorthand to declare a ValueError
    pub fn value(msg: impl Into<String>, ctx: Ctx) -> Self {
        Self::new(ErrorKind::Value, Some(msg), Some(ctx))
    }

    fn make_report(&self) -> String {
        // If there is no context, then just return the short message
        let Some(ctx) = &self.ctx else {
            return self.short_msg();
        };
        let tail = format!("\n    {}", self.short_msg());
        ctx.build_report(tail)
    }

    pub fn short_msg(&self) -> String {
        // If there is a message and it is not empty, then display it
        // Otherwise, the message type is enough
        match &self.msg {
            Some(msg) => format!("{}: {}", self.kind, msg),
            _ => format!("{}", self.kind),
        }
    }

    pub fn traceback(&self) -> String {
        self.make_report()
    }
}

/// A [`CFError`] is like an [`Error`], but it does not have the context field.
/// It is only used in situations where there is no context, like adding or
/// subtracting values, or executing a builtin function.
/// The Interpreter will take care of converting a [`CFError`] into a [`Error`],
/// because the Interpreter does have a context.
pub struct CFError(pub ErrorKind, pub String);

/// Message to display when an invalid operand has been used between left and right
pub fn invalid_op_err_msg(left: Object, op: TokenKind, right: Object) -> String {
    format!(
        "Cannot apply {} to {} and {}",
        op,
        left.kind(),
        right.kind()
    )
}

/// Message to display when a builtin function expected the nth argument
/// to be of a certain type, while we received a different type
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
