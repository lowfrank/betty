//! Type aliases used throughout the betty code

use std::collections::VecDeque;

use super::error::{CFError, Error};
use super::node::Node;
use super::object::Object;
use super::token::Token;

/// The default integer type
pub type Int = i64;

/// The default floating point type
pub type Float = f64;

/// How line information is stored
pub type Line = u64;

/// A type representing the result of a lexical analysis over a sequence
/// of characters
pub type LexerResult = Result<Token, Error>;

/// The result of a parser analysis over a sequence of [`Token`]s.
/// The [`Ok`] variant holds a single [`Node`].
pub type ParserResult = Result<Node, Error>;

/// The result of a parser analysis over a sequence of [`Token`]s.
/// The [`Ok`] variant holds a [`Vec`] of [`Node`].
pub type ParserResults = Result<Vec<Node>, Error>;

/// The result of an interpreter visiting a [`Node`].
pub type InterpreterResult = Result<Object, Error>;

/// CFResult stands for 'Context Free Result', because [`CFError`] does not have
/// the context information attached to it, such as [`Line`] and Ctx.
/// It is emitted by functions that are not related to the interpreter, because the
/// interpreter itself does have that information.
pub type CFResult = Result<Object, CFError>;

/// Function arguments type alias.
pub type FunArgs = VecDeque<Object>;
