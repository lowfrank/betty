use std::collections::VecDeque;

use super::error::{CFError, Error};
use super::node::Node;
use super::object::Object;
use super::token::Token;

pub type Int = i64;
pub type Float = f64;
pub type Line = u64;
pub type LexerResult = Result<Token, Error>;
pub type ParserResult = Result<Node, Error>;
pub type ParserResults = Result<Vec<Node>, Error>;
pub type InterpreterResult = Result<Object, Error>;
pub type CFResult = Result<Object, CFError>;
pub type FunArgs = VecDeque<Object>;
