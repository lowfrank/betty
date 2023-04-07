//! The [`Node`] struct represents a component of the AST.

use std::path::PathBuf;

use super::token::TokenKind;
use super::type_alias::{Float, Int, Line};

#[derive(Clone, PartialEq, Debug)]
pub struct Node {
    pub kind: NodeKind,
    pub line: Line,
}

impl Node {
    pub fn new(kind: NodeKind, line: Line) -> Self {
        Self { kind, line }
    }
}

/// The kind of a [`Node`] is a [`NodeKind`]
#[derive(Clone, PartialEq, Debug)]
pub enum NodeKind {
    Int {
        num: Int,
    },
    Float {
        num: Float,
    },
    String {
        string: String,
    },
    Vector {
        nodes: Vec<Node>,
    },
    BinOp {
        left: Box<Node>,
        op: TokenKind,
        right: Box<Node>,
    },
    UnaryOp {
        op: TokenKind,
        right: Box<Node>,
    },
    Ident {
        ident: String,
    },
    True,
    False,
    Nothing,

    // Option<Node> is the condition: if None, it's the 'else' case
    // Vec<Node> is the body that will be executed if the condition evaluates to true
    If {
        cases: Vec<(Option<Node>, Vec<Node>)>,
    },
    For {
        ident: Option<String>,
        start: Box<Option<Node>>,
        end: Box<Node>,
        step: Box<Option<Node>>,
        body: Vec<Node>,
    },
    ForEach {
        ident: Option<String>,
        iterable: Box<Node>,
        body: Vec<Node>,
    },
    While {
        condition: Box<Node>,
        body: Vec<Node>,
    },
    Fun {
        fun_name: String,
        arg_names: Vec<String>,
        body: Vec<Node>,
    },
    AnonymousFun {
        arg_names: Vec<String>,
        body: Vec<Node>,
    },
    Call {
        callable: Box<Node>,
        args: Vec<Node>,
    },
    Return {
        expr: Box<Node>,
    },
    Break,
    Continue,
    Match {
        input: Box<Node>,
        // Option<Vec<Node>> is one pattern, which can have multiple values to match to
        // If None, it is the 'else' case
        // Option<Node> is the guard
        // Vec<Node> is the body
        branches: Vec<(Option<Vec<Node>>, Option<Node>, Vec<Node>)>,
    },
    Try {
        try_nodes: Vec<Node>,
        // Option<Vec<Node>> is the sequence of catchable errors. If None, it's the 'all' case
        // Option<String> is the name alias to bind the error to
        // Vec<Node> is the body
        catch_blocks: Vec<(Option<Vec<Node>>, Option<String>, Vec<Node>)>,
        else_nodes: Option<Vec<Node>>,
    },
    ConditionalExpr {
        condition: Box<Node>,
        true_node: Box<Node>,
        false_node: Box<Node>,
    },
    Throw {
        err: String,
        msg: Box<Option<Node>>,
    },
    InfiniteLoop {
        body: Vec<Node>,
    },
    Assign {
        ident: String,
        op: TokenKind,
        expr: Box<Node>,
    },
    Using {
        nodes: Vec<Node>,
        path: PathBuf,
        relative_imports: Option<Vec<(String, Option<String>)>>,
    },
    NewError {
        ident: String,
    },
}
