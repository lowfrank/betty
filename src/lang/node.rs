//! The [`Node`] struct represents a component of the AST.

use std::path::PathBuf;

use super::error::ErrorKind;
use super::token::TokenKind;
use super::typ::Type;
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
        branches: Vec<(Option<Vec<Node>>, Option<Node>, Vec<Node>)>,
    },
    Try {
        try_nodes: Vec<Node>,
        catch_blocks: CatchBlocks,
        else_nodes: Option<Vec<Node>>,
    },
    ConditionalExpr {
        condition: Box<Node>,
        true_node: Box<Node>,
        false_node: Box<Node>,
    },
    Throw {
        err_kind: ErrorKind,
        err_msg: Box<Option<Node>>,
    },
    Type {
        inner: Type,
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
}

/// The [`CatchBlocks`] struct handles the catch statements
#[derive(Clone, PartialEq, Debug, Default)]
pub struct CatchBlocks {
    pub err_kinds: Vec<Option<Vec<ErrorKind>>>,
    pub err_aliases: Vec<Option<String>>,

    // Multiple catch nodes, each catch node has its own statements
    pub catch_nodes: Vec<Vec<Node>>,
}

impl CatchBlocks {
    pub fn new() -> Self {
        Default::default()
    }

    /// Push a new catch block at the end of the stack
    pub fn push_block(
        &mut self,
        err_kinds: Option<Vec<ErrorKind>>,
        err_alias: Option<String>,
        nodes: Vec<Node>,
    ) {
        self.err_kinds.push(err_kinds);
        self.err_aliases.push(err_alias);
        self.catch_nodes.push(nodes);
    }

    /// Get the i-th catch block
    fn get(&mut self, i: usize) -> Option<(Option<String>, Vec<Node>)> {
        Some((self.err_aliases.remove(i), self.catch_nodes.remove(i)))
    }

    /// Try tp catch 'err_kind' by iterating over its errors. If it contains the error,
    /// it is caught??? I can't remember it now
    pub fn try_catch(&mut self, err_kind: ErrorKind) -> Option<(Option<String>, Vec<Node>)> {
        for (i, err_kinds) in self.err_kinds.iter().enumerate() {
            let Some(err_kinds) = err_kinds else {
                return self.get(i);
            };
            if err_kinds.contains(&err_kind) {
                return self.get(i);
            }
        }
        None
    }
}
