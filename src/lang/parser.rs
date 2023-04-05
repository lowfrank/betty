use std::collections::{HashSet, VecDeque};
use std::path::PathBuf;

use super::error::{BuiltinErrors, Ctx, Error, ErrorKind};
use super::node::{CatchBlocks, Node, NodeKind};
use super::run::BettyFile;
use super::token::{Token, TokenKind};
use super::typ::Type;
use super::type_alias::{ParserResult, ParserResults};

const ASSIGN_KIND: [TokenKind; 7] = [
    TokenKind::Assign,
    TokenKind::ReassignPlus,
    TokenKind::ReassignMinus,
    TokenKind::ReassignMul,
    TokenKind::ReassignDiv,
    TokenKind::ReassignPow,
    TokenKind::ReassignMod,
];

pub struct Parser {
    tokens: VecDeque<Token>,
    current_token: Token,
    nested_fun: u64,
    nested_loop: u64,
    ctx: Ctx,
}

impl Parser {
    pub fn new(mut tokens: VecDeque<Token>) -> Self {
        let current_token = tokens.pop_front().unwrap(); // There must be at least one token
        Self {
            tokens,
            current_token,
            nested_fun: 0,
            nested_loop: 0,
            ctx: Ctx::new(),
        }
    }

    pub fn filename(mut self, filename: PathBuf) -> Self {
        self.ctx.filename = Some(filename);
        self
    }

    #[inline]
    fn advance(&mut self) {
        self.current_token = self.tokens.pop_front().unwrap();
    }

    #[inline]
    fn peek_token_kind(&self) -> Option<&TokenKind> {
        self.tokens.get(0).map(|token| &token.kind)
    }

    #[inline]
    fn skip_newlines(&mut self) -> bool {
        let mut newlines = false;
        while TokenKind::Newline == self.current_token.kind {
            self.advance();
            newlines = true;
        }
        newlines
    }

    fn nodes(&mut self, end_tokens: &[TokenKind]) -> ParserResults {
        let mut nodes = Vec::new();
        self.skip_newlines();
        nodes.push(self.node()?);
        // as long as there are newlines and as long as there is no 'end' kw, we loop
        while self.skip_newlines() && !end_tokens.contains(&self.current_token.kind) {
            nodes.push(self.node()?);
        }
        Ok(nodes)
    }

    fn node(&mut self) -> ParserResult {
        match (self.current_token.kind.clone(), self.peek_token_kind()) {
            (TokenKind::Ident(ident), Some(op))
                if ASSIGN_KIND.contains(op) =>
            {
                self.assign_expr(ident)
            }
            (TokenKind::KwIF, _) => self.if_node(),
            (TokenKind::KwFOR, _) => self.for_node(),
            (TokenKind::KwFOREACH, _) => self.foreach_node(),
            (TokenKind::KwWHILE, _) => self.while_node(),
            (TokenKind::KwFUN, _) => self.fun_node(),
            (TokenKind::KwRETURN, _) => self.return_node(),
            (TokenKind::KwBREAK, _) => self.break_node(),
            (TokenKind::KwCONTINUE, _) => self.continue_node(),
            (TokenKind::KwMATCH, _) => self.match_node(),
            (TokenKind::KwTRY, _) => self.try_node(),
            (TokenKind::KwTHROW, _) => self.throw_node(),
            (TokenKind::KwUSING, _) => self.using_node(),
            _ => self.expr(),
        }
    }

    fn assign_expr(&mut self, ident: String) -> ParserResult {
        let line = self.current_token.line;
        self.advance(); // skip ident
        let op = self.current_token.kind.clone();
        self.advance();
        self.skip_newlines();
        let expr = self.expr()?;
        Ok(Node::new(NodeKind::Assign {
            ident, op, expr: Box::new(expr),
        }, line))
    }

    fn expr(&mut self) -> ParserResult {
        let line = self.current_token.line;
        let expr = self.bin_op(
            Parser::comparison_expr,
            Parser::comparison_expr,
            &[TokenKind::KwAND, TokenKind::KwOR],
        )?;
        if self.current_token.kind != TokenKind::Question {
            return Ok(expr);
        }

        self.advance(); // skip '?'
        self.skip_newlines();
        let true_expr = self.expr()?;
        self.skip_newlines();
        self.check_token_kind(
            TokenKind::KwELSE,
            format!("Expected {} in conditional expression", TokenKind::KwELSE),
        )?;
        self.skip_newlines();
        let false_expr = self.expr()?;
        Ok(Node::new(
            NodeKind::ConditionalExpr {
                condition: Box::new(expr),
                true_node: Box::new(true_expr),
                false_node: Box::new(false_expr),
            },
            line,
        ))
    }

    fn comparison_expr(&mut self) -> ParserResult {
        match self.current_token.kind {
            TokenKind::KwNOT => {
                let line = self.current_token.line;
                self.advance();
                let expr = self.comparison_expr()?;
                Ok(Node::new(
                    NodeKind::UnaryOp {
                        op: TokenKind::KwNOT,
                        right: Box::new(expr),
                    },
                    line,
                ))
            }
            _ => self.bin_op(
                Parser::plus_minus_expr,
                Parser::plus_minus_expr,
                &[
                    TokenKind::Eq,
                    TokenKind::Neq,
                    TokenKind::Gt,
                    TokenKind::Ge,
                    TokenKind::Lt,
                    TokenKind::Le,
                    TokenKind::KwIN,
                ],
            ),
        }
    }

    fn plus_minus_expr(&mut self) -> ParserResult {
        self.bin_op(
            Parser::mul_div_mod_expr,
            Parser::plus_minus_expr,
            &[TokenKind::Plus, TokenKind::Minus],
        )
    }

    fn mul_div_mod_expr(&mut self) -> ParserResult {
        self.bin_op(
            Parser::unary_expr,
            Parser::unary_expr,
            &[TokenKind::Mul, TokenKind::Div, TokenKind::Rem],
        )
    }

    fn unary_expr(&mut self) -> ParserResult {
        match self.current_token.kind.clone() {
            op @ (TokenKind::Plus | TokenKind::Minus) => {
                let line = self.current_token.line;
                self.advance();
                let unary_expr = self.unary_expr()?;
                Ok(Node::new(
                    NodeKind::UnaryOp {
                        op,
                        right: Box::new(unary_expr),
                    },
                    line,
                ))
            }
            _ => self.power_expr(),
        }
    }

    fn power_expr(&mut self) -> ParserResult {
        self.bin_op(Parser::call_expr, Parser::unary_expr, &[TokenKind::Pow])
    }

    fn call_expr(&mut self) -> ParserResult {
        let line = self.current_token.line;
        let expr = self.atom_expr()?;
        if self.current_token.kind != TokenKind::LeftRoundBracket {
            return Ok(expr);
        }
        self.advance(); // skip '('
        self.skip_newlines();
        let mut args = Vec::new();
        while self.current_token.kind != TokenKind::RightRoundBracket {
            args.push(self.expr()?);
            match self.current_token.kind {
                TokenKind::Comma => {
                    self.advance();
                    self.skip_newlines();
                }
                TokenKind::RightRoundBracket => {} // will exit the while loop,
                TokenKind::Newline
                    if self.peek_token_kind() == Some(&TokenKind::RightRoundBracket) =>
                {
                    self.advance()
                }
                _ => {
                    return Err(Error::syntax(
                        format!(
                            "Expected {} or {}, got {}",
                            TokenKind::Comma,
                            TokenKind::RightRoundBracket,
                            self.current_token
                        ),
                        self.ctx.set_line(self.current_token.line),
                    ))
                }
            }
        }
        self.advance(); // skip ')'
        Ok(Node::new(
            NodeKind::Call {
                callable: Box::new(expr),
                args,
            },
            line,
        ))
    }

    fn atom_expr(&mut self) -> ParserResult {
        let line = self.current_token.line;
        match self.current_token.kind.clone() {
            TokenKind::Int(num) => {
                self.advance();
                Ok(Node::new(NodeKind::Int { num }, line))
            }
            TokenKind::Float(num) => {
                self.advance();
                Ok(Node::new(NodeKind::Float { num }, line))
            }
            TokenKind::String(string) => {
                self.advance();
                Ok(Node::new(NodeKind::String { string }, line))
            }
            TokenKind::LeftSquareBracket => self.vector_node(),
            TokenKind::Ident(ident) => self.ident_node(ident),
            TokenKind::True => {
                self.advance();
                Ok(Node::new(NodeKind::True, line))
            }
            TokenKind::False => {
                self.advance();
                Ok(Node::new(NodeKind::False, line))
            }
            TokenKind::Nothing => {
                self.advance();
                Ok(Node::new(NodeKind::Nothing, line))
            }
            TokenKind::LeftRoundBracket => self.round_brackets_expr(),
            TokenKind::KwFUN => {
                // We MUST skip here because otherwise the combination of named function and
                // nameless function (stmt vs expr) is messed up (we can call self.fun_node
                // both as a statement (named) and as expression (nameless), but here
                // we only want to call an anonymous one, which due to the design of self.fun_node
                // and self.anonymous_fun, does not skip the keyword 'fun'. Therefore, it MUST
                // be skipped here.
                self.advance(); // skip 'fun'
                self.anonymous_fun_node()
            }
            _ => Err(Error::syntax(
                format!("Expected expression, got {}", self.current_token),
                self.ctx.set_line(self.current_token.line),
            )),
        }
    }

    fn ident_node(&mut self, ident: String) -> ParserResult {
        let line = self.current_token.line;
        self.advance();
        match Type::try_from(ident.as_str()) {
            Ok(inner) => Ok(Node::new(NodeKind::Type { inner }, line)),
            _ => Ok(Node::new(NodeKind::Ident { ident }, line)),
        }
    }

    fn round_brackets_expr(&mut self) -> ParserResult {
        self.advance(); // skip '('
        self.skip_newlines();
        let expr = self.expr()?;
        self.skip_newlines();
        self.check_token_kind(
            TokenKind::RightRoundBracket,
            format!(
                "Expected {} at the end of parenthesised expression",
                TokenKind::RightRoundBracket
            ),
        )?;
        Ok(expr)
    }

    fn vector_node(&mut self) -> ParserResult {
        let line = self.current_token.line;
        self.advance(); // skip '['
        self.skip_newlines();
        let mut nodes = Vec::new();
        while self.current_token.kind != TokenKind::RightSquareBracket {
            /* We use a while instead of a loop because this example would break it
            [
                [1, 2 ,3],
                [4, 5 ,6],
                [7, 8, 9],
            ]
            */
            let expr = self.expr()?;
            nodes.push(expr); // push here cuz after the match we may exit the loop
            match self.current_token.kind {
                TokenKind::Comma => {
                    self.advance();
                    if self.current_token.kind == TokenKind::Newline {
                        self.skip_newlines();
                    }
                }
                TokenKind::RightSquareBracket => break,
                _ => {
                    return Err(Error::syntax(
                        format!(
                            "Expected {} or {}, got {}",
                            TokenKind::Comma,
                            TokenKind::RightSquareBracket,
                            self.current_token
                        ),
                        self.ctx.set_line(self.current_token.line),
                    ))
                }
            }
        }

        self.advance(); // skip ']'
        Ok(Node::new(NodeKind::Vector { nodes }, line))
    }

    fn if_node(&mut self) -> ParserResult {
        let line = self.current_token.line;
        let mut cases = Vec::new();
        self.advance(); // skip 'if'
        cases.push(self.get_condition_and_body()?);
        self.skip_newlines();

        // 'else if' scenario
        while self.current_token.kind == TokenKind::KwELSE {
            if Some(&TokenKind::KwIF) == self.peek_token_kind() {
                // 'else if' scenario
                self.advance(); // skip 'else'
                self.advance(); // skip 'if'
                cases.push(self.get_condition_and_body()?);
            } else {
                break;
            }
        }

        // 'else' scenario
        self.skip_newlines();
        if self.current_token.kind == TokenKind::KwELSE {
            self.advance(); // skip 'else'
            cases.push((None, self.get_body()?));
            Ok(Node::new(NodeKind::If { cases }, line))
        } else {
            // We have no 'else' block, just check for 'end' keyword to close 'if' statement
            self.check_token_kind(
                TokenKind::KwEND,
                format!("Expected {} at the end of if statement", TokenKind::KwEND),
            )?;
            Ok(Node::new(NodeKind::If { cases }, line))
        }
    }

    fn get_condition_and_body(&mut self) -> Result<(Option<Node>, Vec<Node>), Error> {
        let condition = Some(self.expr()?);
        self.check_token_kind(TokenKind::KwDO, format!("Expected {}", TokenKind::KwDO))?;
        self.skip_newlines();
        if [TokenKind::KwEND, TokenKind::KwELSE].contains(&self.current_token.kind) {
            Ok((condition, Vec::new()))
        } else {
            let expressions = self.nodes(&[TokenKind::KwEND, TokenKind::Eof, TokenKind::KwELSE])?;
            self.skip_newlines();
            Ok((condition, expressions))
        }
    }

    fn get_for_loop_ident(&mut self) -> Result<Option<String>, Error> {
        let TokenKind::Ident(ident) = self.current_token.kind.clone() else {
            return Err(Error::syntax(
                format!(
                    "Expected identifier as cycle variable of for loop, got {}",
                    self.current_token
                ),
                self.ctx.set_line(self.current_token.line),
            ));
        };

        self.advance(); // skip ident
        if ident == "_" {
            Ok(None)
        } else {
            Ok(Some(ident))
        }
    }

    fn get_for_loop_start(&mut self) -> Result<Option<Node>, Error> {
        match self.current_token.kind {
            TokenKind::Assign => {
                self.advance(); // skip 'from'
                Ok(Some(self.expr()?))
            }
            TokenKind::ThickArrow => Ok(None),
            _ => Err(Error::syntax(
                format!(
                    "Expected {} or {}, got {}",
                    TokenKind::Assign,
                    TokenKind::ThickArrow,
                    self.current_token
                ),
                self.ctx.set_line(self.current_token.line),
            )),
        }
    }

    fn get_for_loop_end(&mut self) -> ParserResult {
        match self.current_token.kind {
            TokenKind::ThickArrow => {
                self.advance(); // skip 'to'
                self.expr()
            }
            _ => Err(Error::syntax(
                format!(
                    "Expected {}, got {}",
                    TokenKind::ThickArrow,
                    self.current_token
                ),
                self.ctx.set_line(self.current_token.line),
            )),
        }
    }

    fn get_for_loop_step(&mut self) -> Result<Option<Node>, Error> {
        match self.current_token.kind {
            TokenKind::Comma => {
                self.advance();
                Ok(Some(self.expr()?))
            }
            _ => Ok(None),
        }
    }

    fn for_node(&mut self) -> ParserResult {
        let line = self.current_token.line;
        self.advance(); // skip 'for'
        let ident = self.get_for_loop_ident()?;
        let start = self.get_for_loop_start()?;
        let end = self.get_for_loop_end()?;
        let step = self.get_for_loop_step()?;
        let body = self.get_loop_body()?;
        Ok(Node::new(
            NodeKind::For {
                ident,
                start: Box::new(start),
                end: Box::new(end),
                step: Box::new(step),
                body,
            },
            line,
        ))
    }

    fn foreach_node(&mut self) -> ParserResult {
        let line = self.current_token.line;
        self.advance(); // skip 'foreach'
        let ident = self.get_for_loop_ident()?;
        self.check_token_kind(
            TokenKind::KwIN,
            format!(
                "Expected {} after identifier in foreach loop",
                TokenKind::KwIN
            ),
        )?;
        let iterable = self.expr()?;
        let body = self.get_loop_body()?;
        Ok(Node::new(
            NodeKind::ForEach {
                ident,
                iterable: Box::new(iterable),
                body,
            },
            line,
        ))
    }

    fn while_node(&mut self) -> ParserResult {
        let line = self.current_token.line;
        self.advance(); // skip 'while'
        let condition = self.expr()?;
        let body = self.get_loop_body()?;
        let kind = if condition.kind == NodeKind::True {
            NodeKind::InfiniteLoop { body }
        } else {
            NodeKind::While {
                condition: Box::new(condition),
                body,
            }
        };
        Ok(Node::new(kind, line))
    }

    fn fun_node(&mut self) -> ParserResult {
        let line = self.current_token.line;
        self.advance(); // skip 'fun'
        if self.current_token.kind == TokenKind::LeftRoundBracket {
            return self.anonymous_fun_node();
        }
        let fun_name = match self.current_token.kind.clone() {
            TokenKind::Ident(ident) => {
                self.advance(); // skip identifier
                ident
            }
            _ => {
                return Err(Error::syntax(
                    format!(
                        "Expected identifier as function name, got {}",
                        self.current_token
                    ),
                    self.ctx.set_line(self.current_token.line),
                ))
            }
        };
        let arg_names = self.get_fun_args(&fun_name)?;
        let body = self.get_fun_body()?;
        Ok(Node::new(
            NodeKind::Fun {
                fun_name,
                arg_names,
                body,
            },
            line,
        ))
    }

    fn get_fun_args(&mut self, fun_name: &str) -> Result<Vec<String>, Error> {
        let line = self.current_token.line;
        self.check_token_kind(
            TokenKind::LeftRoundBracket,
            format!("Expected {}", TokenKind::LeftRoundBracket),
        )?;
        self.skip_newlines();
        let mut args = Vec::new();
        while self.current_token.kind != TokenKind::RightRoundBracket {
            match (self.current_token.kind.clone(), self.peek_token_kind()) {
                (TokenKind::Ident(ident), Some(TokenKind::Comma)) => {
                    self.advance();
                    self.advance();
                    self.skip_newlines();
                    args.push(ident);
                }
                (TokenKind::Ident(ident), Some(TokenKind::RightRoundBracket)) => {
                    self.advance();
                    // newlines will be skipped in the body beginning
                    args.push(ident);
                }
                /*
                    Enable both these pattern

                    (1)
                    fun test(
                        a,
                        b
                    ) do end

                    (2)
                    fun test(
                        a,
                        b,
                    ) do end

                */
                (TokenKind::Ident(ident), Some(TokenKind::Newline)) => {
                    self.advance(); // skip ident
                    self.advance(); // skip newline
                    self.skip_newlines();
                    args.push(ident);
                    if self.current_token.kind != TokenKind::RightRoundBracket {
                        return Err(Error::syntax(
                            format!(
                                "Expected {}, got {}",
                                TokenKind::RightRoundBracket,
                                self.current_token
                            ),
                            self.ctx.set_line(self.current_token.line),
                        ));
                    }
                }
                _ => {
                    return Err(Error::syntax(
                        format!(
                            "Expected identifier or {} in declaring arguments of function {}",
                            TokenKind::Comma,
                            fun_name
                        ),
                        self.ctx.set_line(self.current_token.line),
                    ))
                }
            }
        }
        self.advance(); // skip ')'

        let unique_args = args.iter().collect::<HashSet<_>>();
        if args.len() == unique_args.len() {
            return Ok(args);
        }

        let duplicates = args
            .iter()
            .filter(|name| {
                let count = args.iter().filter(|item| item == name).count();
                count > 1 // Get the arg names that occur more than once in the fun parameters
            })
            .collect::<HashSet<_>>() // Remove the string duplicates
            .into_iter()
            .map(|name| format!("'{}'", name)) // Add nice format to names
            .collect::<Vec<_>>();
        Err(Error::syntax(
            format!(
                "There are duplicate parameter(s) in function definition {}: {}",
                fun_name,
                duplicates.join(", ")
            ),
            self.ctx.set_line(line),
        ))
    }

    fn anonymous_fun_node(&mut self) -> ParserResult {
        let line_start = self.current_token.line;
        let arg_names = self.get_fun_args(&Type::AnonymousFun.to_string())?;
        match self.current_token.kind {
            TokenKind::ThickArrow => {
                self.advance();
                let expr = self.get_fun_single_expr()?;
                let line = expr.line;
                let body = Node::new(
                    NodeKind::Return {
                        expr: Box::new(expr),
                    },
                    line,
                );
                let body = vec![body];
                Ok(Node::new(
                    NodeKind::AnonymousFun { arg_names, body },
                    line_start,
                ))
            }
            TokenKind::KwDO => {
                let body = self.get_fun_body()?;
                Ok(Node::new(
                    NodeKind::AnonymousFun { arg_names, body },
                    line_start,
                ))
            }
            _ => Err(Error::syntax(
                format!(
                    "Expected {} or {} in anonymous function body, got {}",
                    TokenKind::ThickArrow,
                    TokenKind::KwDO,
                    self.current_token
                ),
                self.ctx.set_line(self.current_token.line),
            )),
        }
    }

    fn get_fun_single_expr(&mut self) -> ParserResult {
        self.nested_fun += 1; // self.fun_body() accepts multiple statements, we only want one
        let expr = self.expr();
        self.nested_fun -= 1;
        expr
    }

    fn get_fun_body(&mut self) -> ParserResults {
        self.nested_fun += 1;
        let body = self.get_body();
        self.nested_fun -= 1;
        body
    }

    fn get_loop_body(&mut self) -> ParserResults {
        self.nested_loop += 1;
        let body = self.get_body();
        self.nested_loop -= 1;
        body
    }

    fn is_inside_fun(&self) -> bool {
        self.nested_fun > 0
    }

    fn return_node(&mut self) -> ParserResult {
        let line = self.current_token.line;
        if !self.is_inside_fun() {
            return Err(Error::syntax(
                format!("{} outside function", TokenKind::KwRETURN),
                self.ctx.set_line(self.current_token.line),
            ));
        }

        self.advance(); // skip 'return'
        let expr = Box::new(self.expr()?);
        Ok(Node::new(NodeKind::Return { expr }, line))
    }

    fn is_inside_loop(&self) -> bool {
        self.nested_loop > 0
    }

    fn break_node(&mut self) -> ParserResult {
        // TODO: merge break_node and continue_node into one function?
        // But they return different NodeKind, how to do that?
        let line = self.current_token.line;
        if !self.is_inside_loop() {
            return Err(Error::syntax(
                format!("{} outside loop", TokenKind::KwBREAK),
                self.ctx.set_line(self.current_token.line),
            ));
        }
        self.advance();
        Ok(Node::new(NodeKind::Break, line))
    }

    fn continue_node(&mut self) -> ParserResult {
        let line = self.current_token.line;
        if !self.is_inside_loop() {
            return Err(Error::syntax(
                format!("{} outside loop", TokenKind::KwCONTINUE),
                self.ctx.set_line(self.current_token.line),
            ));
        }
        self.advance();
        Ok(Node::new(NodeKind::Continue, line))
    }

    fn match_node(&mut self) -> ParserResult {
        let line = self.current_token.line;
        self.advance(); // skip 'match'
        let input = self.expr()?;
        self.check_token_kind(TokenKind::KwDO, format!("Expected {}", TokenKind::KwDO))?;
        self.skip_newlines();
        let mut branches = Vec::new();

        while self.current_token.kind != TokenKind::KwEND {
            let (patterns, guard) = if self.current_token.kind == TokenKind::KwELSE {
                self.advance(); // skip 'else'
                (None, self.get_match_guard()?)
            } else {
                let (patterns, guard) = self.get_match_patterns()?;
                (Some(patterns), guard)
            };
            let nodes = self.get_match_arm()?;
            branches.push((patterns, guard, nodes));
        }

        self.advance(); // skip 'end'
        Ok(Node::new(
            NodeKind::Match {
                input: Box::new(input),
                branches,
            },
            line,
        ))
    }

    fn get_match_patterns(&mut self) -> Result<(Vec<Node>, Option<Node>), Error> {
        let mut patters = Vec::new();
        patters.push(self.expr()?);
        while self.current_token.kind == TokenKind::Comma {
            self.advance();
            self.skip_newlines();
            patters.push(self.expr()?);
            self.skip_newlines();
        }
        self.skip_newlines();
        let guard = self.get_match_guard()?;
        Ok((patters, guard))
    }

    fn get_match_guard(&mut self) -> Result<Option<Node>, Error> {
        if self.current_token.kind == TokenKind::KwIF {
            self.advance();
            Ok(Some(self.expr()?))
        } else {
            Ok(None)
        }
    }

    fn get_match_arm(&mut self) -> Result<Vec<Node>, Error> {
        self.check_token_kind(
            TokenKind::ThickArrow,
            format!(
                "Expected {} after pattern in match arm",
                TokenKind::ThickArrow
            ),
        )?;
        let nodes = if self.current_token.kind == TokenKind::KwDO {
            self.get_body()?
        } else {
            // Single line expr
            vec![self.node()?]
        };
        self.skip_newlines();
        Ok(nodes)
    }

    fn try_node(&mut self) -> ParserResult {
        let line = self.current_token.line;
        self.advance(); // skip 'try'
        self.check_token_kind(
            TokenKind::KwDO,
            format!("Expected {} after {}", TokenKind::KwDO, TokenKind::KwTRY),
        )?;
        self.skip_newlines();
        let try_nodes = if self.current_token.kind == TokenKind::KwCATCH {
            Vec::new()
        } else {
            self.nodes(&[TokenKind::KwCATCH])?
        };
        let catch_blocks = self.get_catch_blocks()?;
        let else_nodes = self.get_else_nodes()?;
        Ok(Node::new(
            NodeKind::Try {
                try_nodes,
                catch_blocks,
                else_nodes,
            },
            line,
        ))
    }

    fn get_err_name_from_ident(&mut self) -> Result<String, Error> {
        if let TokenKind::Ident(ident) = self.current_token.kind.clone() {
            self.advance();
            Ok(ident)
        } else {
            Err(Error::syntax(
                format!(
                    "Expected identifier as error name, got {}",
                    self.current_token
                ),
                self.ctx.set_line(self.current_token.line),
            ))
        }
    }

    fn get_catch_blocks(&mut self) -> Result<CatchBlocks, Error> {
        let mut catch_blocks = CatchBlocks::new();
        while self.current_token.kind == TokenKind::KwCATCH {
            self.advance();
            let err_alias = self.get_err_alias()?;
            let at_least_one_err = err_alias.is_some();
            let err_names = self.get_err_names(at_least_one_err)?;
            self.check_token_kind(TokenKind::KwDO, format!("Expected {}", TokenKind::KwDO))?;
            self.skip_newlines();
            let nodes = self.get_catch_nodes()?;
            catch_blocks.push_block(err_names, err_alias, nodes);
        }

        Ok(catch_blocks)
    }

    fn get_err_alias(&mut self) -> Result<Option<String>, Error> {
        match (self.current_token.kind.clone(), self.peek_token_kind()) {
            (TokenKind::Ident(ident), Some(TokenKind::Assign)) => {
                self.advance(); // skip ident
                self.advance(); // skip '::'
                Ok(Some(ident))
            }
            _ => Ok(None),
        }
    }

    fn get_err_names(&mut self, at_least_one_err: bool) -> Result<Option<Vec<ErrorKind>>, Error> {
        let line = self.current_token.line;
        let mut err_names = Vec::new();
        self.skip_newlines();
        while self.current_token.kind != TokenKind::KwDO {
            let err = self.get_err_name_from_ident()?;
            let err = ErrorKind::try_from(err)
                .map_err(|(kind, msg)| Error::new(kind, msg, self.ctx.set_line(line)))?;
            err_names.push(err);
            match self.current_token.kind {
                TokenKind::KwDO => {}
                TokenKind::Comma => {
                    self.advance(); // skip ','
                    self.skip_newlines();
                }
                _ => {
                    return Err(Error::syntax(
                        format!(
                            "Expected {} or {}, got {}",
                            TokenKind::KwDO,
                            TokenKind::Comma,
                            self.current_token
                        ),
                        self.ctx.set_line(self.current_token.line),
                    ))
                }
            }
        }
        if err_names.is_empty() {
            if at_least_one_err {
                Err(Error::syntax(
                    format!("Expect one of {} after error alias", BuiltinErrors),
                    self.ctx.set_line(line),
                ))
            } else {
                Ok(None)
            }
        } else {
            Ok(Some(err_names))
        }
    }

    fn get_catch_nodes(&mut self) -> ParserResults {
        let mut catch_nodes = Vec::new();
        while ![TokenKind::KwELSE, TokenKind::KwEND, TokenKind::KwCATCH]
            .contains(&self.current_token.kind)
        {
            catch_nodes.push(self.node()?);
            self.skip_newlines();
        }
        Ok(catch_nodes)
    }

    fn get_else_nodes(&mut self) -> Result<Option<Vec<Node>>, Error> {
        match self.current_token.kind {
            TokenKind::KwELSE => {
                self.advance(); // skip 'else'
                Ok(Some(self.get_body()?))
            }
            TokenKind::KwEND => {
                self.advance(); // skip 'end'
                Ok(None)
            }
            _ => unreachable!(), // Guarded by self.get_catch_statements
        }
    }

    fn throw_node(&mut self) -> ParserResult {
        let line = self.current_token.line;
        self.advance(); // skip 'throw'
        let err_name = self.get_err_name_from_ident()?;
        let err_kind = ErrorKind::try_from(err_name)
            .map_err(|(kind, msg)| Error::new(kind, msg, self.ctx.set_line(line)))?;

        let err_msg = if self.current_token.kind == TokenKind::LeftRoundBracket {
            self.advance();
            self.skip_newlines();
            let err_msg = self.expr()?;
            self.skip_newlines();
            self.check_token_kind(
                TokenKind::RightRoundBracket,
                format!("Expected {}", TokenKind::RightRoundBracket),
            )?;
            Some(err_msg)
        } else {
            None
        };
        Ok(Node::new(
            NodeKind::Throw {
                err_kind,
                err_msg: Box::new(err_msg),
            },
            line,
        ))
    }

    fn using_node(&mut self) -> ParserResult {
        let line = self.current_token.line;
        self.advance(); // skip 'using'
        self.skip_newlines();

        let relative_imports = if let TokenKind::Ident(ident) = self.current_token.kind.clone() {
            self.advance(); // skip ident
            let mut relative_imports = Vec::new();
            let alias = self.get_import_alias()?;
            relative_imports.push((ident, alias));
            while self.current_token.kind == TokenKind::Comma {
                self.advance(); // skip comma
                self.skip_newlines();
                let TokenKind::Ident(ident) = self.current_token.kind.clone() else {
                    return Err(Error::syntax(
                        format!("Expected identifier after {} in {} statement, got {}", TokenKind::Comma, TokenKind::KwUSING, self.current_token.kind), 
                        self.ctx.set_line(self.current_token.line))
                    );
                };
                self.advance();
                let alias = self.get_import_alias()?;
                relative_imports.push((ident, alias));
            }
            Some(relative_imports)
        } else {
            None
        };

        if relative_imports.is_some() {
            self.skip_newlines();
            self.check_token_kind(TokenKind::KwIN, format!("Expected {}", TokenKind::KwIN))?;
        }

        let TokenKind::String(path) = self.current_token.kind.clone() else {
            let msg = if relative_imports.is_some() {
                format!("Expected string literal after {}, got {}", TokenKind::KwIN, self.current_token)
            } else {
                format!("Expected string literal after {}, got {}", TokenKind::KwUSING, self.current_token)
            };
            return Err(Error::syntax(msg, self.ctx.set_line(line)));
        };

        self.advance();
        let mut path = PathBuf::from(path);
        if path.extension().is_none() {
            path.set_extension("betty");
        }
        let ctx = self.ctx.set_line(line);
        let nodes = BettyFile::import_module(path.clone(), ctx)?;
        Ok(Node::new(
            NodeKind::Using {
                nodes,
                path,
                relative_imports,
            },
            line,
        ))
    }

    fn get_import_alias(&mut self) -> Result<Option<String>, Error> {
        if self.current_token.kind != TokenKind::KwAS {
            return Ok(None);
        }

        self.advance(); // skip 'as'
        let TokenKind::Ident(alias) = self.current_token.kind.clone() else {
            return Err(Error::syntax(
                format!("Expected identifier after {}, got {}", TokenKind::KwAS, self.current_token), 
                self.ctx.set_line(self.current_token.line))
            );
        };

        self.advance(); // skip alias
        Ok(Some(alias))
    }

    fn check_token_kind(&mut self, kind: TokenKind, msg: String) -> Result<(), Error> {
        if self.current_token.kind != kind {
            Err(Error::syntax(
                format!("{}, got {}", msg, self.current_token),
                self.ctx.set_line(self.current_token.line),
            ))
        } else {
            self.advance(); // skip token
            Ok(())
        }
    }

    fn get_body(&mut self) -> ParserResults {
        self.skip_newlines(); // idk if this is needed
        self.check_token_kind(TokenKind::KwDO, format!("Expected {}", TokenKind::KwDO))?;
        self.skip_newlines();
        if self.current_token.kind == TokenKind::KwEND {
            self.advance(); // skip 'end'
            Ok(Vec::new())
        } else {
            let body = self.nodes(&[TokenKind::KwEND, TokenKind::Eof])?;
            self.check_token_kind(TokenKind::KwEND, format!("Expected {}", TokenKind::KwEND))?;
            Ok(body)
        }
    }

    fn bin_op(
        &mut self,
        left: fn(&mut Parser) -> ParserResult,
        right: fn(&mut Parser) -> ParserResult,
        ops: &[TokenKind],
    ) -> ParserResult {
        let line = self.current_token.line;
        let mut left = left(self)?;
        while ops.contains(&self.current_token.kind) {
            let op = self.current_token.kind.clone();
            self.advance(); // skip op
            self.skip_newlines(); // Multiline bin op expressions
            let right = right(self)?;
            left = Node::new(
                NodeKind::BinOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                line,
            )
        }
        Ok(left)
    }

    pub fn parse(mut self) -> ParserResults {
        let nodes = self.nodes(&[TokenKind::Eof])?;
        if !self.tokens.is_empty() {
            Err(Error::syntax(
                format!("Invalid syntax on {}", self.current_token),
                self.ctx.set_line(self.current_token.line),
            ))
        } else {
            Ok(nodes)
        }
    }
}
