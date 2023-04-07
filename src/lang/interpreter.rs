use std::cell::RefCell;
use std::collections::VecDeque;
use std::path::PathBuf;
use std::rc::Rc;

use super::error::{Ctx, Error, ErrorKind};
use super::namespace::{Namespace, NamespaceWrapper};
use super::node::{CatchBlocks, Node, NodeKind};
use super::object::{call_anonymous_function, call_builtin_function, call_function, Object};
use super::token::TokenKind;
use super::typ::Type;
use super::type_alias::{CFResult, Int, InterpreterResult, Line};

macro_rules! check_state {
    ($self:ident, $return_expr:expr) => {{
        if let Some(state) = $self.state {
            match state {
                State::Return => return Ok($return_expr),
                State::Break => {
                    $self.state = None;
                    break;
                }
                State::Continue => $self.state = None,
            }
        }
    }};
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum State {
    Return,
    Continue,
    Break,
}

#[derive(Debug)]
pub struct Interpreter {
    pub namespace: Rc<RefCell<Namespace>>,
    pub state: Option<State>,
    pub ctx: Ctx,
}

impl Interpreter {
    #[inline]
    pub fn new(namespace: Rc<RefCell<Namespace>>) -> Self {
        Self {
            namespace,
            state: None,
            ctx: Ctx::new(),
        }
    }

    #[inline]
    pub fn filename(mut self, filename: PathBuf) -> Self {
        self.ctx.filename = Some(filename);
        self
    }

    #[inline]
    pub fn fun_name(mut self, fun_name: String) -> Self {
        self.ctx.fun_name = Some(fun_name);
        self
    }

    #[inline]
    pub fn parent(mut self, parent: Box<Ctx>) -> Self {
        self.ctx.parent = Some(parent);
        self
    }

    pub fn repl() -> Self {
        Self {
            namespace: Namespace::new(),
            state: None,
            ctx: Ctx::new(),
        }
    }

    #[inline]
    pub fn insert_args(mut self, args: Option<Vec<String>>) -> Self {
        let mut args = args.unwrap_or_default();

        // Insert filename as first argument of args
        args.insert(0, self.ctx.display_filename());
        let args = args.into_iter().map(Object::String).collect::<Vec<_>>();
        self.namespace
            .add("argv", Object::Vector(Rc::new(RefCell::new(args))));
        self
    }

    #[inline]
    pub fn visit_multiple(&mut self, nodes: Vec<Node>) -> InterpreterResult {
        for node in nodes {
            let expr = self.visit(node)?;

            if let Some(state) = self.state {
                match state {
                    State::Return => return Ok(expr),
                    State::Continue => break,
                    State::Break => break,
                }
            }
        }
        Ok(Object::Nothing)
    }

    #[inline]
    pub fn visit(&mut self, node: Node) -> InterpreterResult {
        match node.kind {
            NodeKind::Int { num } => Ok(Object::Int(num)),
            NodeKind::Float { num } => Ok(Object::Float(num)),
            NodeKind::String { string } => Ok(Object::String(string)),
            NodeKind::True => Ok(Object::Bool(true)),
            NodeKind::False => Ok(Object::Bool(false)),
            NodeKind::Nothing => Ok(Object::Nothing),
            NodeKind::AnonymousFun { arg_names, body } => Ok(Object::AnonymousFun(arg_names, body)),
            NodeKind::Fun {
                fun_name,
                arg_names,
                body,
            } => self.visit_fun_node(fun_name, arg_names, body),
            NodeKind::Vector { nodes } => self.visit_vector_node(nodes),
            NodeKind::BinOp { left, op, right } => {
                self.visit_bin_op_node(*left, op, *right, node.line)
            }
            NodeKind::UnaryOp { op, right } => self.visit_unary_op_node(op, *right, node.line),
            NodeKind::Ident { ident } => self.visit_ident_node(ident, node.line),
            NodeKind::Assign { ident, op, expr } => {
                self.visit_assign_node(ident, op, *expr, node.line)
            }
            NodeKind::If { cases } => self.visit_if_node(cases, node.line),
            NodeKind::While { condition, body } => {
                self.visit_while_node(*condition, body, node.line)
            }
            NodeKind::For {
                ident,
                start,
                end,
                step,
                body,
            } => self.visit_for_node(ident, *start, *end, *step, body),
            NodeKind::ForEach {
                ident,
                iterable,
                body,
            } => self.visit_foreach_node(ident, *iterable, body, node.line),
            NodeKind::Call { callable, args } => self.visit_call_node(*callable, args, node.line),
            NodeKind::Return { expr } => self.visit_return_node(*expr),
            NodeKind::Continue => self.set_state(State::Continue),
            NodeKind::Break => self.set_state(State::Break),
            NodeKind::Match { input, branches } => self.visit_match_node(*input, branches),
            NodeKind::Try {
                try_nodes,
                catch_blocks,
                else_nodes,
            } => self.visit_try_node(try_nodes, catch_blocks, else_nodes),
            NodeKind::ConditionalExpr {
                condition,
                true_node,
                false_node,
            } => self.visit_conditional_expr(*condition, *true_node, *false_node, node.line),
            NodeKind::Throw { err_kind, err_msg } => {
                self.visit_throw_node(err_kind, *err_msg, node.line)
            }
            NodeKind::InfiniteLoop { body } => self.visit_infinite_loop(body),
            NodeKind::Using {
                nodes,
                path,
                relative_imports,
            } => self.visit_using_node(nodes, path, relative_imports, node.line),
        }
    }

    #[inline]
    fn set_state(&mut self, state: State) -> InterpreterResult {
        self.state = Some(state);
        Ok(Object::Nothing)
    }

    #[inline]
    fn visit_fun_node(
        &mut self,
        name: String,
        args: Vec<String>,
        body: Vec<Node>,
    ) -> InterpreterResult {
        self.namespace
            .add(name.clone(), Object::Fun(name, args, body));
        Ok(Object::Nothing)
    }

    #[inline]
    fn visit_vector_node(&mut self, nodes: Vec<Node>) -> InterpreterResult {
        let v = nodes
            .into_iter()
            .map(|node| self.visit(node))
            .collect::<Result<Vec<Object>, Error>>()?;

        Ok(Object::Vector(Rc::new(RefCell::new(v))))
    }

    #[inline]
    fn visit_bin_op_node(
        &mut self,
        left: Node,
        op: TokenKind,
        right: Node,
        line: Line,
    ) -> InterpreterResult {
        let left = self.visit(left)?;
        let right = self.visit(right)?;
        let result = self.get_bin_op_result(left, op, right);
        result.map_err(|err| Error::from((err, self.ctx.set_line(line))))
    }

    #[inline]
    fn get_bin_op_result(&mut self, left: Object, op: TokenKind, right: Object) -> CFResult {
        match op {
            TokenKind::Plus => left.add(right),
            TokenKind::Minus => left.sub(right),
            TokenKind::Mul => left.mul(right),
            TokenKind::Div => left.div(right),
            TokenKind::Pow => left.pow(right),
            TokenKind::Rem => left.rem(right),
            TokenKind::Eq => Ok(Object::Bool(left == right)),
            TokenKind::Neq => Ok(Object::Bool(left != right)),
            TokenKind::Gt => left.gt(right),
            TokenKind::Ge => left.ge(right),
            TokenKind::Lt => left.lt(right),
            TokenKind::Le => left.le(right),
            TokenKind::KwAND => left.and(right),
            TokenKind::KwOR => left.or(right),
            TokenKind::KwIN => left.contains(right),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn visit_unary_op_node(&mut self, op: TokenKind, right: Node, line: Line) -> InterpreterResult {
        let right = self.visit(right)?;
        let result = self.get_unary_op_result(op, right);
        result.map_err(|err| Error::from((err, self.ctx.set_line(line))))
    }

    #[inline]
    fn get_unary_op_result(&mut self, op: TokenKind, right: Object) -> CFResult {
        match op {
            TokenKind::Plus => right.unary_plus(),
            TokenKind::Minus => right.unary_minus(),
            TokenKind::KwNOT => right.not(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn visit_ident_node(&mut self, ident: String, line: Line) -> InterpreterResult {
        self.namespace
            .get(&ident)
            .map_err(|err| Error::from((err, self.ctx.set_line(line))))
    }

    #[inline]
    fn visit_assign_node(
        &mut self,
        ident: String,
        op: TokenKind,
        expr: Node,
        line: Line,
    ) -> InterpreterResult {
        let result = self.visit(expr)?;

        if op == TokenKind::Assign {
            self.namespace.add(ident, result);
        } else {
            let old = self
                .namespace
                .get(&ident)
                .map_err(|err| Error::from((err, self.ctx.set_line(line))))?;

            let result = match op {
                TokenKind::ReassignPlus => old.add(result),
                TokenKind::ReassignMinus => old.sub(result),
                TokenKind::ReassignMul => old.mul(result),
                TokenKind::ReassignDiv => old.div(result),
                TokenKind::ReassignPow => old.pow(result),
                TokenKind::ReassignMod => old.rem(result),
                _ => unreachable!(),
            }
            .map_err(|err| Error::from((err, self.ctx.set_line(line))))?;

            self.namespace.add(ident, result);
        }

        Ok(Object::Nothing)
    }

    #[inline]
    fn visit_conditional_expr(
        &mut self,
        condition: Node,
        true_node: Node,
        false_node: Node,
        line: Line,
    ) -> InterpreterResult {
        let result = self.visit(condition)?;
        let bool_value =
            bool::try_from(result).map_err(|err| Error::from((err, self.ctx.set_line(line))))?;
        if bool_value {
            self.visit(true_node)
        } else {
            self.visit(false_node)
        }
    }

    #[inline]
    fn visit_if_node(
        &mut self,
        cases: Vec<(Option<Node>, Vec<Node>)>,
        line: Line,
    ) -> InterpreterResult {
        for (condition, nodes) in cases {
            let should_exec = match condition {
                Some(node) => bool::try_from(self.visit(node)?)
                    .map_err(|err| Error::from((err, self.ctx.set_line(line))))?,
                None => true, // 'else' statement
            };
            if should_exec {
                let expr = self.visit_multiple(nodes)?;
                if Some(State::Return) == self.state {
                    return Ok(expr);
                }
                break;
            }
        }
        Ok(Object::Nothing)
    }

    #[inline]
    fn visit_while_node(
        &mut self,
        condition: Node,
        body: Vec<Node>,
        line: Line,
    ) -> InterpreterResult {
        loop {
            let should_execute = bool::try_from(self.visit(condition.clone())?)
                .map_err(|err| Error::from((err, self.ctx.set_line(line))))?;

            if !should_execute {
                break;
            }
            let expr = self.visit_multiple(body.clone())?;
            check_state!(self, expr);
        }
        Ok(Object::Nothing)
    }

    #[inline]
    fn visit_for_node(
        &mut self,
        ident: Option<String>,
        start: Option<Node>,
        to: Node,
        step: Option<Node>,
        body: Vec<Node>,
    ) -> InterpreterResult {
        let start = self.get_for_loop_start(start)?;
        let end = self.get_for_loop_end(to)?;
        let default_step = if start <= end { 1 } else { -1 };
        let step = self.get_for_loop_step(step, default_step)?;
        if start <= end {
            match ident {
                Some(ident) => self.for_loop_incr_ident(ident, start, end, step, body),
                None => self.for_loop_incr(start, end, step, body),
            }
        } else {
            match ident {
                Some(ident) => self.for_loop_decr_ident(ident, start, end, step, body),
                None => self.for_loop_decr(start, end, step, body),
            }
        }
    }

    #[inline]
    fn get_for_loop_start(&mut self, node: Option<Node>) -> Result<Int, Error> {
        let Some(node) = node else {
            return Ok(0);  // Default start of for loop
        };
        let line = node.line;
        Int::try_from(self.visit(node)?).map_err(|err| Error::from((err, self.ctx.set_line(line))))
    }

    #[inline]
    fn get_for_loop_end(&mut self, node: Node) -> Result<Int, Error> {
        let line = node.line;
        Int::try_from(self.visit(node)?).map_err(|err| Error::from((err, self.ctx.set_line(line))))
    }

    #[inline]
    fn get_for_loop_step(&mut self, node: Option<Node>, default: Int) -> Result<Int, Error> {
        let Some(node) = node else {
            return Ok(default);
        };
        let line = node.line;
        Int::try_from(self.visit(node)?).map_err(|err| Error::from((err, self.ctx.set_line(line))))
    }

    #[inline]
    fn visit_foreach_node(
        &mut self,
        ident: Option<String>,
        iterable: Node,
        body: Vec<Node>,
        line: Line,
    ) -> InterpreterResult {
        let obj = self.visit(iterable)?;
        match obj {
            Object::Vector(v) => {
                let v = &*v.borrow();
                match ident {
                    Some(ident) => self.foreach_vector_ident(ident, v, body),
                    None => self.foreach_vector(v, body),
                }
            }
            Object::String(s) => match ident {
                Some(ident) => self.foreach_string_ident(ident, s, body),
                None => self.foreach_string(s, body),
            },
            _ => Err(Error::value(
                format!(
                    "Expected object {} or object {} as iterable in foreach loop, got {}",
                    Type::Vector,
                    Type::String,
                    obj.kind()
                ),
                self.ctx.set_line(line),
            )),
        }
    }

    #[inline]
    fn foreach_string_ident(
        &mut self,
        ident: String,
        s: String,
        body: Vec<Node>,
    ) -> InterpreterResult {
        for ch in s.chars() {
            self.namespace
                .add(ident.clone(), Object::String(String::from(ch)));
            let expr = self.visit_multiple(body.clone())?;
            check_state!(self, expr);
        }
        Ok(Object::Nothing)
    }

    #[inline]
    fn foreach_string(&mut self, s: String, body: Vec<Node>) -> InterpreterResult {
        for _ in s.chars() {
            let expr = self.visit_multiple(body.clone())?;
            check_state!(self, expr);
        }
        Ok(Object::Nothing)
    }

    #[inline]
    fn foreach_vector_ident(
        &mut self,
        ident: String,
        v: &[Object],
        body: Vec<Node>,
    ) -> InterpreterResult {
        for item in v {
            let item = item.duplicate();
            self.namespace.add(&ident, item);
            let expr = self.visit_multiple(body.clone())?;
            check_state!(self, expr);
        }
        Ok(Object::Nothing)
    }

    #[inline]
    fn foreach_vector(&mut self, v: &[Object], body: Vec<Node>) -> InterpreterResult {
        for _ in v {
            let expr = self.visit_multiple(body.clone())?;
            check_state!(self, expr);
        }
        Ok(Object::Nothing)
    }

    #[inline]
    fn visit_call_node(
        &mut self,
        callable: Node,
        args: Vec<Node>,
        line: Line,
    ) -> InterpreterResult {
        let obj = self.visit(callable)?;

        match obj {
            Object::Fun(func_name, arg_names, body) => {
                let mut namespace = Namespace::from(&self.namespace);
                self.setup_function(args, arg_names, Some(&func_name), line, &mut namespace)?;
                let ctx = self.ctx.set_line(line); // Already clones it
                call_function(
                    func_name,
                    body,
                    Rc::new(RefCell::new(namespace)),
                    ctx,
                    self.ctx.filename.clone(),
                )
            }
            Object::BuiltinFun(func_name) => {
                let args = args
                    .into_iter()
                    .map(|arg| self.visit(arg))
                    .collect::<Result<Vec<Object>, Error>>()?;

                call_builtin_function(&func_name, VecDeque::from(args))
                    .map_err(|err| Error::from((err, self.ctx.set_line(line))))
            }

            Object::AnonymousFun(arg_names, body) => {
                let mut namespace = Namespace::from(&self.namespace);
                self.setup_function(args, arg_names, None, line, &mut namespace)?;
                let ctx = self.ctx.set_line(line); // Already clones it
                call_anonymous_function(
                    body,
                    Rc::new(RefCell::new(namespace)),
                    ctx,
                    self.ctx.filename.clone(),
                )
            }
            _ => Err(Error::value(
                format!("object {} is not callable", obj.kind()),
                self.ctx.set_line(line),
            )),
        }
    }

    #[inline]
    fn visit_match_node(
        &mut self,
        input: Node,
        branches: Vec<(Option<Vec<Node>>, Option<Node>, Vec<Node>)>,
    ) -> InterpreterResult {
        let input = self.visit(input)?;
        'outer: for (patterns, guard, nodes) in branches {
            if !self.check_match_guard(guard)? {
                continue;
            }

            let Some(patterns) = patterns else {
                // Default case, 'else' keyword found
                let expr = self.visit_multiple(nodes)?;
                if Some(State::Return) == self.state {
                    return Ok(expr);
                }
                break;
            };

            for pattern in patterns {
                let pattern = self.visit(pattern)?;
                if pattern == input {
                    let expr = self.visit_multiple(nodes)?;
                    if Some(State::Return) == self.state {
                        return Ok(expr);
                    }
                    break 'outer;
                }
            }
        }
        Ok(Object::Nothing)
    }

    #[inline]
    /// Return true if the guard check was passed
    fn check_match_guard(&mut self, guard: Option<Node>) -> Result<bool, Error> {
        let Some(guard) = guard else {
            return Ok(true);
        };

        let line = guard.line;
        let result = self.visit(guard)?;
        bool::try_from(result).map_err(|err| Error::from((err, self.ctx.set_line(line))))
    }

    #[inline]
    fn visit_try_node(
        &mut self,
        try_nodes: Vec<Node>,
        mut catch_blocks: CatchBlocks,
        else_nodes: Option<Vec<Node>>,
    ) -> InterpreterResult {
        let mut exec_err = None;
        let mut err_alias = None;
        let mut catch_nodes = Vec::new();

        for node in try_nodes {
            match self.visit(node) {
                Ok(expr) => {
                    if Some(State::Return) == self.state {
                        return Ok(expr);
                    }
                }
                Err(err) => {
                    (exec_err, err_alias, catch_nodes) =
                        if let Some((alias, nodes)) = catch_blocks.try_catch(err.kind) {
                            (Some(err), alias, nodes)
                        } else {
                            return Err(err);
                        };
                    break;
                }
            }
        }

        if let Some(err) = exec_err {
            if let Some(err_alias) = err_alias {
                self.namespace.add(err_alias, Object::Error(err));
            }
            let expr = self.visit_multiple(catch_nodes)?;
            if Some(State::Return) == self.state {
                return Ok(expr);
            }
        } else if let Some(else_nodes) = else_nodes {
            let expr = self.visit_multiple(else_nodes)?;
            if Some(State::Return) == self.state {
                return Ok(expr);
            }
        }

        Ok(Object::Nothing)
    }

    #[inline]
    fn visit_return_node(&mut self, expr: Node) -> InterpreterResult {
        self.state = Some(State::Return);
        self.visit(expr)
    }

    #[inline]
    fn visit_throw_node(
        &mut self,
        err_kind: ErrorKind,
        err_msg: Option<Node>,
        line: Line,
    ) -> InterpreterResult {
        let err_msg = match err_msg {
            Some(node) => {
                let result = self.visit(node)?;
                // Otherwise the expr does not live long enough
                result.to_string()
            }
            None => String::new(),
        };
        Err(Error::new(err_kind, err_msg, self.ctx.set_line(line)))
    }

    #[inline]
    fn visit_infinite_loop(&mut self, body: Vec<Node>) -> InterpreterResult {
        loop {
            let expr = self.visit_multiple(body.clone())?;
            check_state!(self, expr)
        }
        Ok(Object::Nothing)
    }

    #[inline]
    fn visit_using_node(
        &mut self,
        nodes: Vec<Node>,
        path: PathBuf,
        relative_imports: Option<Vec<(String, Option<String>)>>,
        line: Line,
    ) -> InterpreterResult {
        if let Some(relative_imports) = relative_imports {
            return self.visit_using_relative_imports(nodes, path, relative_imports, line);
        }
        Interpreter::new(Rc::clone(&self.namespace))
            .filename(path)
            .parent(Box::new(self.ctx.set_line(line)))
            .visit_multiple(nodes)
    }

    #[inline]
    fn visit_using_relative_imports(
        &mut self,
        nodes: Vec<Node>,
        path: PathBuf,
        relative_imports: Vec<(String, Option<String>)>,
        line: Line,
    ) -> InterpreterResult {
        // Avoid name overriding in the main namespace, without cloning the pointer
        let mut interpreter =
            Interpreter::new(Rc::new(RefCell::new(Namespace::from(&self.namespace))))
                .filename(path)
                .parent(Box::new(self.ctx.set_line(line)));
        interpreter.visit_multiple(nodes)?;

        let namespace = Rc::try_unwrap(interpreter.namespace).unwrap().into_inner();
        for (import_name, alias) in relative_imports {
            let obj = namespace
                .get(&import_name)
                .map_err(|err| Error::from((err, self.ctx.set_line(line))))?;
            let ident = alias.unwrap_or(import_name);
            self.namespace.add(ident, obj);
        }
        Ok(Object::Nothing)
    }

    #[inline]
    fn setup_function(
        &mut self,
        args: Vec<Node>,
        arg_names: Vec<String>,
        func_name: Option<&str>,
        line: Line,
        namespace: &mut Namespace,
    ) -> Result<(), Error> {
        if arg_names.len() != args.len() {
            let err_msg = match func_name {
                Some(name) => format!(
                    "Expected {} arguments in call to function '{}', got {}",
                    arg_names.len(),
                    name,
                    args.len()
                ),
                None => format!(
                    "Expected {} arguments in call to anonymous function, got {}",
                    arg_names.len(),
                    args.len()
                ),
            };
            return Err(Error::new(
                ErrorKind::Type,
                err_msg,
                self.ctx.set_line(line),
            ));
        }

        for (arg, ident) in args.into_iter().zip(arg_names) {
            let obj = self.visit(arg)?;
            namespace.add(ident, obj);
        }

        Ok(())
    }
}

macro_rules! impl_for_loop {
    ($for_loop_with_ident:ident, $for_loop_without_ident:ident, $op:tt) => {
        impl Interpreter {
            #[inline]
            fn $for_loop_with_ident (
                &mut self,
                ident: String,
                mut start: Int,
                end: Int,
                step: Int,
                body: Vec<Node>,
            ) -> InterpreterResult {
                self.namespace.add(&ident, Object::Int(start));

                while start $op end {
                    let expr = self.visit_multiple(body.clone())?;
                    check_state!(self, expr);
                    start += step;
                    self.namespace.add(&ident, Object::Int(start));
                }
                Ok(Object::Nothing)
            }

            #[inline]
            fn $for_loop_without_ident (
                &mut self,
                mut start: Int,
                end: Int,
                step: Int,
                body: Vec<Node>,
            ) -> InterpreterResult {
                while start $op end {
                    let expr = self.visit_multiple(body.clone())?;
                    check_state!(self, expr);
                    start += step;
                }
                Ok(Object::Nothing)
            }
        }
    };
}

impl_for_loop!(for_loop_incr_ident, for_loop_incr, <);
impl_for_loop!(for_loop_decr_ident, for_loop_decr, >);
