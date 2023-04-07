//! The [`Object`] is the basic structure of a `betty` program. Every [`Object`]
//! is composed of one type, and a variable can refer to different [`Object`]s
//! during the program execution.

use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;
use std::rc::Rc;

use lazy_static::lazy_static;

use super::builtin_functions::*;
use super::builtin_functions_names::*;
use super::error::{invalid_op_err_msg, CFError, Ctx, Error, ErrorKind};
use super::interpreter::Interpreter;
use super::namespace::Namespace;
use super::node::Node;
use super::token::TokenKind;
use super::typ::Type;
use super::type_alias::{CFResult, Float, FunArgs, Int, InterpreterResult};

#[derive(Clone, Debug)]
pub enum Object {
    Int(Int),
    Float(Float),
    String(String),
    Vector(Rc<RefCell<Vec<Object>>>),
    Bool(bool),
    Nothing,
    AnonymousFun(Vec<String>, Vec<Node>),
    Fun(String, Vec<String>, Vec<Node>),
    BuiltinFun(String),
    Error(Error),
}

/// How do we want to print the [`Object`]?
impl fmt::Display for Object {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(n) => write!(f, "{}", n),

            // The current implementation prints Floats with 15 decimal points
            Self::Float(n) => write!(f, "{:.15}", n),
            Self::String(s) => write!(f, "{}", s),
            Self::Vector(vector) => {
                let v = &*vector.borrow();
                write!(
                    f,
                    "[{}]",
                    v.iter()
                        .map(|obj| {
                            match obj {
                                // We want to nicely format strings inside vectors,
                                // therefore we put their contents between double quotes
                                Self::String(s) => format!("\"{}\"", s),
                                /*
                                ERROR
                                v :: [1, 2, 3]
                                vpush_back(v, v)
                                println(v)

                                the ptr appends itself to itself, so when it has to be dereferenced for printing,
                                it prints the first three elements ok, then it dereferences the fourth element,
                                which is itself, and starts from scratch again

                                From the Debug trait implementation of RefCell:
                                The RefCell is mutably borrowed so we can't look at its value here.
                                Show a placeholder instead.

                                Check for pointer address equality. If it's the same, print placeholder instead.
                                This way we avoid infinite calls to .borrow()
                                */
                                Self::Vector(inner) if Rc::as_ptr(vector) == Rc::as_ptr(inner) => {
                                    format!("[Vector at {:?}]", Rc::as_ptr(inner))
                                }
                                _ => obj.to_string(),
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Self::Bool(x) => write!(f, "{}", x),
            Self::Nothing => write!(f, "nothing"),
            Self::Fun(..) => write!(f, "{}", self.kind()),
            Self::BuiltinFun(..) => write!(f, "{}", self.kind()),
            Self::AnonymousFun(..) => write!(f, "{}", self.kind()),
            Self::Error(err) => write!(f, "{}", err),
        }
    }
}

/// Operations between two [`Object`]s
impl Object {
    #[inline]
    pub fn add(self, rhs: Self) -> CFResult {
        match (self, rhs) {
            (Self::Int(x), Self::Int(y)) => match x.checked_add(y) {
                Some(result) => Ok(Self::Int(result)),
                None => Err(CFError(ErrorKind::Overflow, format!("{} + {}", x, y))),
            },
            (Self::Int(x), Self::Float(y)) | (Self::Float(y), Self::Int(x)) => {
                let result = x as Float + y;
                if result.is_infinite() {
                    Err(CFError(ErrorKind::Overflow, format!("{} + {}", x, y)))
                } else {
                    Ok(Self::Float(result))
                }
            }
            (Self::Float(x), Self::Float(y)) => {
                let result = x + y;
                if result.is_infinite() {
                    Err(CFError(ErrorKind::Overflow, format!("{} + {}", x, y)))
                } else {
                    Ok(Self::Float(result))
                }
            }
            (Self::String(s1), Self::String(s2)) => Ok(Self::String(s1 + &s2)),
            (Self::Vector(v1), Self::Vector(v2)) => {
                let v1 = &*v1.borrow();
                let v2 = &*v2.borrow();
                let mut result = Vec::new();
                result.reserve(v1.len() + v2.len());
                for item in v1 {
                    result.push(item.duplicate())
                }
                for item in v2 {
                    result.push(item.duplicate())
                }
                Ok(Self::Vector(Rc::new(RefCell::new(result))))
            }
            (left, right) => Err(CFError(
                ErrorKind::Type,
                invalid_op_err_msg(left, TokenKind::Plus, right),
            )),
        }
    }

    #[inline]
    pub fn sub(self, rhs: Self) -> CFResult {
        match (self, rhs) {
            (Self::Int(x), Self::Int(y)) => match x.checked_sub(y) {
                Some(result) => Ok(Self::Int(result)),
                None => Err(CFError(
                    ErrorKind::Overflow,
                    format!("Overflow: {} - {}", x, y),
                )),
            },
            (Self::Int(x), Self::Float(y)) | (Self::Float(y), Self::Int(x)) => {
                let result = x as Float - y;
                if result.is_infinite() {
                    Err(CFError(
                        ErrorKind::Overflow,
                        format!("Overflow: {} - {}", x, y),
                    ))
                } else {
                    Ok(Self::Float(result))
                }
            }
            (Self::Float(x), Self::Float(y)) => {
                let result = x - y;
                if result.is_infinite() {
                    Err(CFError(
                        ErrorKind::Overflow,
                        format!("Overflow: {} - {}", x, y),
                    ))
                } else {
                    Ok(Self::Float(result))
                }
            }
            (left, right) => Err(CFError(
                ErrorKind::Type,
                invalid_op_err_msg(left, TokenKind::Minus, right),
            )),
        }
    }

    #[inline]
    pub fn mul(self, rhs: Self) -> CFResult {
        match (self, rhs) {
            (Self::Int(x), Self::Int(y)) => match x.checked_mul(y) {
                Some(result) => Ok(Self::Int(result)),
                None => Err(CFError(ErrorKind::Overflow, format!("{} * {}", x, y))),
            },
            (Self::Int(x), Self::Float(y)) | (Self::Float(y), Self::Int(x)) => {
                let result = x as Float * y;
                if result.is_infinite() {
                    Err(CFError(ErrorKind::Overflow, format!("{} * {}", x, y)))
                } else {
                    Ok(Self::Float(result))
                }
            }
            (Self::Float(x), Self::Float(y)) => {
                let result = x * y;
                if result.is_infinite() {
                    Err(CFError(ErrorKind::Overflow, format!("{} * {}", x, y)))
                } else {
                    Ok(Self::Float(result))
                }
            }
            (Self::String(s), Self::Int(n)) | (Self::Int(n), Self::String(s)) => {
                if n <= 0 {
                    Err(CFError(
                        ErrorKind::Value,
                        format!(
                            "Cannot multiply object {} \"{}\" by negative object {} '{}'",
                            Type::String,
                            s,
                            Type::Int,
                            n
                        ),
                    ))
                } else {
                    Ok(Self::String(s.repeat(n as usize)))
                }
            }
            (Self::Vector(v), Self::Int(n)) | (Self::Int(n), Self::Vector(v)) => match n.cmp(&0) {
                cmp::Ordering::Less => Err(CFError(
                    ErrorKind::Value,
                    format!(
                        "Cannot multiply object {} by negative object {} '{}'",
                        Type::Vector,
                        Type::Int,
                        n
                    ),
                )),
                cmp::Ordering::Equal => Ok(Self::Vector(Default::default())),
                cmp::Ordering::Greater => {
                    let items = &*v.borrow();
                    let mut new_items = Vec::new();
                    for _ in 0..n {
                        new_items.extend_from_slice(&items[..])
                    }
                    Ok(Self::Vector(Rc::new(RefCell::new(new_items))))
                }
            },
            (left, right) => Err(CFError(
                ErrorKind::Type,
                invalid_op_err_msg(left, TokenKind::Mul, right),
            )),
        }
    }

    #[inline]
    pub fn div(self, rhs: Self) -> CFResult {
        if rhs.is_num_and_is_zero() {
            return Err(CFError(ErrorKind::DivisionByZero, "Divisor is 0".into()));
        }

        match (self, rhs) {
            (Self::Int(x), Self::Int(y)) => {
                let result = x as Float / y as Float;
                if result.is_infinite() {
                    Err(CFError(ErrorKind::Overflow, format!("{} / {}", x, y)))
                } else {
                    Ok(Self::Float(result))
                }
            }
            (Self::Int(x), Self::Float(y)) => {
                let result = x as Float / y;
                if result.is_infinite() {
                    Err(CFError(ErrorKind::Overflow, format!("{} / {}", x, y)))
                } else {
                    Ok(Self::Float(result))
                }
            }
            (Self::Float(x), Self::Int(y)) => {
                let result = x / y as Float;
                if result.is_infinite() {
                    Err(CFError(ErrorKind::Overflow, format!("{} / {}", x, y)))
                } else {
                    Ok(Self::Float(result))
                }
            }
            (Self::Float(x), Self::Float(y)) => {
                let result = x / y;
                if result.is_infinite() {
                    Err(CFError(ErrorKind::Overflow, format!("{} / {}", x, y)))
                } else {
                    Ok(Self::Float(result))
                }
            }
            (left, right) => Err(CFError(
                ErrorKind::Type,
                invalid_op_err_msg(left, TokenKind::Div, right),
            )),
        }
    }

    #[inline]
    pub fn pow(self, rhs: Self) -> CFResult {
        match (self, rhs) {
            (Self::Int(x), Self::Int(y)) => {
                let result = Float::powf(x as Float, y as Float) as Int;
                if result == Int::MAX || result == Int::MIN {
                    Err(CFError(ErrorKind::Overflow, format!("{} ^ {}", x, y)))
                } else {
                    Ok(Self::Int(result))
                }
            }
            (Self::Int(x), Self::Float(y)) => {
                let result = Float::powf(x as Float, y);
                if result.is_infinite() {
                    Err(CFError(ErrorKind::Overflow, format!("{} ^ {}", x, y)))
                } else {
                    Ok(Self::Float(result))
                }
            }
            (Self::Float(x), Self::Int(y)) => {
                let result = Float::powf(x, y as Float);
                if result.is_infinite() {
                    Err(CFError(ErrorKind::Overflow, format!("{} ^ {}", x, y)))
                } else {
                    Ok(Self::Float(result))
                }
            }
            (Self::Float(x), Self::Float(y)) => {
                let result = x.powf(y);
                if result.is_infinite() {
                    Err(CFError(ErrorKind::Overflow, format!("{} ^ {}", x, y)))
                } else {
                    Ok(Self::Float(result))
                }
            }
            (left, right) => Err(CFError(
                ErrorKind::Type,
                invalid_op_err_msg(left, TokenKind::Pow, right),
            )),
        }
    }

    #[inline]
    pub fn rem(self, rhs: Self) -> CFResult {
        if rhs.is_num_and_is_zero() {
            return Err(CFError(
                ErrorKind::DivisionByZero,
                "Attempted to calculate the remainder of a value with a divisor of 0".into(),
            ));
        }

        match (self, rhs) {
            (Self::Int(x), Self::Int(y)) => Ok(Self::Int(x % y)),
            (Self::Int(x), Self::Float(y)) => Ok(Self::Float(x as Float % y)),
            (Self::Float(x), Self::Int(y)) => Ok(Self::Float(x % y as Float)),
            (Self::Float(x), Self::Float(y)) => Ok(Self::Float(x % y)),
            (left, right) => Err(CFError(
                ErrorKind::Type,
                invalid_op_err_msg(left, TokenKind::Rem, right),
            )),
        }
    }

    #[inline]
    pub fn and(self, rhs: Self) -> CFResult {
        let left = bool::try_from(self)?;
        let right = bool::try_from(rhs)?;
        Ok(Self::Bool(left && right))
    }

    #[inline]
    pub fn or(self, rhs: Self) -> CFResult {
        let left = bool::try_from(self)?;
        let right = bool::try_from(rhs)?;
        Ok(Self::Bool(left || right))
    }

    #[inline]
    pub fn gt(self, rhs: Self) -> CFResult {
        let comp = self.get_comp(rhs, TokenKind::Gt)?;
        match comp {
            cmp::Ordering::Less | cmp::Ordering::Equal => Ok(Self::Bool(false)),
            cmp::Ordering::Greater => Ok(Self::Bool(true)),
        }
    }

    #[inline]
    pub fn ge(self, rhs: Self) -> CFResult {
        let comp = self.get_comp(rhs, TokenKind::Ge)?;
        match comp {
            cmp::Ordering::Less => Ok(Self::Bool(false)),
            cmp::Ordering::Equal | cmp::Ordering::Greater => Ok(Self::Bool(true)),
        }
    }

    #[inline]
    pub fn lt(self, rhs: Self) -> CFResult {
        let comp = self.get_comp(rhs, TokenKind::Lt)?;
        match comp {
            cmp::Ordering::Less => Ok(Self::Bool(true)),
            cmp::Ordering::Equal | cmp::Ordering::Greater => Ok(Self::Bool(false)),
        }
    }

    #[inline]
    pub fn le(self, rhs: Self) -> CFResult {
        let comp = self.get_comp(rhs, TokenKind::Le)?;
        match comp {
            cmp::Ordering::Less | cmp::Ordering::Equal => Ok(Self::Bool(true)),
            cmp::Ordering::Greater => Ok(Self::Bool(false)),
        }
    }

    #[inline]
    pub fn contains(self, item: Self) -> CFResult {
        match (&self, &item) {
            (Self::String(substring), Self::String(string)) => {
                Ok(Self::Bool(string.contains(substring)))
            }
            (item, Self::Vector(v)) => {
                let v = &*v.borrow();
                Ok(Self::Bool(v.contains(item)))
            }
            _ => Err(CFError(
                ErrorKind::Type,
                invalid_op_err_msg(self, TokenKind::KwIN, item),
            )),
        }
    }
}

/// Operations on the single [`Object`]
impl Object {
    #[inline]
    pub fn not(self) -> CFResult {
        bool::try_from(self).map(|value| Self::Bool(!value))
    }

    #[inline]
    pub fn unary_plus(self) -> CFResult {
        match self {
            Self::Int(_) | Self::Float(_) => Ok(self),
            _ => Err(CFError(
                ErrorKind::Type,
                format!("Cannot apply {} to {}", TokenKind::Plus, self.kind()),
            )),
        }
    }

    #[inline]
    pub fn unary_minus(self) -> CFResult {
        match self {
            Self::Int(num) => Ok(Self::Int(-num)),
            Self::Float(num) => Ok(Self::Float(-num)),
            _ => Err(CFError(
                ErrorKind::Type,
                format!("Cannot apply {} to {}", TokenKind::Minus, self.kind()),
            )),
        }
    }
}

/// Utilities
impl Object {
    #[inline]
    pub fn duplicate(&self) -> Self {
        if let Self::Vector(v) = self {
            Self::Vector(Rc::clone(v))
        } else {
            self.clone()
        }
    }

    #[inline]
    pub fn kind(&self) -> String {
        match self {
            Self::Int(_) => format!("object {}", Type::Int),
            Self::Float(_) => format!("object {}", Type::Float),
            Self::String(_) => format!("object {}", Type::String),
            Self::Bool(_) => format!("object {}", Type::Bool),
            Self::Vector(_) => format!("object {}", Type::Vector),
            Self::Nothing => format!("object {}", Type::Nothing),
            Self::Fun(name, ..) => format!("object {} {}", Type::Fun, name),
            Self::BuiltinFun(name) => format!("object {} {}", Type::BuiltinFun, name),
            Self::AnonymousFun(..) => format!("object {}", Type::AnonymousFun),
            Self::Error(name) => format!("object {} ({})", Type::Error, name),
        }
    }

    #[inline]
    fn get_comp(self, rhs: Self, op: TokenKind) -> Result<cmp::Ordering, CFError> {
        self.partial_cmp(&rhs)
            .ok_or_else(|| CFError(ErrorKind::Type, invalid_op_err_msg(self, op, rhs)))
    }

    #[inline]
    fn is_num_and_is_zero(&self) -> bool {
        matches!(self, Self::Int(n) if n == &0) || matches!(self, Self::Float(n) if n == &0.0)
    }
}

impl PartialOrd for Object {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => x.partial_cmp(y),
            (Self::Int(x), Self::Float(y)) => (*x as Float).partial_cmp(y),
            (Self::Float(x), Self::Int(y)) => x.partial_cmp(&(*y as Float)),
            (Self::Float(x), Self::Float(y)) => x.partial_cmp(y),
            _ => None,
        }
    }
}

// We decided to go for a better precision at the cost of execution speed, with the
// Epsilon comparison instead of the simple == check
impl PartialEq for Object {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => x == y,

            (Self::Int(x), Self::Float(y)) => (*x as Float - *y).abs() < Float::EPSILON,

            (Self::Float(x), Self::Int(y)) => (*x - *y as Float).abs() < Float::EPSILON,

            (Self::Float(x), Self::Float(y)) => (x - y).abs() < Float::EPSILON,

            (Self::String(x), Self::String(y)) => x == y,

            (Self::Vector(x), Self::Vector(y)) => x == y,

            (Self::Bool(x), Self::Bool(y)) => x == y,

            // I lost approx 2 hours of life because the following was written as
            //
            // other == &Self::Nothing
            //
            // which caused an infinite recursion
            // Found at 22:30 28 Dec 2022
            // Idk why I am so stupid btw
            //
            // (Self::Nothing, _) => matches!(other, Self::Nothing)
            (Self::Nothing, Self::Nothing) => true,

            (Self::AnonymousFun(..), _) => false,

            (Self::Fun(name1, ..), Self::Fun(name2, ..)) => name1 == name2,

            (Self::BuiltinFun(name1), Self::BuiltinFun(name2)) => name1 == name2,

            (Self::Error(err1), Self::Error(err2)) => err1 == err2,

            _ => false,
        }
    }
}

#[inline]
pub fn call_function(
    fun_name: String,
    body: Vec<Node>,
    namespace: Rc<RefCell<Namespace>>,
    ctx: Ctx,
    filename: Option<PathBuf>,
) -> InterpreterResult {
    match filename {
        Some(filename) => Interpreter::new(namespace).filename(filename),
        None => Interpreter::new(namespace),
    }
    .fun_name(fun_name)
    .parent(Box::new(ctx))
    .visit_multiple(body)
}

#[inline]
pub fn call_anonymous_function(
    body: Vec<Node>,
    namespace: Rc<RefCell<Namespace>>,
    ctx: Ctx,
    filename: Option<PathBuf>,
) -> InterpreterResult {
    match filename {
        Some(filename) => Interpreter::new(namespace).filename(filename),
        None => Interpreter::new(namespace),
    }
    .fun_name(Type::AnonymousFun.to_string())
    .parent(Box::new(ctx))
    .visit_multiple(body)
}

#[inline]
pub fn call_builtin_function(fun_name: &str, args: FunArgs) -> CFResult {
    BUILTIN_FUN_CALLER.call(fun_name, args)
}

macro_rules! builtin_fun_caller {
    ( $($fun_name:expr => $fun_struct:expr),+ $(,)? ) => {
        struct BuiltinFunCaller<'a>(HashMap<&'a str, Box<dyn BuiltinFn>>);
        impl BuiltinFunCaller<'_> {
            pub fn new() -> Self {
                // We convert the $fun_struct to a trait object otherwise the compiler
                // doesn't understand what that is
                Self {
                    0: HashMap::from([
                        $(
                            ($fun_name, Box::new($fun_struct) as Box<dyn BuiltinFn>),
                        )*
                    ])
                }
            }

            #[inline]
            pub fn call(&self, fun_name: &str, args: FunArgs) -> CFResult {
                self.0.get(fun_name).unwrap().call(args)
            }
        }
    };
}

builtin_fun_caller! {
    BUILTIN_PRINT => Print,
    BUILTIN_PRINTLN => Println,
    BUILTIN_READ_LINE => ReadLine,

    BUILTIN_TO_INT => ToInt,
    BUILTIN_TO_FLOAT => ToFloat,
    BUILTIN_TO_STR => ToStr,

    BUILTIN_VPUSH_BACK => VPushBack,
    BUILTIN_VPUSH_FRONT => VPushFront,
    BUILTIN_VPUSH_AT => VPushAt,
    BUILTIN_VPOP_FRONT => VPopFront,
    BUILTIN_VPOP_BACK => VPopBack,
    BUILTIN_VPOP_AT => VPopAt,
    BUILTIN_VFROM_RANGE => VFromRange,
    BUILTIN_VCOPY => VCopy,

    BUILTIN_STR_ENDS_WITH => StrEndsWith,
    BUILTIN_STR_STARTS_WITH => StrStartsWith,
    BUILTIN_STR_IS_UPPERCASE => StrIsUppercase,
    BUILTIN_STR_IS_LOWERCASE => StrIsLowercase,
    BUILTIN_STR_TO_UPPERCASE => StrToUppercase,
    BUILTIN_STR_TO_LOWERCASE => StrToLowercase,

    BUILTIN_FREAD => FRead,
    BUILTIN_FWRITE => FWrite,
    BUILTIN_FAPPEND => FAppend,

    BUILTIN_ERR_SHORT => ErrShort,
    BUILTIN_ERR_TRACEBACK => ErrTraceback,
    BUILTIN_ERR_LINE => ErrLine,
    BUILTIN_ERR_KIND => ErrKind,

    BUILTIN_LEN => Len,
    BUILTIN_GET => Get,
    BUILTIN_JOIN => Join,
    BUILTIN_SPLIT => Split,
    BUILTIN_SLICE => Slice,
    BUILTIN_REPLACE => Replace,

    BUILTIN_ASSERT => Assert,

    BUILTIN_ISINT => IsInt,
    BUILTIN_ISFLOAT => IsFloat,
    BUILTIN_ISSTR => IsStr,
    BUILTIN_ISVEC => IsVec,
    BUILTIN_ISBOOL => IsBool,
    BUILTIN_ISCALLABLE => IsCallable,
    BUILTIN_ISERR => IsErr,
}

lazy_static! {
    static ref BUILTIN_FUN_CALLER: BuiltinFunCaller<'static> = BuiltinFunCaller::new();
}

impl TryFrom<Object> for bool {
    type Error = CFError;

    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj {
            Object::Bool(value) => Ok(value),
            _ => Err(CFError(
                ErrorKind::Type,
                format!("Expected object {}, got {}", Type::Bool, obj.kind()),
            )),
        }
    }
}

impl TryFrom<Object> for Int {
    type Error = CFError;

    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj {
            Object::Int(num) => Ok(num),
            _ => Err(CFError(
                ErrorKind::Type,
                format!("Expected object {}, got {}", Type::Int, obj.kind()),
            )),
        }
    }
}
