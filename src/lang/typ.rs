use std::fmt;

use super::object::Object;

/// [`Type`] is the abstract representation of a group of attributes and methods.
///
/// [`Object`] is the single implementation of a [`Type`], there can be many of them.
///
/// It is like classes and instances in Python. There can be only one class, but multiple instances
/// of the same class.
#[derive(Clone, PartialEq, Debug, Eq)]
pub enum Type {
    Int,
    Float,
    String,
    Vector,
    Bool,
    Nothing,
    AnonymousFun,
    Fun,
    BuiltinFun,
    Error,
    Type,
}

impl fmt::Display for Type {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{:?}>", self)
    }
}

impl Type {
    pub const INT_LITERAL: &'static str = "Int";
    pub const FLOAT_LITERAL: &'static str = "Float";
    pub const STRING_LITERAL: &'static str = "String";
    pub const VECTOR_LITERAL: &'static str = "Vector";
    pub const BOOL_LITERAL: &'static str = "Bool";
    pub const NOTHING_LITERAL: &'static str = "Nothing";
    pub const ANONYMOUS_FUN_LITERAL: &'static str = "AnonymousFun";
    pub const FUN_LITERAL: &'static str = "Fun";
    pub const BUILTIN_FUN_LITERAL: &'static str = "BuiltinFun";
    pub const ERROR_LITERAL: &'static str = "Error";
    pub const TYPE_LITERAL: &'static str = "Type";
}

impl TryFrom<&str> for Type {
    type Error = ();

    #[inline]
    fn try_from(value: &str) -> Result<Self, <Type as TryFrom<&str>>::Error> {
        match value {
            Self::INT_LITERAL => Ok(Self::Int),
            Self::FLOAT_LITERAL => Ok(Self::Float),
            Self::STRING_LITERAL => Ok(Self::String),
            Self::VECTOR_LITERAL => Ok(Self::Vector),
            Self::BOOL_LITERAL => Ok(Self::Bool),
            Self::NOTHING_LITERAL => Ok(Self::Nothing),
            Self::ANONYMOUS_FUN_LITERAL => Ok(Self::AnonymousFun),
            Self::FUN_LITERAL => Ok(Self::Fun),
            Self::BUILTIN_FUN_LITERAL => Ok(Self::BuiltinFun),
            Self::ERROR_LITERAL => Ok(Self::Error),
            Self::TYPE_LITERAL => Ok(Self::Type),
            _ => Err(()),
        }
    }
}

impl From<Object> for Type {
    #[inline]
    fn from(obj: Object) -> Self {
        match obj {
            Object::Int(_) => Self::Int,
            Object::Float(_) => Self::Float,
            Object::String(_) => Self::String,
            Object::Vector(_) => Self::Vector,
            Object::Bool(_) => Self::Bool,
            Object::Nothing => Self::Nothing,
            Object::AnonymousFun(..) => Self::AnonymousFun,
            Object::Fun(..) => Self::Fun,
            Object::BuiltinFun(_) => Self::BuiltinFun,
            Object::Error(_) => Self::Error,
            Object::Type(_) => Self::Type,
        }
    }
}
