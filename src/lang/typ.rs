use std::fmt;

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
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<{:?}>", self)
    }
}