use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::builtin_functions_names::*;
use super::error::{CFError, Error, ErrorKind};
use super::object::Object;
use super::type_alias::CFResult;

#[derive(Debug)]
pub struct Namespace {
    inner: HashMap<String, Object>,
    parent: Option<Rc<RefCell<Namespace>>>,
}

impl Namespace {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            inner: populate_namespace(),
            parent: None,
        }))
    }

    #[inline]
    pub fn get(&self, ident: &str) -> CFResult {
        match self.inner.get(ident) {
            Some(obj) => Ok(obj.duplicate()),
            None => match &self.parent {
                Some(parent) => parent.get(ident),
                None => Err(CFError(
                    ErrorKind::UnknownIdentifier,
                    format!("'{}'", ident),
                )),
            },
        }
    }

    #[inline]
    pub fn add(&mut self, ident: String, value: Object) {
        self.inner.insert(ident, value);
    }
}

impl IntoIterator for Namespace {
    type Item = (String, Object);
    type IntoIter = std::collections::hash_map::IntoIter<String, Object>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl From<&Rc<RefCell<Namespace>>> for Namespace {
    #[inline]
    fn from(other: &Rc<RefCell<Namespace>>) -> Self {
        Self {
            inner: HashMap::new(),
            parent: Some(Rc::clone(other)),
        }
    }
}

pub trait NamespaceWrapper {
    fn get(&self, ident: &str) -> CFResult;
    fn add(&mut self, ident: impl Into<String>, value: Object);
}

impl NamespaceWrapper for Rc<RefCell<Namespace>> {
    #[inline]
    fn get(&self, ident: &str) -> CFResult {
        let namespace = &*self.borrow();
        namespace.get(ident)
    }

    #[inline]
    fn add(&mut self, ident: impl Into<String>, value: Object) {
        let namespace = &mut *self.borrow_mut();
        namespace.add(ident.into(), value);
    }
}

#[inline]
fn populate_namespace() -> HashMap<String, Object> {
    HashMap::from([
        (
            BUILTIN_PRINT.into(),
            Object::BuiltinFun(BUILTIN_PRINT.into()),
        ),
        (
            BUILTIN_PRINTLN.into(),
            Object::BuiltinFun(BUILTIN_PRINTLN.into()),
        ),
        (
            BUILTIN_READ_LINE.into(),
            Object::BuiltinFun(BUILTIN_READ_LINE.into()),
        ),
        (
            BUILTIN_TO_INT.into(),
            Object::BuiltinFun(BUILTIN_TO_INT.into()),
        ),
        (
            BUILTIN_TO_FLOAT.into(),
            Object::BuiltinFun(BUILTIN_TO_FLOAT.into()),
        ),
        (
            BUILTIN_TO_STR.into(),
            Object::BuiltinFun(BUILTIN_TO_STR.into()),
        ),
        (BUILTIN_LEN.into(), Object::BuiltinFun(BUILTIN_LEN.into())),
        (
            BUILTIN_VPUSH_BACK.into(),
            Object::BuiltinFun(BUILTIN_VPUSH_BACK.into()),
        ),
        (
            BUILTIN_VPUSH_FRONT.into(),
            Object::BuiltinFun(BUILTIN_VPUSH_FRONT.into()),
        ),
        (
            BUILTIN_VPUSH_AT.into(),
            Object::BuiltinFun(BUILTIN_VPUSH_AT.into()),
        ),
        (
            BUILTIN_VPOP_BACK.into(),
            Object::BuiltinFun(BUILTIN_VPOP_BACK.into()),
        ),
        (
            BUILTIN_VPOP_FRONT.into(),
            Object::BuiltinFun(BUILTIN_VPOP_FRONT.into()),
        ),
        (
            BUILTIN_VPOP_AT.into(),
            Object::BuiltinFun(BUILTIN_VPOP_AT.into()),
        ),
        (
            BUILTIN_VFROM_RANGE.into(),
            Object::BuiltinFun(BUILTIN_VFROM_RANGE.into()),
        ),
        (
            BUILTIN_VCOPY.into(),
            Object::BuiltinFun(BUILTIN_VCOPY.into()),
        ),
        (
            BUILTIN_STR_STARTS_WITH.into(),
            Object::BuiltinFun(BUILTIN_STR_STARTS_WITH.into()),
        ),
        (
            BUILTIN_STR_ENDS_WITH.into(),
            Object::BuiltinFun(BUILTIN_STR_ENDS_WITH.into()),
        ),
        (
            BUILTIN_STR_IS_UPPERCASE.into(),
            Object::BuiltinFun(BUILTIN_STR_IS_UPPERCASE.into()),
        ),
        (
            BUILTIN_STR_IS_LOWERCASE.into(),
            Object::BuiltinFun(BUILTIN_STR_IS_LOWERCASE.into()),
        ),
        (BUILTIN_JOIN.into(), Object::BuiltinFun(BUILTIN_JOIN.into())),
        (
            BUILTIN_STR_TO_UPPERCASE.into(),
            Object::BuiltinFun(BUILTIN_STR_TO_UPPERCASE.into()),
        ),
        (
            BUILTIN_STR_TO_LOWERCASE.into(),
            Object::BuiltinFun(BUILTIN_STR_TO_LOWERCASE.into()),
        ),
        (
            BUILTIN_SLICE.into(),
            Object::BuiltinFun(BUILTIN_SLICE.into()),
        ),
        (
            BUILTIN_REPLACE.into(),
            Object::BuiltinFun(BUILTIN_REPLACE.into()),
        ),
        (
            BUILTIN_SPLIT.into(),
            Object::BuiltinFun(BUILTIN_SPLIT.into()),
        ),
        (
            BUILTIN_ASSERT.into(),
            Object::BuiltinFun(BUILTIN_ASSERT.into()),
        ),
        (BUILTIN_GET.into(), Object::BuiltinFun(BUILTIN_GET.into())),
        (
            BUILTIN_FREAD.into(),
            Object::BuiltinFun(BUILTIN_FREAD.into()),
        ),
        (
            BUILTIN_FWRITE.into(),
            Object::BuiltinFun(BUILTIN_FWRITE.into()),
        ),
        (
            BUILTIN_FAPPEND.into(),
            Object::BuiltinFun(BUILTIN_FAPPEND.into()),
        ),
        (
            BUILTIN_ERR_SHORT.into(),
            Object::BuiltinFun(BUILTIN_ERR_SHORT.into()),
        ),
        (
            BUILTIN_ERR_TRACEBACK.into(),
            Object::BuiltinFun(BUILTIN_ERR_TRACEBACK.into()),
        ),
        (
            BUILTIN_ERR_LINE.into(),
            Object::BuiltinFun(BUILTIN_ERR_LINE.into()),
        ),
        (
            BUILTIN_ERR_KIND.into(),
            Object::BuiltinFun(BUILTIN_ERR_KIND.into()),
        ),
        (
            BUILTIN_ISINT.into(),
            Object::BuiltinFun(BUILTIN_ISINT.into()),
        ),
        (
            BUILTIN_ISFLOAT.into(),
            Object::BuiltinFun(BUILTIN_ISFLOAT.into()),
        ),
        (
            BUILTIN_ISSTR.into(),
            Object::BuiltinFun(BUILTIN_ISSTR.into()),
        ),
        (
            BUILTIN_ISBOOL.into(),
            Object::BuiltinFun(BUILTIN_ISBOOL.into()),
        ),
        (
            BUILTIN_ISVEC.into(),
            Object::BuiltinFun(BUILTIN_ISVEC.into()),
        ),
        (
            BUILTIN_ISCALLABLE.into(),
            Object::BuiltinFun(BUILTIN_ISCALLABLE.into()),
        ),
        (
            BUILTIN_ISERR.into(),
            Object::BuiltinFun(BUILTIN_ISERR.into()),
        ),
        (
            "ValueError".into(),
            Object::Error(Error::new(ErrorKind::Value, None::<String>, None)),
        ),
        (
            "TypeError".into(),
            Object::Error(Error::new(ErrorKind::Type, None::<String>, None)),
        ),
        (
            "UnknownIdentifierError".into(),
            Object::Error(Error::new(
                ErrorKind::UnknownIdentifier,
                None::<String>,
                None,
            )),
        ),
        (
            "OverflowError".into(),
            Object::Error(Error::new(ErrorKind::Overflow, None::<String>, None)),
        ),
        (
            "DivisionByZeroError".into(),
            Object::Error(Error::new(ErrorKind::DivisionByZero, None::<String>, None)),
        ),
        (
            "WrongArgumentsNumberError".into(),
            Object::Error(Error::new(
                ErrorKind::WrongArgumentsNumber,
                None::<String>,
                None,
            )),
        ),
        (
            "IndexOutOfBoundsError".into(),
            Object::Error(Error::new(
                ErrorKind::IndexOutOfBounds,
                None::<String>,
                None,
            )),
        ),
        (
            "FileIOError".into(),
            Object::Error(Error::new(ErrorKind::FileIO, None::<String>, None)),
        ),
        (
            "AssertionError".into(),
            Object::Error(Error::new(ErrorKind::Assertion, None::<String>, None)),
        ),
        (
            "VectorMutationError".into(),
            Object::Error(Error::new(ErrorKind::VectorMutation, None::<String>, None)),
        ),
        (
            "ModuleImportError".into(),
            Object::Error(Error::new(ErrorKind::ModuleImport, None::<String>, None)),
        ),
    ])
}
