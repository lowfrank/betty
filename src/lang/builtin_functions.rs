use std::cell::RefCell;
use std::cmp;
use std::fs;
use std::io::{self, Write};
use std::num::IntErrorKind;
use std::rc::Rc;

use super::builtin_functions_names::*;
use super::error::{expected_value_err_msg, CFError, Error, ErrorKind};
use super::object::Object;
use super::typ::Type;
use super::type_alias::{CFResult, Float, FunArgs, Int};

// Sync is required by lazy_static, otherwise the following error shows up
// (dyn builtin_functions::BuiltinFn + 'static)` cannot be shared between threads safely
pub trait BuiltinFn: Sync {
    fn call(&self, args: FunArgs) -> CFResult;
}

macro_rules! range {
    ($num:expr) => {
        match $num.cmp(&0) {
            cmp::Ordering::Less => Rc::new(RefCell::new(
                (($num + 1)..=0).rev().map(Object::Int).collect::<Vec<_>>(),
            )),
            cmp::Ordering::Equal => Rc::new(RefCell::new(Vec::new())),
            cmp::Ordering::Greater => {
                Rc::new(RefCell::new((0..$num).map(Object::Int).collect::<Vec<_>>()))
            }
        }
    };
    ($start:expr, $end:expr) => {
        match $start.cmp(&$end) {
            cmp::Ordering::Less => Rc::new(RefCell::new(
                ($start..$end).map(Object::Int).collect::<Vec<_>>(),
            )),
            cmp::Ordering::Equal => Rc::new(RefCell::new(Vec::new())),
            cmp::Ordering::Greater => Rc::new(RefCell::new(
                (($end + 1)..=$start)
                    .rev()
                    .map(Object::Int)
                    .collect::<Vec<_>>(),
            )),
        }
    };
    ($start:expr, $end:expr, $step:expr) => {
        match $start.cmp(&$end) {
            cmp::Ordering::Less => Rc::new(RefCell::new(
                ($start..$end)
                    .filter(|x| x % $step == 0)
                    .map(Object::Int)
                    .collect::<Vec<_>>(),
            )),
            cmp::Ordering::Equal => Rc::new(RefCell::new(Vec::new())),
            cmp::Ordering::Greater => Rc::new(RefCell::new(
                ($end..=$start)
                    .rev()
                    .filter(|x| (x - $start) % $step == 0)
                    .map(Object::Int)
                    .collect::<Vec<_>>(),
            )),
        }
    };
}

#[inline]
fn check_vfrom_range_args(start: Int, end: Int, step: Int) -> Result<(), CFError> {
    if step < 0 && start < end {
        return Err(CFError(
            ErrorKind::Value,
            format!(
                "Cannot have start < end and step < 0 in builtin function '{}' (start is {}, end is {}, step is {})",
                BUILTIN_VFROM_RANGE, start, end, step
            ),
        ));
    } else if step > 0 && start > end {
        return Err(CFError(
            ErrorKind::Value,
            format!(
                "Cannot have start > end and step > 0 in builtin function '{}' (start is {}, end is {}, step is {})",
                BUILTIN_VFROM_RANGE, start, end, step
            ),
        ));
    } else if step == 0 {
        return Err(CFError(
            ErrorKind::Value,
            format!(
                "Cannot have step = 0 in builtin function '{}'",
                BUILTIN_VFROM_RANGE
            ),
        ));
    }
    Ok(())
}

#[inline]
fn check_slice_args(start: Int, end: Int, len: usize) -> Result<(), CFError> {
    if end < 0 || end > (len - 1) as Int {
        return Err(CFError(
            ErrorKind::IndexOutOfBounds,
            format!(
                "The end index given to '{}' is out of bounds (len: {}, index: {})",
                BUILTIN_SLICE, len, end
            ),
        ));
    }

    if start < 0 || start > (len - 1) as Int {
        return Err(CFError(
            ErrorKind::IndexOutOfBounds,
            format!(
                "The start index given to '{}' is out of is out of bounds (len: {}, index: {})",
                BUILTIN_SLICE, len, start,
            ),
        ));
    }

    if start > end {
        return Err(CFError(
            ErrorKind::IndexOutOfBounds,
            format!(
                "The start index is greater than the end index (start: {}, end: {}) \
                                in builtin function '{}'",
                start, end, BUILTIN_SLICE
            ),
        ));
    }

    Ok(())
}

macro_rules! builtin_fn {
    ($ident:ident, $name:expr, $expected_args_len:expr) => {
        pub struct $ident;

        impl $ident {
            const NAME: &str = $name;

            #[inline]
            fn arg(args: &mut FunArgs) -> Object {
                args.pop_front().unwrap()
            }

            #[allow(dead_code)]
            #[inline]
            fn check_args_len(args_len: usize) -> Result<(), CFError> {
                if args_len != $expected_args_len {
                    Err(CFError(
                        ErrorKind::WrongArgumentsNumber,
                        format!(
                            "Required for builtin function '{}': {}, got {}",
                            Self::NAME,
                            $expected_args_len,
                            args_len
                        ),
                    ))
                } else {
                    Ok(())
                }
            }

            #[allow(dead_code)]
            #[inline]
            fn int_from_args(args: &mut FunArgs, n: u8) -> Result<Int, CFError> {
                let obj = Self::arg(args);
                match obj {
                    Object::Int(n) => Ok(n),
                    _ => Err(CFError(
                        ErrorKind::Type,
                        expected_value_err_msg(format!("{}", Type::Int), n, Self::NAME, obj.kind()),
                    )),
                }
            }

            #[allow(dead_code)]
            #[inline]
            fn float_from_args(args: &mut FunArgs, n: u8) -> Result<Float, CFError> {
                let obj = Self::arg(args);
                match obj {
                    Object::Float(n) => Ok(n),
                    _ => Err(CFError(
                        ErrorKind::Type,
                        expected_value_err_msg(
                            format!("{}", Type::Float),
                            n,
                            Self::NAME,
                            obj.kind(),
                        ),
                    )),
                }
            }

            #[allow(dead_code)]
            #[inline]
            fn str_from_args(args: &mut FunArgs, n: u8) -> Result<String, CFError> {
                let obj = Self::arg(args);
                match obj {
                    Object::String(s) => Ok(s),
                    _ => Err(CFError(
                        ErrorKind::Type,
                        expected_value_err_msg(
                            format!("{}", Type::String),
                            n,
                            Self::NAME,
                            obj.kind(),
                        ),
                    )),
                }
            }

            #[allow(dead_code)]
            #[inline]
            fn vec_from_args(
                args: &mut FunArgs,
                n: u8,
            ) -> Result<Rc<RefCell<Vec<Object>>>, CFError> {
                let obj = Self::arg(args);
                match obj {
                    Object::Vector(v) => Ok(v),
                    _ => Err(CFError(
                        ErrorKind::Type,
                        expected_value_err_msg(
                            format!("{}", Type::Vector),
                            n,
                            Self::NAME,
                            obj.kind(),
                        ),
                    )),
                }
            }

            #[allow(dead_code)]
            #[inline]
            fn err_from_args(args: &mut FunArgs, n: u8) -> Result<Error, CFError> {
                let obj = Self::arg(args);
                match obj {
                    Object::Error(err) => Ok(err),
                    _ => Err(CFError(
                        ErrorKind::Type,
                        expected_value_err_msg(
                            format!("{}", Type::Error),
                            n,
                            Self::NAME,
                            obj.kind(),
                        ),
                    )),
                }
            }

            #[allow(dead_code)]
            #[inline]
            fn bool_from_args(args: &mut FunArgs, n: u8) -> Result<bool, CFError> {
                let obj = Self::arg(args);
                match obj {
                    Object::Bool(x) => Ok(x),
                    _ => Err(CFError(
                        ErrorKind::Type,
                        expected_value_err_msg(
                            format!("{}", Type::Bool),
                            n,
                            Self::NAME,
                            obj.kind(),
                        ),
                    )),
                }
            }

            #[allow(dead_code)]
            #[inline]
            fn check_index_out_of_bounds(index: Int, upper: usize) -> Result<(), CFError> {
                if index < 0 || index > upper as Int {
                    Err(CFError(
                        ErrorKind::IndexOutOfBounds,
                        format!(
                            "The index given to '{}' is out of bounds (len: {}, index: {})",
                            Self::NAME,
                            if Self::NAME == BUILTIN_VPOP_AT {
                                upper + 1
                            } else {
                                upper
                            },
                            index
                        ),
                    ))
                } else {
                    Ok(())
                }
            }
        }
    };
}

macro_rules! vec_obj_mut {
    ($v:expr) => {
        &mut *$v.try_borrow_mut().map_err(|_| {
            CFError(
                ErrorKind::VectorMutation,
                format!("Cannot mutate {} in the current context", Type::Vector),
            )
        })?
    };
}

// 999 is just a placeholder, because it won't be checked
builtin_fn!(Print, BUILTIN_PRINT, 999);
impl BuiltinFn for Print {
    #[inline]
    fn call(&self, args: FunArgs) -> CFResult {
        print!(
            "{}",
            args.into_iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join("")
        );
        io::stdout().flush().unwrap();
        Ok(Object::Nothing)
    }
}

// 999 is just a placeholder, because it won't be checked
builtin_fn!(Println, BUILTIN_PRINTLN, 999);
impl BuiltinFn for Println {
    #[inline]
    fn call(&self, args: FunArgs) -> CFResult {
        println!(
            "{}",
            args.into_iter()
                .map(|obj| obj.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        );
        Ok(Object::Nothing)
    }
}

builtin_fn!(ReadLine, BUILTIN_READ_LINE, 0);
impl BuiltinFn for ReadLine {
    #[inline]
    fn call(&self, args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let mut result = String::new();
        io::stdin().read_line(&mut result).unwrap();
        if result.ends_with('\n') {
            result.pop();
        }
        if result.ends_with('\r') {
            result.pop();
        }
        Ok(Object::String(result))
    }
}

builtin_fn!(ToInt, BUILTIN_TO_INT, 1);
impl BuiltinFn for ToInt {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let obj = Self::arg(&mut args);
        match obj {
            Object::Int(_) => Ok(obj),
            Object::Float(num) => Ok(Object::Int(num as Int)),
            Object::String(ref s) => match s.parse::<Int>() {
                Ok(num) => Ok(Object::Int(num)),
                Err(err) => match err.kind() {
                    IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => {
                        Err(CFError(ErrorKind::Overflow, format!("to_int(\"{}\")", s)))
                    }
                    _ => Err(CFError(
                        ErrorKind::Value,
                        format!("Cannot parse {} \"{}\" to {}", Type::String, s, Type::Int),
                    )),
                },
            },
            _ => Err(CFError(
                ErrorKind::Type,
                expected_value_err_msg(
                    format!("{} or {} or {}", Type::Int, Type::Float, Type::String),
                    1,
                    Self::NAME,
                    obj.kind(),
                ),
            )),
        }
    }
}

builtin_fn!(ToFloat, BUILTIN_TO_FLOAT, 1);
impl BuiltinFn for ToFloat {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let obj = Self::arg(&mut args);
        match obj {
            Object::Int(num) => Ok(Object::Float(num as Float)),
            Object::Float(_) => Ok(obj),
            Object::String(ref s) => match s.parse::<Float>() {
                Ok(num) => {
                    if num.is_infinite() {
                        Err(CFError(ErrorKind::Overflow, format!("to_float(\"{}\")", s)))
                    } else {
                        Ok(Object::Float(num))
                    }
                }
                Err(_) => Err(CFError(
                    ErrorKind::Value,
                    format!("Cannot parse {} \"{}\" to {}", Type::String, s, Type::Float),
                )),
            },
            _ => Err(CFError(
                ErrorKind::Type,
                expected_value_err_msg(
                    format!("{} or {} or {}", Type::Int, Type::Float, Type::String),
                    1,
                    Self::NAME,
                    obj.kind(),
                ),
            )),
        }
    }
}

builtin_fn!(ToStr, BUILTIN_TO_STR, 1);
impl BuiltinFn for ToStr {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let obj = Self::arg(&mut args);
        Ok(Object::String(obj.to_string()))
    }
}

builtin_fn!(VPushBack, BUILTIN_VPUSH_BACK, 2);
impl BuiltinFn for VPushBack {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let v = Self::vec_from_args(&mut args, 1)?;
        let v = vec_obj_mut!(v);
        let item = Self::arg(&mut args);
        v.push(item);
        Ok(Object::Nothing)
    }
}

builtin_fn!(VPushFront, BUILTIN_VPUSH_FRONT, 2);
impl BuiltinFn for VPushFront {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let v = Self::vec_from_args(&mut args, 1)?;
        let v = vec_obj_mut!(v);
        let item = Self::arg(&mut args);
        v.insert(0, item);
        Ok(Object::Nothing)
    }
}

builtin_fn!(VPushAt, BUILTIN_VPUSH_AT, 3);
impl BuiltinFn for VPushAt {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let v = Self::vec_from_args(&mut args, 1)?;
        let v = vec_obj_mut!(v);
        let index = Self::int_from_args(&mut args, 2)?;
        Self::check_index_out_of_bounds(index, v.len())?;
        let item = Self::arg(&mut args);
        v.insert(index as usize, item);
        Ok(Object::Nothing)
    }
}

builtin_fn!(VPopFront, BUILTIN_VPOP_FRONT, 1);
impl BuiltinFn for VPopFront {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let obj = Self::vec_from_args(&mut args, 1)?;
        let v = vec_obj_mut!(obj);
        if !v.is_empty() {
            Ok(v.remove(0))
        } else {
            Err(CFError(
                ErrorKind::IndexOutOfBounds,
                format!("Cannot remove first element of empty {}", Type::Vector),
            ))
        }
    }
}
builtin_fn!(VPopBack, BUILTIN_VPOP_BACK, 1);
impl BuiltinFn for VPopBack {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let obj = Self::vec_from_args(&mut args, 1)?;
        let v = vec_obj_mut!(obj);
        v.pop().ok_or_else(|| {
            CFError(
                ErrorKind::IndexOutOfBounds,
                format!("Cannot remove last element of empty {}", Type::Vector),
            )
        })
    }
}
builtin_fn!(VPopAt, BUILTIN_VPOP_AT, 2);
impl BuiltinFn for VPopAt {
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let obj = Self::vec_from_args(&mut args, 1)?;
        let v = vec_obj_mut!(obj);
        let index = Self::int_from_args(&mut args, 2)?;
        let upper = if v.is_empty() { 0 } else { v.len() - 1 };
        Self::check_index_out_of_bounds(index, upper)?;
        Ok(v.remove(index as usize))
    }
}

// 999 is just a placeholder, because it won't be checked
builtin_fn!(VFromRange, BUILTIN_VFROM_RANGE, 999);
impl BuiltinFn for VFromRange {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        match args.len() {
            1 => {
                let Some(Object::Int(num)) = args.pop_front() else {
                    return Err(CFError(
                        ErrorKind::Type,
                        format!("All arguments passed to builtin function '{}', must be {}", BUILTIN_VFROM_RANGE, Type::Int)
                    ));
                };
                Ok(Object::Vector(range!(num)))
            }
            2 => {
                let (Some(Object::Int(start)), Some(Object::Int(end))) = (args.pop_front(), args.pop_front()) else {
                    return Err(CFError(
                        ErrorKind::Type,
                        format!("All arguments passed to builtin function '{}', must be of type {}", BUILTIN_VFROM_RANGE, Type::Int)
                    ));
                };
                Ok(Object::Vector(range!(start, end)))
            }
            3 => {
                let (
                    Some(Object::Int(start)), Some(Object::Int(end)), Some(Object::Int(step))
                ) = (
                    args.pop_front(), args.pop_front(), args.pop_front()
                ) else {
                    return Err(CFError(
                        ErrorKind::Type,
                        format!("All arguments passed to builtin function '{}', must be of type {}", BUILTIN_VFROM_RANGE, Type::Int)
                    ));
                };
                check_vfrom_range_args(start, end, step)?;
                Ok(Object::Vector(range!(start, end, step)))
            }
            _ => Err(CFError(
                ErrorKind::WrongArgumentsNumber,
                format!(
                    "Required for builtin function '{}': 1 or 2 or 3, got {}",
                    BUILTIN_VFROM_RANGE,
                    args.len()
                ),
            )),
        }
    }
}

builtin_fn!(VCopy, BUILTIN_VCOPY, 1);
impl BuiltinFn for VCopy {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let v = Self::vec_from_args(&mut args, 1)?;
        let v = &*v.borrow();
        Ok(Object::Vector(Rc::new(RefCell::new(v.to_vec()))))
    }
}

builtin_fn!(StrStartsWith, BUILTIN_STR_STARTS_WITH, 2);
impl BuiltinFn for StrStartsWith {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let s = Self::str_from_args(&mut args, 1)?;
        let sub = Self::str_from_args(&mut args, 2)?;
        Ok(Object::Bool(s.starts_with(&sub)))
    }
}

builtin_fn!(StrEndsWith, BUILTIN_STR_ENDS_WITH, 2);
impl BuiltinFn for StrEndsWith {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let s = Self::str_from_args(&mut args, 1)?;
        let sub = Self::str_from_args(&mut args, 2)?;
        Ok(Object::Bool(s.ends_with(&sub)))
    }
}

builtin_fn!(StrIsLowercase, BUILTIN_STR_IS_LOWERCASE, 1);
impl BuiltinFn for StrIsLowercase {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let s = Self::str_from_args(&mut args, 1)?;
        Ok(Object::Bool(s.chars().all(char::is_lowercase)))
    }
}

builtin_fn!(StrIsUppercase, BUILTIN_STR_IS_UPPERCASE, 1);
impl BuiltinFn for StrIsUppercase {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let s = Self::str_from_args(&mut args, 1)?;
        Ok(Object::Bool(s.chars().all(char::is_uppercase)))
    }
}

builtin_fn!(StrToLowercase, BUILTIN_STR_TO_LOWERCASE, 1);
impl BuiltinFn for StrToLowercase {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let s = Self::str_from_args(&mut args, 1)?;
        Ok(Object::String(s.to_lowercase()))
    }
}

builtin_fn!(StrToUppercase, BUILTIN_STR_TO_UPPERCASE, 1);
impl BuiltinFn for StrToUppercase {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let s = Self::str_from_args(&mut args, 1)?;
        Ok(Object::String(s.to_uppercase()))
    }
}

builtin_fn!(FRead, BUILTIN_FREAD, 1);
impl BuiltinFn for FRead {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let path = Self::str_from_args(&mut args, 1)?;
        match fs::read_to_string(path) {
            Ok(contents) => Ok(Object::String(contents)),
            Err(err) => Err(CFError(ErrorKind::FileIO, err.to_string())),
        }
    }
}

builtin_fn!(FWrite, BUILTIN_FWRITE, 2);
impl BuiltinFn for FWrite {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let path = Self::str_from_args(&mut args, 1)?;
        let contents = Self::str_from_args(&mut args, 2)?;
        fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(path)
            .map_err(|err| CFError(ErrorKind::FileIO, err.to_string()))?
            .write_all(contents.as_bytes())
            .map_err(|err| CFError(ErrorKind::FileIO, err.to_string()))?;
        Ok(Object::Nothing)
    }
}

builtin_fn!(FAppend, BUILTIN_FAPPEND, 2);
impl BuiltinFn for FAppend {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let path = Self::str_from_args(&mut args, 1)?;
        let contents = Self::str_from_args(&mut args, 2)?;
        fs::OpenOptions::new()
            .append(true)
            .create(true)
            .open(path)
            .map_err(|err| CFError(ErrorKind::FileIO, err.to_string()))?
            .write_all(contents.as_bytes())
            .map_err(|err| CFError(ErrorKind::FileIO, err.to_string()))?;
        Ok(Object::Nothing)
    }
}

builtin_fn!(ErrShort, BUILTIN_ERR_SHORT, 1);
impl BuiltinFn for ErrShort {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let err = Self::err_from_args(&mut args, 1)?;
        Ok(Object::String(err.short_msg()))
    }
}

builtin_fn!(ErrTraceback, BUILTIN_ERR_TRACEBACK, 1);
impl BuiltinFn for ErrTraceback {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let err = Self::err_from_args(&mut args, 1)?;
        Ok(Object::String(err.traceback()))
    }
}

builtin_fn!(ErrLine, BUILTIN_ERR_LINE, 1);
impl BuiltinFn for ErrLine {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let err = Self::err_from_args(&mut args, 1)?;
        match err.ctx {
            Some(ctx) => Ok(Object::Int(ctx.line as Int)),
            None => Ok(Object::Nothing),
        }
    }
}

builtin_fn!(ErrKind, BUILTIN_ERR_KIND, 1);
impl BuiltinFn for ErrKind {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let err = Self::err_from_args(&mut args, 1)?;
        Ok(Object::String(err.kind.to_string()))
    }
}

builtin_fn!(ErrDescription, BUILTIN_ERR_DESCRIPTION, 1);
impl BuiltinFn for ErrDescription {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let err = Self::err_from_args(&mut args, 1)?;
        match err.msg {
            Some(msg) => Ok(Object::String(msg)),
            None => Ok(Object::Nothing),
        }
    }
}

builtin_fn!(Len, BUILTIN_LEN, 1);
impl BuiltinFn for Len {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let obj = Self::arg(&mut args);
        match obj {
            Object::String(s) => Ok(Object::Int(s.len() as Int)),
            Object::Vector(v) => {
                let v = &*v.borrow();
                Ok(Object::Int(v.len() as Int))
            }
            _ => Err(CFError(
                ErrorKind::Type,
                expected_value_err_msg(
                    format!("{} or {}", Type::String, Type::Vector),
                    1,
                    Self::NAME,
                    obj.kind(),
                ),
            )),
        }
    }
}

builtin_fn!(Get, BUILTIN_GET, 2);
impl BuiltinFn for Get {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let iter = Self::arg(&mut args);
        let index = Self::int_from_args(&mut args, 2)?;
        if index < 0 {
            return Err(CFError(
                ErrorKind::Value,
                format!(
                    "Expected index >= 0 as argument n. {} of builtin function '{}', got '{}'",
                    2,
                    Self::NAME,
                    index
                ),
            ));
        }

        match iter {
            Object::Vector(v) => {
                let v = &*v.borrow();
                match v.get(index as usize) {
                    Some(item) => Ok(item.duplicate()),
                    None => Err(CFError(
                        ErrorKind::IndexOutOfBounds,
                        format!(
                            "The index given to {} in '{}' is out of bounds (upper bound: {}, lower bound: 0, index: {})",
                            Type::Vector,
                            Self::NAME,
                            v.len(),
                            index
                        ),
                    )),
                }
            }
            Object::String(s) => match s.chars().nth(index as usize) {
                Some(item) => Ok(Object::String(String::from(item))),
                None => Err(CFError(
                    ErrorKind::IndexOutOfBounds,
                    format!(
                        "The index given to {} in '{}' is out of bounds (upper bound: {}, lower bound: 0, index: {})",
                        Type::String,
                        Self::NAME,
                        s.len(),
                        index
                    ),
                )),
            }
            _ => Err(CFError(
                ErrorKind::Type,
                expected_value_err_msg(
                    format!("{} or {}", Type::String, Type::Vector),
                    1,
                    Self::NAME,
                    iter.kind()
                ),
            ))
        }
    }
}

builtin_fn!(Join, BUILTIN_JOIN, 2);
impl BuiltinFn for Join {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let sep = Self::str_from_args(&mut args, 1)?;
        let obj = Self::arg(&mut args);
        match obj {
            Object::Vector(v) => {
                let v = &*v.borrow();
                Ok(Object::String(
                    v.iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(&sep),
                ))
            }
            Object::String(s) => Ok(Object::String(
                s.chars()
                    .map(|ch| ch.to_string())
                    .collect::<Vec<_>>()
                    .join(&sep),
            )),
            _ => Err(CFError(
                ErrorKind::Type,
                expected_value_err_msg(
                    format!("{} or {}", Type::String, Type::Vector),
                    2,
                    Self::NAME,
                    obj.kind(),
                ),
            )),
        }
    }
}

builtin_fn!(Split, BUILTIN_SPLIT, 2);
impl BuiltinFn for Split {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let obj = Self::arg(&mut args);
        let split = Self::arg(&mut args);
        match (obj, split) {
            (Object::Vector(v), split) => {
                let v = &*v.borrow();
                if !v.contains(&split) {
                    return Ok(Object::Vector(Rc::new(RefCell::new(
                        v.iter().map(Object::duplicate).collect(),
                    ))));
                }
                // Type annotation is needed here
                let mut tmp = Vec::<&Object>::new();
                let mut result = Vec::new();
                for item in v {
                    if item == &split {
                        result.push(Object::Vector(Rc::new(RefCell::new(
                            tmp.iter()
                                .map(|x| x.duplicate())
                                .collect::<Vec<_>>(),
                        ))));
                        tmp.clear();
                    } else {
                        tmp.push(item);
                    }
                }
                result.push(Object::Vector(Rc::new(RefCell::new(
                    tmp.into_iter()
                        .map(Object::duplicate)
                        .collect::<Vec<_>>(),
                ))));
                Ok(Object::Vector(Rc::new(RefCell::new(result))))
            }
            (Object::String(s), Object::String(split)) => {
                Ok(Object::Vector(Rc::new(RefCell::new(
                    s.split(&split)
                        .into_iter()
                        .map(|s| Object::String(s.into()))
                        .collect(),
                ))))
            }
            (obj, split) => Err(CFError(
                ErrorKind::Type,
                format!("Expected ({}, {}) or ({}, any) as arguments of builtin function '{}', got ({}, {})",
                    Type::String,
                    Type::String,
                    Type::Vector,
                    Self::NAME,
                    obj.kind(),
                    split.kind(),
                ),
            )),
        }
    }
}

builtin_fn!(Replace, BUILTIN_REPLACE, 3);
impl BuiltinFn for Replace {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let obj = Self::arg(&mut args);
        let old = Self::arg(&mut args);
        let new = Self::arg(&mut args);
        match (obj, old, new) {
            (Object::Vector(v), old, new) => {
                let v = &mut *v.borrow_mut();
                v.iter_mut().for_each(|x| {
                    if *x == old {
                        *x = new.duplicate();
                    }
                });
                Ok(Object::Nothing)
            }
            (Object::String(s), Object::String(old), Object::String(new)) => Ok(Object::String(s.replace(&old, &new))),
            (obj, old, new) => Err(CFError(
                ErrorKind::Type,
                format!("Expected ({}, any, any) or ({}, {}, {}) as arguments of builtin function '{}', got ({}, {}, {})",
                    Type::Vector,
                    Type::String,
                    Type::String,
                    Type::String,
                    Self::NAME,
                    obj.kind(),
                    old.kind(),
                    new.kind(),
                ),
            )),
        }
    }
}

builtin_fn!(Slice, BUILTIN_SLICE, 3);
impl BuiltinFn for Slice {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let obj = Self::arg(&mut args);
        let start = Self::int_from_args(&mut args, 2)?;
        let end = Self::int_from_args(&mut args, 3)?;
        match obj {
            Object::String(s) => {
                check_slice_args(start, end, s.len())?;
                Ok(Object::String(if start == end {
                    String::new()
                } else {
                    s[start as usize..=end as usize].into()
                }))
            }
            Object::Vector(v) => {
                let v = &*v.borrow();
                check_slice_args(start, end, v.len())?;
                Ok(Object::Vector(Rc::new(RefCell::new(if start == end {
                    Vec::new()
                } else {
                    v[start as usize..=end as usize].into()
                }))))
            }
            _ => Err(CFError(
                ErrorKind::Type,
                expected_value_err_msg(
                    format!("{} or {}", Type::String, Type::Vector),
                    1,
                    Self::NAME,
                    obj.kind(),
                ),
            )),
        }
    }
}

builtin_fn!(Assert, BUILTIN_ASSERT, 999);
impl BuiltinFn for Assert {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        if ![1, 2].contains(&args.len()) {
            return Err(CFError(
                ErrorKind::WrongArgumentsNumber,
                format!(
                    "Required for builtin function '{}': 1 or 2, got {}",
                    Self::NAME,
                    args.len()
                ),
            ));
        }
        let bool_value = Self::bool_from_args(&mut args, 1)?;
        let msg = args.pop_front();
        match bool_value {
            true => Ok(Object::Nothing),
            false => Err(CFError(
                ErrorKind::Assertion,
                msg.map(|obj| obj.to_string()).unwrap_or_default(),
            )),
        }
    }
}

builtin_fn!(IsInt, BUILTIN_ISINT, 1);
impl BuiltinFn for IsInt {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let obj = Self::arg(&mut args);
        Ok(Object::Bool(matches!(obj, Object::Int(_))))
    }
}

builtin_fn!(IsFloat, BUILTIN_ISFLOAT, 1);
impl BuiltinFn for IsFloat {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let obj = Self::arg(&mut args);
        Ok(Object::Bool(matches!(obj, Object::Float(_))))
    }
}

builtin_fn!(IsStr, BUILTIN_ISSTR, 1);
impl BuiltinFn for IsStr {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let obj = Self::arg(&mut args);
        Ok(Object::Bool(matches!(obj, Object::String(_))))
    }
}

builtin_fn!(IsBool, BUILTIN_ISBOOL, 1);
impl BuiltinFn for IsBool {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let obj = Self::arg(&mut args);
        Ok(Object::Bool(matches!(obj, Object::Bool(_))))
    }
}

builtin_fn!(IsVec, BUILTIN_ISVEC, 1);
impl BuiltinFn for IsVec {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let obj = Self::arg(&mut args);
        Ok(Object::Bool(matches!(obj, Object::Vector(_))))
    }
}

builtin_fn!(IsCallable, BUILTIN_ISCALLABLE, 1);
impl BuiltinFn for IsCallable {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let obj = Self::arg(&mut args);
        Ok(Object::Bool(matches!(
            obj,
            Object::Fun(..) | Object::AnonymousFun(..) | Object::BuiltinFun(..)
        )))
    }
}

builtin_fn!(IsErr, BUILTIN_ISERR, 1);
impl BuiltinFn for IsErr {
    #[inline]
    fn call(&self, mut args: FunArgs) -> CFResult {
        Self::check_args_len(args.len())?;
        let obj = Self::arg(&mut args);
        Ok(Object::Bool(matches!(obj, Object::Error(_))))
    }
}
