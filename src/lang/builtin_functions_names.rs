// Console stdin and stdout
pub const BUILTIN_PRINT: &str = "print";
pub const BUILTIN_PRINTLN: &str = "println";
pub const BUILTIN_READ_LINE: &str = "read_line";

// Parse values
pub const BUILTIN_TO_INT: &str = "to_int";
pub const BUILTIN_TO_FLOAT: &str = "to_float";
pub const BUILTIN_TO_STR: &str = "to_str";

// Vector
pub const BUILTIN_VPUSH_BACK: &str = "vpush_back";
pub const BUILTIN_VPUSH_FRONT: &str = "vpush_front";
pub const BUILTIN_VPUSH_AT: &str = "vpush_at";
pub const BUILTIN_VPOP_FRONT: &str = "vpop_front";
pub const BUILTIN_VPOP_BACK: &str = "vpop_back";
pub const BUILTIN_VPOP_AT: &str = "vpop_at";
pub const BUILTIN_VFROM_RANGE: &str = "vfrom_range";
pub const BUILTIN_VCOPY: &str = "vcopy";

// String
pub const BUILTIN_STR_STARTS_WITH: &str = "str_starts_with";
pub const BUILTIN_STR_ENDS_WITH: &str = "str_ends_with";
pub const BUILTIN_STR_IS_LOWERCASE: &str = "str_is_lowercase";
pub const BUILTIN_STR_IS_UPPERCASE: &str = "str_is_uppercase";
pub const BUILTIN_STR_TO_LOWERCASE: &str = "str_to_lowercase";
pub const BUILTIN_STR_TO_UPPERCASE: &str = "str_to_uppercase";

// File IO
pub const BUILTIN_FREAD: &str = "fread";
pub const BUILTIN_FWRITE: &str = "fwrite";
pub const BUILTIN_FAPPEND: &str = "fappend";

// Error
pub const BUILTIN_ERR_SHORT: &str = "err_short";
pub const BUILTIN_ERR_TRACEBACK: &str = "err_traceback";
pub const BUILTIN_ERR_LINE: &str = "err_line";
pub const BUILTIN_ERR_KIND: &str = "err_kind";

// Misc
pub const BUILTIN_ASSERT: &str = "assert";

// Iterables
pub const BUILTIN_LEN: &str = "len";
pub const BUILTIN_GET: &str = "get";
pub const BUILTIN_JOIN: &str = "join";
pub const BUILTIN_SLICE: &str = "slice";
pub const BUILTIN_SPLIT: &str = "split";
pub const BUILTIN_REPLACE: &str = "replace";

// Type checking
pub const BUILTIN_ISINT: &str = "isint";
pub const BUILTIN_ISFLOAT: &str = "isfloat";
pub const BUILTIN_ISSTR: &str = "isstr";
pub const BUILTIN_ISBOOL: &str = "isbool";
pub const BUILTIN_ISVEC: &str = "isvec";
pub const BUILTIN_ISCALLABLE: &str = "iscallable";
pub const BUILTIN_ISERR: &str = "iserr";
