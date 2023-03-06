use std::fmt;

use super::type_alias::{Float, Int, Line};

#[derive(Clone, PartialEq, Debug)]
pub enum TokenKind {
    Ident(String),
    Int(Int),       // whole numbers from -inf to +inf
    Float(Float),   // real numbers from -inf to +inf
    String(String), // sequence of characters starting and ending with '"'

    Plus,  // the '+' symbol
    Minus, // the '-' symbol
    Mul,   // '*' symbol
    Div,   // '/' symbol
    Pow,   // '^' symbol
    Rem,   // '%' symbol

    Assign,        // '::' symbol
    ReassignPlus,  // ':+' symbol
    ReassignMinus, // ':-' symbol
    ReassignMul,   // ':*' symbol
    ReassignDiv,   // ':/' symbol
    ReassignPow,   // ':^' symbol
    ReassignMod,   // ':%' symbol

    LeftRoundBracket,   // '(' symbol
    RightRoundBracket,  // ')' symbol
    LeftSquareBracket,  // '[' symbol
    RightSquareBracket, // ']' symbol

    Eq,  // '=' symbol
    Neq, // '!=' symbol
    Gt,  // '>' symbol
    Ge,  // '>=' symbol
    Lt,  // '<' symbol
    Le,  // '<=' symbol

    Comma,      // ',' symbol
    Newline,    // '\n' symbol
    Dot,        // '.' symbol
    ThickArrow, // '=>' symbol
    Question,   // '?' symbol

    KwAND,      // keyword 'and'
    KwOR,       // keyword 'or'
    KwNOT,      // keyword 'not'
    KwIF,       // keyword 'if'
    KwELSE,     // keyword 'else'
    KwFOR,      // keyword 'for'
    KwFOREACH,  // keyword 'foreach'
    KwWHILE,    // keyword 'while'
    KwDO,       // keyword 'do'
    KwEND,      // keyword 'end'
    KwBREAK,    // keyword 'break'
    KwCONTINUE, // keyword 'continue'
    KwRETURN,   // keyword 'return'
    KwFUN,      // keyword 'fun'
    KwMATCH,    // keyword 'match'
    KwTRY,      // keyword 'try'
    KwCATCH,    // keyword 'catch'
    KwIN,       // keyword 'in'
    KwTHROW,    // keyword 'throw'
    KwUSING,    // keyword 'using'
    KwAS,       // keyword 'as'

    True,    // builtin identifier 'true'
    False,   // builtin identifier 'false'
    Nothing, // builtin identifier 'nothing'

    Eof, // end of file, appended at the end of the tokens
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(ref ident) => write!(f, "identifier '{}'", ident),
            Self::Int(x) => write!(f, "number literal '{}'", x),
            Self::Float(x) => write!(f, "number literal '{}'", x),
            Self::String(ref string) => write!(f, "string literal \"{}\"", string),
            Self::Plus => write!(f, "operator '+'"),
            Self::Minus => write!(f, "operator '-'"),
            Self::Mul => write!(f, "operator '*'"),
            Self::Div => write!(f, "operator '/'"),
            Self::Pow => write!(f, "operator '^'"),
            Self::Rem => write!(f, "operator '%'"),
            Self::Assign => write!(f, "assignment operator '::'"),
            Self::ReassignPlus => write!(f, "reassignment operator ':+'"),
            Self::ReassignMinus => write!(f, "reassignment operator ':-'"),
            Self::ReassignMul => write!(f, "reassignment operator ':*'"),
            Self::ReassignDiv => write!(f, "reassignment operator ':/'"),
            Self::ReassignPow => write!(f, "reassignment operator ':^'"),
            Self::ReassignMod => write!(f, "reassignment operator ':%'"),
            Self::LeftRoundBracket => write!(f, "token '('"),
            Self::RightRoundBracket => write!(f, "token ')'"),
            Self::LeftSquareBracket => write!(f, "token '['"),
            Self::RightSquareBracket => write!(f, "token ']'"),
            Self::Eq => write!(f, "operator '='"),
            Self::Neq => write!(f, "operator '!='"),
            Self::Gt => write!(f, "operator '>'"),
            Self::Ge => write!(f, "operator '>='"),
            Self::Lt => write!(f, "operator '<'"),
            Self::Le => write!(f, "operator '<='"),
            Self::Comma => write!(f, "token ','"),
            Self::Newline => write!(f, "newline"),
            Self::Dot => write!(f, "token '.'"),
            Self::ThickArrow => write!(f, "token '=>'"),
            Self::Question => write!(f, "token '?'"),
            Self::KwAND => write!(f, "keyword 'and'"),
            Self::KwOR => write!(f, "keyword 'or'"),
            Self::KwNOT => write!(f, "keyword 'not'"),
            Self::KwIF => write!(f, "keyword 'if'"),
            Self::KwELSE => write!(f, "keyword 'else'"),
            Self::KwFOR => write!(f, "keyword 'for'"),
            Self::KwFOREACH => write!(f, "keyword 'foreach'"),
            Self::KwWHILE => write!(f, "keyword 'while'"),
            Self::KwDO => write!(f, "keyword 'do'"),
            Self::KwEND => write!(f, "keyword 'end'"),
            Self::KwBREAK => write!(f, "keyword 'break'"),
            Self::KwCONTINUE => write!(f, "keyword 'continue'"),
            Self::KwRETURN => write!(f, "keyword 'return'"),
            Self::KwFUN => write!(f, "keyword 'fun'"),
            Self::KwMATCH => write!(f, "keyword 'match'"),
            Self::KwTRY => write!(f, "keyword 'try'"),
            Self::KwCATCH => write!(f, "keyword 'catch'"),
            Self::KwIN => write!(f, "keyword 'in'"),
            Self::KwTHROW => write!(f, "keyword 'throw'"),
            Self::KwUSING => write!(f, "keyword 'using'"),
            Self::KwAS => write!(f, "keyword 'as'"),
            Self::True => write!(f, "builtin identifier 'true'"),
            Self::False => write!(f, "builtin identifier 'false'"),
            Self::Nothing => write!(f, "builtin identifier 'nothing'"),
            Self::Eof => write!(f, "end of file"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub line: Line,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Token {
    pub fn new(kind: TokenKind, line: Line) -> Self {
        Self { kind, line }
    }
}
