//! Lexical analysis is conducted with the [`Lexer`] `struct`.
//! Text is fed to an instance of the [`Lexer`] and tokenized into a `Vec<Token>`.
//! This is the first stage of the execution of a `betty` program, the other two being
//! parsing and interpreting.

use std::collections::{HashMap, VecDeque};
use std::path::PathBuf;

use super::error::{Ctx, Error, ErrorKind};
use super::token::{Token, TokenKind};
use super::type_alias::{Float, Int, LexerResult, Line};

pub struct Lexer {
    /// The text to be lexed as a sequence of chars. It will be drained as the analysis
    /// advances, as each character is removed and owned by `self.current_char`.
    source: VecDeque<char>,

    /// The holder of the current character, moves from the first to the last element
    /// of `self.source`. When the source becomes empty, it gets the value of [`None`].
    current_char: Option<char>,

    /// A map that associates each `betty` keyword to its literal
    keywords: HashMap<&'static str, TokenKind>,

    /// A map that associates each escape character (e.g. `\n`, `\t`, etc.) to the corresponding
    /// non-escaped value
    escape_chars: HashMap<char, char>,

    /// The context of the program
    ctx: Ctx,

    /// The current line of the analysis process. Increases by one each time a `\n` is found in the
    /// source
    line: Line,
}

impl Lexer {
    pub fn new(filename: Option<PathBuf>, mut source: VecDeque<char>) -> Self {
        // `source.remove(0)` cannot panic because it is guarded by an empty source check before.
        let current_char = source.pop_front();
        Self {
            source,
            current_char,
            keywords: keywords(),
            escape_chars: escape_chars(),
            ctx: Ctx::new().set_filename(filename),
            line: 1,
        }
    }

    #[inline]
    fn advance(&mut self) {
        self.current_char = if self.source.is_empty() {
            None
        } else {
            if Some('\n') == self.current_char {
                self.line += 1;
            }
            self.source.pop_front()
        };
    }

    /// Create a new identifier.
    ///
    /// As long as the value is a valid identifier character, that value is pushed to the end
    /// of a buffer. The loop stops when either we reach EOF or the character
    /// is not a valid identifier anymore. Then we check if the new identifier
    /// is a reserved keyword or not. If yes, then return a [`Token`] of that keyword, else an identifier
    fn make_ident(&mut self) -> Token {
        let mut ident = String::new();

        loop {
            match self.current_char {
                Some(ch) if matches!(ch, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9') => {
                    ident.push(ch);
                    self.advance();
                }
                _ => break,
            }
        }

        match ident.as_str() {
            "true" => Token::new(TokenKind::True, self.line),
            "false" => Token::new(TokenKind::False, self.line),
            "nothing" => Token::new(TokenKind::Nothing, self.line),
            _ => match self.keywords.get(ident.as_str()) {
                // The reserved keywords group do contain the identifier
                Some(kind) => Token::new(kind.clone(), self.line),

                // Just create a new identifier
                None => Token::new(TokenKind::Ident(ident), self.line),
            },
        }
    }

    fn make_number(&mut self) -> LexerResult {
        let mut num_str = String::new();
        loop {
            match self.current_char {
                Some(ch) if matches!(ch, '0'..='9' | '.' | '_') => {
                    if ch != '_' {
                        num_str.push(ch);
                    }
                    self.advance();
                }
                _ => break,
            }
        }

        if num_str.contains('.') {
            match num_str.parse::<Float>() {
                Ok(n) => Ok(Token::new(TokenKind::Float(n), self.line)),
                Err(err) => Err(Error::syntax(err.to_string(), self.ctx.set_line(self.line))),
            }
        } else {
            match num_str.parse::<Int>() {
                Ok(n) => Ok(Token::new(TokenKind::Int(n), self.line)),
                Err(_) => Err(Error::new(
                    ErrorKind::Overflow,
                    num_str,
                    self.ctx.set_line(self.line),
                )),
            }
        }
    }

    fn make_string(&mut self) -> LexerResult {
        let mut string = String::new();
        self.advance(); // skip '"'

        while let Some(ch) = self.current_char {
            match ch {
                '\\' => string.push(self.get_escape_char()?),
                '"' => {
                    self.advance(); // skip '"'
                    return Ok(Token::new(TokenKind::String(string), self.line));
                }
                _ => {
                    string.push(ch);
                    self.advance();
                }
            }
        }

        // self.current_char has become None due to EOF being reached
        Err(Error::syntax(
            "Unterminated string literal",
            self.ctx.set_line(self.line),
        ))
    }

    fn get_escape_char(&mut self) -> Result<char, Error> {
        self.advance();
        let Some(ch) = self.current_char else {
            return Err(Error::syntax(
                "Unterminated string",
                self.ctx.set_line(self.line),
            ));
        };

        self.advance(); // skip the escape char
        match self.escape_chars.get(&ch) {
            Some(escape_char) => Ok(*escape_char),
            None => Ok(ch),
        }
    }

    fn make_symbol(
        &mut self,
        symbol: char,
        kind_single: TokenKind,
        kind_double: TokenKind,
    ) -> Token {
        self.advance();
        match self.current_char {
            Some(ch) if ch == symbol => {
                self.advance();
                Token::new(kind_double, self.line)
            }
            _ => Token::new(kind_single, self.line),
        }
    }

    fn make_gt(&mut self) -> Token {
        self.make_symbol('=', TokenKind::Gt, TokenKind::Ge)
    }

    fn make_lt(&mut self) -> Token {
        self.make_symbol('=', TokenKind::Lt, TokenKind::Le)
    }

    fn make_eq(&mut self) -> Token {
        self.make_symbol('>', TokenKind::Eq, TokenKind::ThickArrow)
    }

    #[inline]
    fn make_colon(&mut self) -> LexerResult {
        self.advance(); // Skip first ':'
        let kind = match self.current_char {
            Some('+') => TokenKind::ReassignPlus,
            Some('-') => TokenKind::ReassignMinus,
            Some('*') => TokenKind::ReassignMul,
            Some('/') => TokenKind::ReassignDiv,
            Some('^') => TokenKind::ReassignPow,
            Some('%') => TokenKind::ReassignMod,
            Some(':') => TokenKind::Assign,
            _ => {
                return Err(Error::syntax(
                    format!(
                        "Expected one of {}, {}, {}, {}, {}, {}, {}",
                        TokenKind::Assign,
                        TokenKind::ReassignPlus,
                        TokenKind::ReassignMinus,
                        TokenKind::ReassignMul,
                        TokenKind::ReassignDiv,
                        TokenKind::ReassignPow,
                        TokenKind::ReassignMod
                    ),
                    self.ctx.set_line(self.line),
                ))
            }
        };

        self.advance(); // Skip second symbol
        Ok(Token::new(kind, self.line))
    }

    fn make_not(&mut self) -> LexerResult {
        self.advance();

        match self.current_char {
            Some(ch) if ch == '=' => {
                self.advance();
                Ok(Token::new(TokenKind::Neq, self.line))
            }
            _ => Err(Error::syntax(
                format!("Expected {}", TokenKind::Eq),
                self.ctx.set_line(self.line),
            )),
        }
    }

    #[inline]
    fn skip_comment(&mut self) {
        while let Some(ch) = self.current_char {
            if ch == '\n' {
                break;
            }
            self.advance();
        }
    }

    pub fn make_tokens(mut self) -> Result<Option<Vec<Token>>, Error> {
        let mut tokens = Vec::with_capacity(self.source.len());
        while let Some(ch) = self.current_char {
            match ch {
                ' ' | '\t' | '\r' => self.advance(),
                'a'..='z' | 'A'..='Z' | '_' => tokens.push(self.make_ident()),
                '0'..='9' => tokens.push(self.make_number()?),
                '"' => tokens.push(self.make_string()?),
                '\n' => {
                    tokens.push(Token::new(TokenKind::Newline, self.line));
                    self.advance();
                }
                '+' => {
                    tokens.push(Token::new(TokenKind::Plus, self.line));
                    self.advance();
                }
                '-' => {
                    tokens.push(Token::new(TokenKind::Minus, self.line));
                    self.advance();
                }
                '*' => {
                    tokens.push(Token::new(TokenKind::Mul, self.line));
                    self.advance();
                }
                '/' => {
                    tokens.push(Token::new(TokenKind::Div, self.line));
                    self.advance();
                }
                '^' => {
                    tokens.push(Token::new(TokenKind::Pow, self.line));
                    self.advance();
                }
                '%' => {
                    tokens.push(Token::new(TokenKind::Rem, self.line));
                    self.advance();
                }
                '|' => self.skip_comment(),
                ':' => tokens.push(self.make_colon()?),
                '(' => {
                    tokens.push(Token::new(TokenKind::LeftRoundBracket, self.line));
                    self.advance();
                }
                ')' => {
                    tokens.push(Token::new(TokenKind::RightRoundBracket, self.line));
                    self.advance();
                }
                '[' => {
                    tokens.push(Token::new(TokenKind::LeftSquareBracket, self.line));
                    self.advance();
                }
                ']' => {
                    tokens.push(Token::new(TokenKind::RightSquareBracket, self.line));
                    self.advance();
                }
                '.' => {
                    tokens.push(Token::new(TokenKind::Dot, self.line));
                    self.advance();
                }
                '>' => tokens.push(self.make_gt()),
                '<' => tokens.push(self.make_lt()),
                '=' => tokens.push(self.make_eq()),
                '!' => tokens.push(self.make_not()?),
                ',' => {
                    tokens.push(Token::new(TokenKind::Comma, self.line));
                    self.advance();
                }
                '?' => {
                    tokens.push(Token::new(TokenKind::Question, self.line));
                    self.advance();
                }
                _ => {
                    return Err(Error::syntax(
                        format!("Invalid character '{}'", ch),
                        self.ctx.set_line(self.line),
                    ))
                }
            }
        }

        if tokens.is_empty() || tokens.iter().all(|token| token.kind == TokenKind::Newline) {
            Ok(None)
        } else {
            tokens.push(Token::new(TokenKind::Eof, self.line));
            Ok(Some(tokens))
        }
    }
}

fn keywords() -> HashMap<&'static str, TokenKind> {
    HashMap::from([
        ("and", TokenKind::KwAND),
        ("or", TokenKind::KwOR),
        ("not", TokenKind::KwNOT),
        ("if", TokenKind::KwIF),
        ("else", TokenKind::KwELSE),
        ("do", TokenKind::KwDO),
        ("end", TokenKind::KwEND),
        ("for", TokenKind::KwFOR),
        ("foreach", TokenKind::KwFOREACH),
        ("while", TokenKind::KwWHILE),
        ("fun", TokenKind::KwFUN),
        ("continue", TokenKind::KwCONTINUE),
        ("break", TokenKind::KwBREAK),
        ("return", TokenKind::KwRETURN),
        ("match", TokenKind::KwMATCH),
        ("try", TokenKind::KwTRY),
        ("catch", TokenKind::KwCATCH),
        ("in", TokenKind::KwIN),
        ("throw", TokenKind::KwTHROW),
        ("using", TokenKind::KwUSING),
        ("as", TokenKind::KwAS),
    ])
}

fn escape_chars() -> HashMap<char, char> {
    HashMap::from([
        ('"', '\"'),
        ('n', '\n'),
        ('t', '\t'),
        ('r', '\r'),
        ('0', '\0'),
    ])
}
