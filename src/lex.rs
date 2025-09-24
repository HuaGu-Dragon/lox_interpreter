use std::fmt::Display;

use miette::{Diagnostic, Error, LabeledSpan, NamedSource, SourceSpan, miette};
use thiserror::Error;

#[derive(Error, Debug, Diagnostic)]
#[error("Unexpected token '{token}'")]
#[diagnostic(help("remove or correct the token: `{token}`"))]
pub struct SingleTokenError {
    #[source_code]
    src: NamedSource<String>,

    #[label("this character")]
    bad_bit: SourceSpan,

    pub token: char,
}

impl SingleTokenError {
    pub fn line(&self) -> usize {
        self.src.inner()[..=self.bad_bit.offset()].lines().count()
    }
}

#[derive(Error, Debug, Diagnostic)]
#[error("unterminated double quote string")]
#[diagnostic(help("For more information about this error, try `rustc --explain E0765`."))]
pub struct StringTerminationError {
    #[source_code]
    src: NamedSource<String>,

    #[label("Syntax Error: Missing trailing `\"` symbol to terminate the string literal")]
    bad_line: SourceSpan,
}

impl StringTerminationError {
    pub fn line(&self) -> usize {
        self.src.inner()[..=self.bad_line.offset()].lines().count()
    }
}

#[derive(Error, Debug, Diagnostic)]
#[error("Unexpected end of file")]
#[diagnostic(help(
    "The file ended unexpectedly, possibly due to a missing closing brace or parenthesis."
))]
pub struct Eof {
    #[source_code]
    src: NamedSource<String>,

    #[label("Syntax Error: Unexpected end of file")]
    bad_line: SourceSpan,
}

impl Eof {
    pub fn build(lexer: &Lexer<'_>) -> Self {
        Eof {
            src: NamedSource::new(lexer.filename.unwrap_or("<input>"), lexer.whole.to_string()),
            bad_line: SourceSpan::from(lexer.byte - 1..lexer.byte),
        }
    }

    pub fn line(&self) -> usize {
        self.src.inner()[..=self.bad_line.offset()].lines().count()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'de> {
    pub kind: TokenKind,
    pub literal: &'de str,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    BangEqual,
    EqualEqual,
    GreaterEqual,
    LessEqual,
    Greater,
    Less,
    Slash,
    Bang,
    Equal,
    String,
    Ident,
    Number(f64),
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Print,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lit = self.literal;
        match self.kind {
            TokenKind::LeftParen => write!(f, "LEFT_PAREN {lit} null"),
            TokenKind::RightParen => write!(f, "RIGHT_PAREN {lit} null"),
            TokenKind::LeftBrace => write!(f, "LEFT_BRACE {lit} null"),
            TokenKind::RightBrace => write!(f, "RIGHT_BRACE {lit} null"),
            TokenKind::Comma => write!(f, "COMMA {lit} null"),
            TokenKind::Dot => write!(f, "DOT {lit} null"),
            TokenKind::Minus => write!(f, "MINUS {lit} null"),
            TokenKind::Plus => write!(f, "PLUS {lit} null"),
            TokenKind::Semicolon => write!(f, "SEMICOLON {lit} null"),
            TokenKind::Star => write!(f, "STAR {lit} null"),
            TokenKind::BangEqual => write!(f, "BANG_EQUAL {lit} null"),
            TokenKind::EqualEqual => write!(f, "EQUAL_EQUAL {lit} null"),
            TokenKind::GreaterEqual => write!(f, "GREATER_EQUAL {lit} null"),
            TokenKind::LessEqual => write!(f, "LESS_EQUAL {lit} null"),
            TokenKind::Greater => write!(f, "GREATER {lit} null"),
            TokenKind::Less => write!(f, "LESS {lit} null"),
            TokenKind::Slash => write!(f, "SLASH {lit} null"),
            TokenKind::Bang => write!(f, "BANG {lit} null"),
            TokenKind::Equal => write!(f, "EQUAL {lit} null"),
            TokenKind::String => write!(f, "STRING \"{lit}\" {lit}"),
            TokenKind::Ident => write!(f, "IDENTIFIER {lit} null"),
            TokenKind::Number(n) => {
                if n == n.trunc() {
                    write!(f, "NUMBER {lit} {n}.0")
                } else {
                    write!(f, "NUMBER {lit} {n}")
                }
            }
            TokenKind::And => write!(f, "AND {lit} null"),
            TokenKind::Class => write!(f, "CLASS {lit} null"),
            TokenKind::Else => write!(f, "ELSE {lit} null"),
            TokenKind::False => write!(f, "FALSE {lit} null"),
            TokenKind::For => write!(f, "FOR {lit} null"),
            TokenKind::Fun => write!(f, "FUN {lit} null"),
            TokenKind::If => write!(f, "IF {lit} null"),
            TokenKind::Nil => write!(f, "NIL {lit} null"),
            TokenKind::Or => write!(f, "OR {lit} null"),
            TokenKind::Return => write!(f, "RETURN {lit} null"),
            TokenKind::Super => write!(f, "SUPER {lit} null"),
            TokenKind::This => write!(f, "THIS {lit} null"),
            TokenKind::True => write!(f, "TRUE {lit} null"),
            TokenKind::Var => write!(f, "VAR {lit} null"),
            TokenKind::While => write!(f, "WHILE {lit} null"),
            TokenKind::Print => write!(f, "PRINT {lit} null"),
        }
    }
}

pub struct Lexer<'de> {
    filename: Option<&'de str>,
    whole: &'de str,
    rest: &'de str,
    pub byte: usize,
    peeked: Option<Result<Token<'de>, Error>>,
}

impl<'de> Lexer<'de> {
    pub fn new(filename: Option<&'de str>, input: &'de str) -> Self {
        Lexer {
            filename,
            whole: input,
            rest: input,
            byte: 0,
            peeked: None,
        }
    }

    pub fn expect(&mut self, expected: TokenKind, error: &str) -> Result<Token<'de>, Error> {
        self.expect_where(|token| token.kind == expected, expected, error)
    }

    pub fn expect_where(
        &mut self,
        check: impl FnOnce(&Token<'de>) -> bool,
        expected: TokenKind,
        error: &str,
    ) -> Result<Token<'de>, Error> {
        match self.next() {
            Some(Ok(token)) if check(&token) => Ok(token),
            Some(Ok(token)) => Err(miette::miette!(
                help = format!("use `{expected:?}` here instead"),
                labels = vec![LabeledSpan::at(
                    self.byte - token.literal.len()..self.byte,
                    "here",
                )],
                "{error}",
            )
            .with_source_code(self.whole.to_string())),
            Some(Err(e)) => Err(e),
            None => Err(Eof::build(self).into()),
        }
    }

    pub fn peek(&mut self) -> Option<&Result<Token<'de>, Error>> {
        if self.peeked.is_some() {
            return self.peeked.as_ref();
        }
        self.peeked = self.next();
        self.peeked.as_ref()
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(peeked) = self.peeked.take() {
            return Some(peeked);
        }
        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let literal = &self.rest[..c.len_utf8()];
            let cur = self.rest;
            self.rest = chars.as_str();
            self.byte += c.len_utf8();

            enum Start {
                String,
                Slash,
                Ident,
                Number,
                IfEqualElse(TokenKind, TokenKind),
            }

            let process = |kind: TokenKind| Some(Ok(Token { kind, literal }));

            let started = match c {
                '(' => return process(TokenKind::LeftParen),
                ')' => return process(TokenKind::RightParen),
                '{' => return process(TokenKind::LeftBrace),
                '}' => return process(TokenKind::RightBrace),
                ',' => return process(TokenKind::Comma),
                '.' => return process(TokenKind::Dot),
                '-' => return process(TokenKind::Minus),
                '+' => return process(TokenKind::Plus),
                ';' => return process(TokenKind::Semicolon),
                '*' => return process(TokenKind::Star),
                '/' => Start::Slash,
                '!' => Start::IfEqualElse(TokenKind::BangEqual, TokenKind::Bang),
                '=' => Start::IfEqualElse(TokenKind::EqualEqual, TokenKind::Equal),
                '>' => Start::IfEqualElse(TokenKind::GreaterEqual, TokenKind::Greater),
                '<' => Start::IfEqualElse(TokenKind::LessEqual, TokenKind::Less),
                'a'..='z' | 'A'..='Z' | '_' => Start::Ident,
                '0'..='9' => Start::Number,
                '"' => Start::String,
                ' ' | '\r' | '\t' | '\n' => continue, // Skip whitespace
                c => {
                    return Some(Err(SingleTokenError {
                        src: NamedSource::new(
                            self.filename.unwrap_or("<input>"),
                            self.whole.to_string(),
                        ),
                        bad_bit: SourceSpan::from(self.byte - c.len_utf8()..self.byte),
                        token: c,
                    }
                    .into()));
                }
            };

            match started {
                Start::String => {
                    // different from Jon Gjengset, I just play a cheat. awa
                    if let Some(end) = self.rest.find('"') {
                        let literal = &self.rest[..end];
                        self.byte += end + 1;
                        self.rest = &self.rest[end + 1..];
                        return Some(Ok(Token {
                            kind: TokenKind::String,
                            literal,
                        }));
                    } else {
                        return Some(Err(StringTerminationError {
                            src: NamedSource::new(
                                self.filename.unwrap_or("<input>"),
                                self.whole.to_string(),
                            ),
                            bad_line: SourceSpan::from(self.byte - c.len_utf8()..self.whole.len()),
                        }
                        .into()));
                    }
                }
                Start::Slash => {
                    if self.rest.starts_with('/') {
                        let new_line = self.rest.find('\n').unwrap_or(self.rest.len());
                        self.byte += new_line;
                        self.rest = &self.rest[new_line..];
                        continue; // Skip single-line comment
                    } else {
                        return process(TokenKind::Slash);
                    }
                }
                Start::Ident => {
                    let first_non_ident = cur
                        .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
                        .unwrap_or(cur.len());

                    let literal = &cur[..first_non_ident];

                    let extra_bytes = literal.len() - c.len_utf8();
                    self.byte += extra_bytes;
                    self.rest = &self.rest[extra_bytes..];

                    let kind = match literal {
                        "and" => TokenKind::And,
                        "class" => TokenKind::Class,
                        "else" => TokenKind::Else,
                        "false" => TokenKind::False,
                        "for" => TokenKind::For,
                        "fun" => TokenKind::Fun,
                        "if" => TokenKind::If,
                        "nil" => TokenKind::Nil,
                        "or" => TokenKind::Or,
                        "return" => TokenKind::Return,
                        "super" => TokenKind::Super,
                        "this" => TokenKind::This,
                        "true" => TokenKind::True,
                        "var" => TokenKind::Var,
                        "while" => TokenKind::While,
                        "print" => TokenKind::Print,
                        _ => TokenKind::Ident,
                    };

                    return Some(Ok(Token { kind, literal }));
                }
                Start::Number => {
                    let first_non_digit = cur
                        .find(|c| !matches!(c, '0'..='9' | '.'))
                        .unwrap_or(cur.len());

                    let mut literal = &cur[..first_non_digit];

                    let mut dotted = literal.splitn(3, '.');
                    match (dotted.next(), dotted.next(), dotted.next()) {
                        (Some(one), Some(two), Some(_)) => {
                            literal = &literal[..one.len() + two.len() + 1]
                        }
                        (Some(one), Some(""), None) => literal = &literal[..one.len()],
                        _ => {}
                    };

                    let extra_bytes = literal.len() - c.len_utf8();
                    self.byte += extra_bytes;
                    self.rest = &self.rest[extra_bytes..];

                    let n = match literal.parse() {
                        Ok(n) => n,
                        Err(e) => {
                            return Some(Err(miette!(
                                code = "ParseFloatError",
                                url =
                                    "https://doc.rust-lang.org/std/num/struct.ParseFloatError.html",
                                labels = vec![LabeledSpan::at(
                                    self.byte - literal.len()..self.byte,
                                    "this numeric literal"
                                )],
                                "{e}",
                            )
                            .with_source_code(self.whole.to_string())));
                        }
                    };

                    return Some(Ok(Token {
                        kind: TokenKind::Number(n),
                        literal,
                    }));
                }
                Start::IfEqualElse(yes, no) => {
                    self.rest = self.rest.trim_start();
                    // example: =    =
                    //          cur.len() = 6
                    //          c.len_utf8() = 1
                    //          self.rest.len() = 1
                    //          trimmed = cur.len() - self.rest.len() - c.len_utf8() = 6 - 1 - 1 = 4
                    //          but c is 1 byte, so we need to add 1 to the trimmed length
                    //          so we can get the span of the token
                    let trimmed = cur.len() - self.rest.len() - 1;
                    self.byte += trimmed;
                    if self.rest.starts_with('=') {
                        let span = &cur[..c.len_utf8() + trimmed + 1];
                        self.rest = &self.rest[1..];
                        self.byte += 1;
                        return Some(Ok(Token {
                            kind: yes,
                            literal: span,
                        }));
                    } else {
                        return Some(Ok(Token { kind: no, literal }));
                    }
                }
            }
        }
    }
}
