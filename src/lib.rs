#![allow(unused)]

use std::fmt::Display;

use miette::{Error, LabeledSpan, WrapErr, miette};

pub struct Token<'de> {
    kind: TokenKind,
    literal: &'de str,
}

#[derive(Debug, Clone, PartialEq)]
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
            TokenKind::String => todo!(),
            TokenKind::Ident => write!(f, "IDENTIFIER {lit} null"),
            TokenKind::Number(n) => write!(f, "NUMBER {lit} {n}"),
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
        }
    }
}

pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    byte: usize,
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Lexer {
            whole: input,
            rest: input,
            byte: 0,
        }
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let literal = &self.rest[..c.len_utf8()];
            let cur = self.rest;
            self.rest = chars.as_str();
            self.byte += c.len_utf8();

            enum Start {
                String,
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
                '/' => return process(TokenKind::Slash),
                '!' => Start::IfEqualElse(TokenKind::BangEqual, TokenKind::Bang),
                '=' => Start::IfEqualElse(TokenKind::EqualEqual, TokenKind::Equal),
                '>' => Start::IfEqualElse(TokenKind::GreaterEqual, TokenKind::Greater),
                '<' => Start::IfEqualElse(TokenKind::LessEqual, TokenKind::Less),
                'a'..='z' | 'A'..='Z' | '_' => Start::Ident,
                '0'..='9' => Start::Number,
                '"' => Start::String,
                ' ' | '\r' | '\t' => continue, // Skip whitespace
                c => {
                    return Some(Err(miette!(
                        labels = vec![LabeledSpan::at(
                            self.byte - c.len_utf8()..self.byte,
                            "this character"
                        )],
                        "Unexpected token '{c}' in input"
                    )
                    .with_source_code(self.whole.to_string())));
                }
            };

            match started {
                Start::String => todo!(),
                Start::Ident => todo!(),
                Start::Number => todo!(),
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
