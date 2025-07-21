#![allow(unused)]

use std::fmt::Display;

use miette::{Error, LabeledSpan, WrapErr, miette};

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'de> {
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
    String(&'de str),
    Ident(&'de str),
    Number(&'de str, f64),
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
        match self {
            Token::LeftParen => write!(f, "LEFT_PAREN ( null"),
            Token::RightParen => write!(f, "RIGHT_PAREN ) null"),
            Token::LeftBrace => write!(f, "LEFT_BRACE {{ null"),
            Token::RightBrace => write!(f, "RIGHT_BRACE }} null"),
            Token::Comma => write!(f, "COMMA , null"),
            Token::Dot => write!(f, "DOT . null"),
            Token::Minus => write!(f, "MINUS - null"),
            Token::Plus => write!(f, "PLUS + null"),
            Token::Semicolon => write!(f, "SEMICOLON ; null"),
            Token::Star => write!(f, "STAR * null"),
            Token::BangEqual => write!(f, "BANG_EQUAL != null"),
            Token::EqualEqual => write!(f, "EQUAL_EQUAL == null"),
            Token::GreaterEqual => write!(f, "GREATER_EQUAL >= null"),
            Token::LessEqual => write!(f, "LESS_EQUAL <= null"),
            Token::Greater => write!(f, "GREATER > null"),
            Token::Less => write!(f, "LESS < null"),
            Token::Slash => write!(f, "SLASH / null"),
            Token::Bang => write!(f, "BANG ! null"),
            Token::Equal => write!(f, "EQUAL = null"),
            Token::String(_) => todo!(),
            Token::Ident(i) => write!(f, "IDENTIFIER {i} null"),
            Token::Number(lit, n) => write!(f, "NUMBER {lit} {n}"),
            Token::And => write!(f, "AND and null"),
            Token::Class => write!(f, "CLASS class null"),
            Token::Else => write!(f, "ELSE else null"),
            Token::False => write!(f, "FALSE false null"),
            Token::For => write!(f, "FOR for null"),
            Token::Fun => write!(f, "FUN fun null"),
            Token::If => write!(f, "IF if null"),
            Token::Nil => write!(f, "NIL nil null"),
            Token::Or => write!(f, "OR or null"),
            Token::Return => write!(f, "RETURN return null"),
            Token::Super => write!(f, "SUPER super null"),
            Token::This => write!(f, "THIS this null"),
            Token::True => write!(f, "TRUE true null"),
            Token::Var => write!(f, "VAR var null"),
            Token::While => write!(f, "WHILE while null"),
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
        let mut chars = self.rest.chars();
        let c = chars.next()?;
        self.rest = chars.as_str();
        self.byte += c.len_utf8();

        enum Start<'de> {
            String,
            Ident,
            Number,
            IfEqualElse(Token<'de>, Token<'de>),
        }

        let started = match c {
            '(' => return Some(Ok(Token::LeftParen)),
            ')' => return Some(Ok(Token::RightParen)),
            '{' => return Some(Ok(Token::LeftBrace)),
            '}' => return Some(Ok(Token::RightBrace)),
            ',' => return Some(Ok(Token::Comma)),
            '.' => return Some(Ok(Token::Dot)),
            '-' => return Some(Ok(Token::Minus)),
            '+' => return Some(Ok(Token::Plus)),
            ';' => return Some(Ok(Token::Semicolon)),
            '*' => return Some(Ok(Token::Star)),
            '/' => return Some(Ok(Token::Slash)),
            '!' => Start::IfEqualElse(Token::BangEqual, Token::Bang),
            '=' => Start::IfEqualElse(Token::EqualEqual, Token::Equal),
            '>' => Start::IfEqualElse(Token::GreaterEqual, Token::Greater),
            '<' => Start::IfEqualElse(Token::LessEqual, Token::Less),
            'a'..='z' | 'A'..='Z' | '_' => Start::Ident,
            '0'..='9' => Start::Number,
            '"' => Start::String,
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
                if self.rest.starts_with('=') {
                    self.rest = &self.rest[1..];
                    self.byte += 1;
                    return Some(Ok(yes));
                } else {
                    return Some(Ok(no));
                }
            }
        }
        None
    }
}
