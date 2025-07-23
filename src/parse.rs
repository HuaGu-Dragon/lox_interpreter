#![allow(dead_code)]

use std::iter::Peekable;

use miette::{Context, Error};

use crate::{
    Lexer,
    lex::{Token, TokenKind},
};

pub struct Parser<'de> {
    whole: &'de str,
    lexer: Peekable<Lexer<'de>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenTree<'de> {
    Atom(Atom<'de>),
    Cons(Op, Vec<TokenTree<'de>>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Atom<'de> {
    String(&'de str),
    Number(f64),
    Nil,
    Boolean(bool),
    Ident(&'de str),
    Super,
    This,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Minus,
    Plus,
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
    And,
    Or,
    Class,
    If,
    For,
    Fun,
    Print,
    Return,
    Field,
    Var,
    While,
}

impl<'de> Parser<'de> {
    pub fn new(filename: Option<&'de str>, whole: &'de str) -> Self {
        Parser {
            whole,
            lexer: Lexer::new(filename, whole).peekable(),
        }
    }

    pub fn parse(mut self) -> Result<TokenTree<'de>, Error> {
        self.parse_within(0)
    }

    pub fn parse_within(&mut self, min_bp: u8) -> Result<TokenTree<'de>, Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => {
                return Ok(TokenTree::Atom(Atom::Nil));
            }
            Some(Err(e)) => {
                return Err(miette::miette!("Error while lexing: {}", e));
            }
        };

        let lhs = match lhs {
            Token {
                kind: TokenKind::LeftParen | TokenKind::LeftBrace,
                ..
            } => {
                let terminator = match lhs.kind {
                    TokenKind::LeftParen => TokenKind::RightParen,
                    TokenKind::LeftBrace => TokenKind::RightBrace,
                    _ => unreachable!(),
                };
                let lhs = self.parse_within(0)?;
                // assert_eq!(self.lexer.next().k, terminator);
                todo!()
            }

            Token {
                kind: TokenKind::String,
                literal,
            } => TokenTree::Atom(Atom::String(literal)),
            Token {
                kind: TokenKind::Number(n),
                ..
            } => TokenTree::Atom(Atom::Number(n)),
            Token {
                kind: TokenKind::Nil,
                ..
            } => TokenTree::Atom(Atom::Nil),
            Token {
                kind: TokenKind::True,
                ..
            } => TokenTree::Atom(Atom::Boolean(true)),
            Token {
                kind: TokenKind::False,
                ..
            } => TokenTree::Atom(Atom::Boolean(false)),
            Token {
                kind: TokenKind::Ident,
                literal,
            } => TokenTree::Atom(Atom::Ident(literal)),
            Token {
                kind: TokenKind::Super,
                ..
            } => TokenTree::Atom(Atom::Super),
            Token {
                kind: TokenKind::This,
                ..
            } => TokenTree::Atom(Atom::This),

            Token {
                kind:
                    TokenKind::Minus
                    | TokenKind::Bang
                    | TokenKind::Return
                    | TokenKind::Plus
                    | TokenKind::Print,
                ..
            } => {
                let op = match lhs.kind {
                    TokenKind::Minus => Op::Minus,
                    TokenKind::Bang => Op::Bang,
                    TokenKind::Return => Op::Return,
                    TokenKind::Plus => Op::Plus,
                    TokenKind::Print => Op::Print,
                    _ => unreachable!(),
                };
                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self.parse_within(r_bp).wrap_err("parse RHS")?;
                TokenTree::Cons(op, vec![rhs])
            }

            Token {
                kind: TokenKind::For | TokenKind::While | TokenKind::Class | TokenKind::Var,
                ..
            } => {
                let op = match lhs.kind {
                    TokenKind::For => Op::For,
                    TokenKind::While => Op::While,
                    TokenKind::Class => Op::Class,
                    TokenKind::Var => Op::Var,
                    _ => unreachable!(),
                };
                let first = self.parse_within(0).wrap_err("parse first")?;

                let second = self.parse_within(0).wrap_err("parse second")?;
                TokenTree::Cons(op, vec![first, second])
            }

            Token {
                kind: TokenKind::Fun,
                ..
            } => {
                let op = match lhs.kind {
                    TokenKind::Fun => Op::Fun,
                    _ => unreachable!(),
                };
                let first = self.parse_within(0).wrap_err("parse first")?;
                let second = self.parse_within(0).wrap_err("parse second")?;
                let third = self.parse_within(0).wrap_err("parse third")?;
                TokenTree::Cons(op, vec![first, second, third])
            }
            _ => todo!(),
        };

        loop {
            let op = self.lexer.peek();
            if op.map_or(false, |op| op.is_err()) {
                return Err(miette::miette!("Error while lexing"));
            }
            let op = match op.map(|res| res.as_ref().expect("peeked token")) {
                None => break,
                Some(_) => todo!(),
            };
        }
        todo!()
    }
}

fn prefix_binding_power(op: Op) -> ((), u8) {
    match op {
        Op::Minus | Op::Bang => ((), 7),
        _ => todo!(),
    }
}
