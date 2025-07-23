#![allow(dead_code)]

use miette::{Context, Error};

use crate::{
    Lexer,
    lex::{Token, TokenKind},
};

pub struct Parser<'de> {
    whole: &'de str,
    lexer: Lexer<'de>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenTree<'de> {
    Atom(Atom<'de>),
    Cons(Op, Vec<TokenTree<'de>>),
    Fun(Atom<'de>, Vec<TokenTree<'de>>, TokenTree<'de>),
    If(TokenTree<'de>, TokenTree<'de>, Option<TokenTree<'de>>),
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
            lexer: Lexer::new(filename, whole),
        }
    }

    pub fn parse(mut self) -> Result<TokenTree<'de>, Error> {
        self.parse_within(0)
    }

    pub fn parse_block(&mut self) -> Result<TokenTree<'de>, Error> {
        self.lexer.expect(TokenKind::LeftBrace, "Expected '{'")?;

        let block = self.parse_within(0)?;

        self.lexer.expect(TokenKind::RightBrace, "Expected '}''")?;

        Ok(block)
    }
    pub fn parse_within(&mut self, min_bp: u8) -> Result<TokenTree<'de>, Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => {
                return Ok(TokenTree::Atom(Atom::Nil));
            }
            Some(Err(e)) => {
                return Err(e).wrap_err("Error while parsing left-hand side of expression");
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
                let lhs = self.parse_within(0).wrap_err("in bracketed expression")?;

                self.lexer
                    .expect(terminator, "Unexpected end of expression")
                    .wrap_err("after parsing bracketed expression")?;
                lhs
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
                let rhs = self.parse_within(r_bp).wrap_err("in right-hand side")?;
                TokenTree::Cons(op, vec![rhs])
            }

            Token {
                kind: TokenKind::For,
                ..
            } => {
                self.lexer
                    .expect(TokenKind::LeftParen, "Expected '(' after 'for'")
                    .wrap_err("in for loop condition")?;

                let init = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in init condition of for loop"))?;

                self.lexer
                    .expect(TokenKind::Semicolon, "Expected ';' after for loop init")
                    .wrap_err("in for loop condition")?;

                let cond = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in loop condition of for loop"))?;

                self.lexer
                    .expect(
                        TokenKind::Semicolon,
                        "Expected ';' after for loop condition",
                    )
                    .wrap_err("in for loop condition")?;

                let inc = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in incremental condition of for loop"))?;

                self.lexer
                    .expect(TokenKind::RightParen, "Expected ')' after 'for'")
                    .wrap_err("in for loop condition")?;

                let block = self.parse_block().wrap_err("in body of for loop")?;

                TokenTree::Cons(Op::For, vec![init, cond, inc, block])
            }

            Token {
                kind: TokenKind::While,
                ..
            } => {
                self.lexer
                    .expect(TokenKind::LeftParen, "Expected '(' after 'while'")
                    .wrap_err("in while loop condition")?;

                let cond = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in condition of while loop"))?;

                self.lexer
                    .expect(TokenKind::RightParen, "Expected ')' after 'while'")
                    .wrap_err("in while loop condition")?;

                let block = self.parse_block().wrap_err("in body of while loop")?;

                TokenTree::Cons(Op::While, vec![cond, block])
            }

            Token {
                kind: TokenKind::Class | TokenKind::Var,
                ..
            } => {
                let op = match lhs.kind {
                    TokenKind::Class => Op::Class,
                    TokenKind::Var => Op::Var,
                    _ => unreachable!(),
                };

                let token = self
                    .lexer
                    .expect(TokenKind::Ident, "Expected identifier")
                    .wrap_err_with(|| format!("in name declaration of {op:?}"))?;

                let ident = TokenTree::Atom(Atom::Ident(token.literal));

                if lhs.kind == TokenKind::Var {
                    self.lexer
                        .expect(TokenKind::Equal, "Expected '=' after variable name")
                        .wrap_err("in variable assignment")?;
                }

                let second = self
                    .parse_within(0)
                    .wrap_err_with(|| format!("in the second argument of {op:?}"))?;

                TokenTree::Cons(op, vec![ident, second])
            }

            Token {
                kind: TokenKind::Fun,
                ..
            } => {
                let token = self
                    .lexer
                    .expect(TokenKind::Ident, "Expected identifier")
                    .wrap_err("in name declaration of function")?;
                let name = token.literal;
                let ident = Atom::Ident(token.literal);

                self.lexer
                    .expect(TokenKind::LeftParen, "Expected '(' after function name")
                    .wrap_err_with(|| format!("in parameter list of function {name}"))?;

                let mut params = Vec::new();

                if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        kind: TokenKind::RightParen,
                        ..
                    }))
                ) {
                    self.lexer.next(); // Consume the right parenthesis
                } else {
                    loop {
                        let param = self
                            .lexer
                            .expect(TokenKind::Ident, "Expected identifier")
                            .wrap_err_with(|| {
                                format!("in parameter #{} of function {name}", params.len() + 1)
                            })?;
                        params.push(TokenTree::Atom(Atom::Ident(param.literal)));

                        self.lexer
                            .expect_where(
                                |token| {
                                    matches!(token.kind, TokenKind::Comma | TokenKind::RightParen)
                                },
                                "Expected ',' or ')' after function parameter",
                            )
                            .wrap_err_with(|| format!("in parameter list of function {name}"))?;

                        if token.kind == TokenKind::RightParen {
                            break;
                        }
                    }
                }

                let body = self
                    .parse_block()
                    .wrap_err_with(|| format!("in body of function {name}"))?;

                TokenTree::Fun(ident, params, body)
            }

            Token {
                kind: TokenKind::If,
                ..
            } => {
                self.lexer
                    .expect(TokenKind::LeftParen, "Expected '(' after 'if'")
                    .wrap_err("in if condition")?;

                let cond = self.parse_within(0).wrap_err("in if condition")?;

                self.lexer
                    .expect(TokenKind::RightParen, "Expected ')' after 'if'")
                    .wrap_err("in if condition")?;

                let block = self.parse_block().wrap_err("in body of if statement")?;

                let mut otherwise = None;
                if matches!(
                    self.lexer.peek(),
                    Some(Ok(Token {
                        kind: TokenKind::Else,
                        ..
                    }))
                ) {
                    self.lexer.next(); // Consume the 'else' token
                } else {
                    let else_block = self.parse_block().wrap_err("in body of else statement")?;
                    otherwise = Some(else_block);
                }

                TokenTree::If(cond, block, otherwise)
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
