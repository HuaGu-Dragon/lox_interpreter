#![allow(dead_code)]

use std::fmt::Display;

use miette::{Context, Error, LabeledSpan};

use crate::{
    Lexer,
    lex::{Eof, Token, TokenKind},
};

pub struct Parser<'de> {
    pub whole: &'de str,
    pub lexer: Lexer<'de>,
}

pub enum StatementTree<'de> {
    Block(Vec<StatementTree<'de>>),
    Expression(TokenTree<'de>),
    Fun {
        name: Atom<'de>,
        params: Vec<Token<'de>>,
        body: Box<StatementTree<'de>>,
    },
    If {
        condition: Box<TokenTree<'de>>,
        then_branch: Box<StatementTree<'de>>,
        else_branch: Option<Box<StatementTree<'de>>>,
    },
    For {
        init: Box<StatementTree<'de>>,
        condition: Box<TokenTree<'de>>,
        increment: Box<TokenTree<'de>>,
        body: Box<StatementTree<'de>>,
    },
    While {
        condition: Box<TokenTree<'de>>,
        body: Box<StatementTree<'de>>,
    },
    Class {
        name: Atom<'de>,
        father: Option<Atom<'de>>,
        body: Box<StatementTree<'de>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenTree<'de> {
    Atom(Atom<'de>),
    Cons(Op, Vec<TokenTree<'de>>),
    Call {
        callee: Box<TokenTree<'de>>,
        arguments: Vec<TokenTree<'de>>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Atom<'de> {
    String(&'de str),
    Number(f64),
    Nil,
    Boolean(bool),
    Ident(&'de str, usize),
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
    Call,
}

impl<'de> Iterator for Parser<'de> {
    type Item = Result<StatementTree<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.lexer.peek().is_some() {
            Some(self.parse_statement_within())
        } else {
            None
        }
    }
}

impl<'de> Parser<'de> {
    pub fn new(filename: Option<&'de str>, whole: &'de str) -> Self {
        Parser {
            whole,
            lexer: Lexer::new(filename, whole),
        }
    }

    pub fn parse(mut self) -> Result<StatementTree<'de>, Error> {
        self.parse_statement_within()
    }

    pub fn parse_expr(mut self) -> Result<TokenTree<'de>, Error> {
        self.parse_expression_within(0)
    }

    pub fn parse_block(&mut self) -> Result<StatementTree<'de>, Error> {
        self.lexer.expect(TokenKind::LeftBrace, "Expected '{'")?;

        let mut block = Vec::new();
        while !matches!(
            self.lexer.peek(),
            Some(Ok(Token {
                kind: TokenKind::RightBrace,
                ..
            }))
        ) {
            let statement = self
                .parse_statement_within()
                .wrap_err("parsing block statement")?;
            block.push(statement);
        }

        self.lexer.expect(TokenKind::RightBrace, "Expected '}''")?;

        Ok(StatementTree::Block(block))
    }

    pub fn parse_fun_call(&mut self) -> Result<Vec<TokenTree<'de>>, Error> {
        let mut arguments = Vec::new();

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
                let argument = self.parse_expression_within(0).wrap_err_with(|| {
                    format!("in argument #{} of function call", arguments.len() + 1)
                })?;
                arguments.push(argument);

                let token = self.lexer.expect_where(
                    |token| matches!(token.kind, TokenKind::Comma | TokenKind::RightParen),
                    "Expected ',' or ')' after function argument",
                )?;

                if token.kind == TokenKind::RightParen {
                    break;
                }
            }
        }
        Ok(arguments)
    }

    pub fn parse_statement_within(&mut self) -> Result<StatementTree<'de>, Error> {
        let statement = if matches!(
            self.lexer.peek(),
            Some(Ok(Token {
                kind: TokenKind::Class
                    | TokenKind::If
                    | TokenKind::Fun
                    | TokenKind::For
                    | TokenKind::Var
                    | TokenKind::While
                    | TokenKind::Print
                    | TokenKind::Return
                    | TokenKind::Semicolon
                    | TokenKind::LeftBrace,
                ..
            }))
        ) {
            if let Some(Ok(Token {
                kind: TokenKind::LeftBrace,
                ..
            })) = self.lexer.peek()
            {
                return self.parse_block();
            }
            let statement = match self.lexer.next() {
                Some(Ok(token)) => token,
                None => {
                    return Ok(StatementTree::Expression(TokenTree::Atom(Atom::Nil)));
                }
                Some(Err(e)) => {
                    return Err(e).wrap_err("Error while parsing statement");
                }
            };

            match statement {
                Token {
                    kind: TokenKind::Semicolon,
                    ..
                } => {
                    return Ok(StatementTree::Expression(TokenTree::Atom(Atom::Nil)));
                }
                Token {
                    kind: TokenKind::Return | TokenKind::Print,
                    ..
                } => {
                    let op = match statement.kind {
                        TokenKind::Return => Op::Return,
                        TokenKind::Print => Op::Print,
                        _ => unreachable!(),
                    };
                    let ((), r_bp) = prefix_binding_power(op);
                    let expression = self
                        .parse_expression_within(r_bp)
                        .wrap_err("parsing expression")?;
                    StatementTree::Expression(TokenTree::Cons(op, vec![expression]))
                }

                Token {
                    kind: TokenKind::For,
                    ..
                } => {
                    self.lexer
                        .expect(TokenKind::LeftParen, "Expected '(' after 'for'")
                        .wrap_err("in for loop condition")?;

                    let init = self
                        .parse_statement_within()
                        .wrap_err_with(|| "in init condition of for loop".to_string())?;

                    let cond = if matches!(
                        self.lexer.peek(),
                        Some(Ok(Token {
                            kind: TokenKind::Semicolon,
                            ..
                        }))
                    ) {
                        TokenTree::Atom(Atom::Boolean(true))
                    } else {
                        self.parse_expression_within(0)
                            .wrap_err_with(|| "in loop condition of for loop".to_string())?
                    };

                    self.lexer
                        .expect(
                            TokenKind::Semicolon,
                            "Expected ';' after for loop condition",
                        )
                        .wrap_err("in for loop condition")?;

                    let inc = if matches!(
                        self.lexer.peek(),
                        Some(Ok(Token {
                            kind: TokenKind::RightParen,
                            ..
                        }))
                    ) {
                        TokenTree::Atom(Atom::Nil)
                    } else {
                        self.parse_expression_within(0)
                            .wrap_err_with(|| "in incremental condition of for loop".to_string())?
                    };
                    self.lexer
                        .expect(TokenKind::RightParen, "Expected ')' after 'for'")
                        .wrap_err("in for loop condition")?;

                    let block = self.parse_block().wrap_err("in body of for loop")?;

                    return Ok(StatementTree::For {
                        init: Box::new(init),
                        condition: Box::new(cond),
                        increment: Box::new(inc),
                        body: Box::new(block),
                    });
                }

                Token {
                    kind: TokenKind::While,
                    ..
                } => {
                    self.lexer
                        .expect(TokenKind::LeftParen, "Expected '(' after 'while'")
                        .wrap_err("in while loop condition")?;

                    let cond = self
                        .parse_expression_within(0)
                        .wrap_err_with(|| "in condition of while loop".to_string())?;

                    self.lexer
                        .expect(TokenKind::RightParen, "Expected ')' after 'while'")
                        .wrap_err("in while loop condition")?;

                    let block = self.parse_block().wrap_err("in body of while loop")?;

                    return Ok(StatementTree::While {
                        condition: Box::new(cond),
                        body: Box::new(block),
                    });
                    // return Ok(TokenTree::Cons(Op::While, vec![cond, block]));
                }

                Token {
                    kind: TokenKind::Class,
                    ..
                } => {
                    let token = self
                        .lexer
                        .expect(TokenKind::Ident, "Expected identifier")
                        .wrap_err("in class name")?;
                    let _ident = TokenTree::Atom(Atom::Ident(token.literal, self.lexer.byte));

                    // let class_block = self.parse_class_body().wrap_err("in class body")?;

                    // TODO: support inheritance
                    todo!()
                    // let block = self.parse_expression_within(0).wrap_err("in class body")?;
                    // let block = self.parse_block().wrap_err("in class definition")?;

                    // return Ok(StatementTree::Expression(TokenTree::Cons(
                    //     Op::Class,
                    //     vec![ident, block],
                    // )));
                }
                Token {
                    kind: TokenKind::Var,
                    ..
                } => {
                    let token = self
                        .lexer
                        .expect(TokenKind::Ident, "Expected identifier")
                        .wrap_err("in variable assignment")?;

                    let ident = TokenTree::Atom(Atom::Ident(token.literal, self.lexer.byte));

                    // TODO: Handle nil Var declaration

                    self.lexer
                        .expect(TokenKind::Equal, "Expected '=' after variable name")
                        .wrap_err("in variable assignment")?;

                    let second = self
                        .parse_expression_within(0)
                        .wrap_err("in variable assignment expression")?;

                    StatementTree::Expression(TokenTree::Cons(Op::Var, vec![ident, second]))
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
                    let ident = Atom::Ident(token.literal, self.lexer.byte);

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
                            params.push(param);

                            let token = self
                                .lexer
                                .expect_where(
                                    |token| {
                                        matches!(
                                            token.kind,
                                            TokenKind::Comma | TokenKind::RightParen
                                        )
                                    },
                                    "Expected ',' or ')' after function parameter",
                                )
                                .wrap_err_with(|| {
                                    format!("in parameter list of function {name}")
                                })?;

                            if token.kind == TokenKind::RightParen {
                                break;
                            }
                        }
                    }

                    let body = self
                        .parse_block()
                        .wrap_err_with(|| format!("in body of function {name}"))?;

                    return Ok(StatementTree::Fun {
                        name: ident,
                        params,
                        body: Box::new(body),
                    });
                }

                Token {
                    kind: TokenKind::If,
                    ..
                } => {
                    self.lexer
                        .expect(TokenKind::LeftParen, "Expected '(' after 'if'")
                        .wrap_err("in if condition")?;

                    let cond = self
                        .parse_expression_within(0)
                        .wrap_err("in if condition")?;

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
                        let else_block =
                            self.parse_block().wrap_err("in body of else statement")?;
                        otherwise = Some(else_block);
                    }

                    return Ok(StatementTree::If {
                        condition: Box::new(cond),
                        then_branch: Box::new(block),
                        else_branch: otherwise.map(Box::new),
                    });
                }

                token => {
                    return Err(miette::miette! {
                        labels = vec![
                            LabeledSpan::at(
                                self.lexer.byte - token.literal.len()..self.lexer.byte,
                                "here",
                            )
                        ],
                        "expected a statement"
                    }
                    .with_source_code(self.whole.to_string()));
                }
            }
        } else {
            let expr = self
                .parse_expression_within(0)
                .wrap_err("parse expression statement");

            StatementTree::Expression(expr.wrap_err("in expression statement")?)
        };
        self.lexer
            .expect(TokenKind::Semicolon, "In the end of statement expected ';'")?;
        Ok(statement)
    }
    pub fn parse_expression_within(&mut self, min_bp: u8) -> Result<TokenTree<'de>, Error> {
        let lhs = match self.lexer.next() {
            Some(Ok(token)) => token,
            None => {
                return Err(Eof::build(&self.lexer).into());
            }
            Some(Err(e)) => {
                return Err(e).wrap_err("Error while parsing left-hand side of expression");
            }
        };

        let mut lhs = match lhs {
            Token {
                kind: TokenKind::LeftParen,
                ..
            } => {
                let lhs = self
                    .parse_expression_within(0)
                    .wrap_err("in bracketed expression")?;

                self.lexer
                    .expect(TokenKind::RightParen, "Unexpected end of expression")
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
            } => TokenTree::Atom(Atom::Ident(literal, self.lexer.byte)),
            Token {
                kind: TokenKind::Super,
                ..
            } => TokenTree::Atom(Atom::Super),
            Token {
                kind: TokenKind::This,
                ..
            } => TokenTree::Atom(Atom::This),

            Token {
                kind: TokenKind::Minus | TokenKind::Bang | TokenKind::Plus,
                ..
            } => {
                let op = match lhs.kind {
                    TokenKind::Minus => Op::Minus,
                    TokenKind::Bang => Op::Bang,
                    TokenKind::Plus => Op::Plus,
                    _ => unreachable!(),
                };
                let ((), r_bp) = prefix_binding_power(op);
                let rhs = self
                    .parse_expression_within(r_bp)
                    .wrap_err("in right-hand side")?;
                TokenTree::Cons(op, vec![rhs])
            }

            token => {
                return Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(
                            self.lexer.byte - lhs.literal.len()..self.lexer.byte,
                            "here",
                        )
                    ],
                    "unexpected token in expression: {token:?}",
                }
                .with_source_code(self.whole.to_string()));
            }
        };

        loop {
            let bytes = self.lexer.byte;
            let op = self.lexer.peek();
            if op.is_some_and(|op| op.is_err()) {
                return Err(self
                    .lexer
                    .next()
                    .expect("Error while lexing")
                    .expect_err("peeked token")
                    .wrap_err("in place of operator"));
            }
            let op = match op.map(|res| res.as_ref().expect("peeked token")) {
                None => break,
                Some(Token {
                    kind: TokenKind::LeftParen,
                    ..
                }) => Op::Call,
                Some(Token {
                    kind: TokenKind::And,
                    ..
                }) => Op::And,
                Some(Token {
                    kind: TokenKind::BangEqual,
                    ..
                }) => Op::BangEqual,
                Some(Token {
                    kind: TokenKind::Dot,
                    ..
                }) => Op::Field,
                Some(Token {
                    kind: TokenKind::EqualEqual,
                    ..
                }) => Op::EqualEqual,
                Some(Token {
                    kind: TokenKind::Equal,
                    ..
                }) => Op::Equal,
                Some(Token {
                    kind: TokenKind::Greater,
                    ..
                }) => Op::Greater,
                Some(Token {
                    kind: TokenKind::GreaterEqual,
                    ..
                }) => Op::GreaterEqual,
                Some(Token {
                    kind: TokenKind::Less,
                    ..
                }) => Op::Less,
                Some(Token {
                    kind: TokenKind::LessEqual,
                    ..
                }) => Op::LessEqual,
                Some(Token {
                    kind: TokenKind::Or,
                    ..
                }) => Op::Or,
                Some(Token {
                    kind: TokenKind::Plus,
                    ..
                }) => Op::Plus,
                Some(Token {
                    kind: TokenKind::Minus,
                    ..
                }) => Op::Minus,
                Some(Token {
                    kind: TokenKind::Slash,
                    ..
                }) => Op::Slash,
                Some(Token {
                    kind: TokenKind::Star,
                    ..
                }) => Op::Star,
                Some(Token {
                    kind:
                        TokenKind::RightParen
                        | TokenKind::RightBrace
                        | TokenKind::Semicolon
                        | TokenKind::Comma,
                    ..
                }) => break,
                Some(token) => {
                    return Err(miette::miette! {
                        help = "expected an operator",
                        labels = vec![
                            LabeledSpan::at(
                                bytes..bytes + token.literal.len(),
                                "here",
                            )
                        ],
                        "unexpected token in expression: {token:?}",
                    }
                    .with_source_code(self.whole.to_string()));
                }
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                lhs = match op {
                    Op::Call => TokenTree::Call {
                        callee: Box::new(lhs),
                        arguments: self
                            .parse_fun_call()
                            .wrap_err("in argument list of function call")?,
                    },
                    _ => TokenTree::Cons(op, vec![lhs]),
                };
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                let rhs = self
                    .parse_expression_within(r_bp)
                    .wrap_err("in right-hand side of infix expression")?;
                lhs = TokenTree::Cons(op, vec![lhs, rhs]);
                continue;
            }

            break;
        }
        Ok(lhs)
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Minus => "-",
                Op::Plus => "+",
                Op::Star => "*",
                Op::BangEqual => "!=",
                Op::EqualEqual => "==",
                Op::GreaterEqual => ">=",
                Op::LessEqual => "<=",
                Op::Greater => ">",
                Op::Less => "<",
                Op::Slash => "/",
                Op::Bang => "!",
                Op::Equal => "=",
                Op::And => "and",
                Op::Or => "or",
                Op::Class => "class",
                Op::If => "if",
                Op::For => "for",
                Op::Fun => "fun",
                Op::Print => "print",
                Op::Return => "return",
                Op::Field => ".",
                Op::Var => "var",
                Op::While => "while",
                Op::Call => "call",
            }
        )
    }
}

impl Display for Atom<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Atom::String(s) => write!(f, "{s}"),
            Atom::Number(n) => write!(f, "{n}"),
            Atom::Nil => write!(f, "nil"),
            Atom::Boolean(b) => write!(f, "{b:?}"),
            Atom::Ident(i, _) => write!(f, "{i}"),
            Atom::Super => write!(f, "super"),
            Atom::This => write!(f, "this"),
        }
    }
}

impl Display for TokenTree<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenTree::Atom(atom) => write!(f, "{atom}"),
            TokenTree::Cons(op, token_trees) => {
                write!(f, "({op}")?;
                for s in token_trees {
                    write!(f, " {s}")?;
                }
                write!(f, ")")
            }
            TokenTree::Call { callee, arguments } => {
                write!(f, "({callee}")?;
                for arg in arguments {
                    write!(f, " {arg}")?;
                }
                write!(f, ")")
            }
        }
    }
}

impl Display for StatementTree<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StatementTree::Fun { name, params, body } => {
                write!(f, "(def {name}")?;
                for para in params {
                    write!(f, " {para}")?;
                }
                write!(f, " {body})")
            }
            StatementTree::If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(f, "(if {condition} {then_branch}")?;
                if let Some(else_branch) = else_branch {
                    write!(f, " {else_branch}")?;
                }
                write!(f, ")")
            }
            StatementTree::Block(token_trees) => {
                write!(f, "{{")?;
                for tree in token_trees {
                    write!(f, " {tree}")?;
                }
                write!(f, " }}")
            }
            StatementTree::Expression(token_tree) => write!(f, "{token_tree}"),
            StatementTree::For {
                init,
                condition,
                increment,
                body,
            } => write!(f, "(for {init} {condition} {increment} {body})"),
            StatementTree::While { condition, body } => write!(f, "(while {condition} {body})"),
            // TODO: support inheritance
            StatementTree::Class { name, father, body } => todo!(),
        }
    }
}

fn prefix_binding_power(op: Op) -> ((), u8) {
    match op {
        Op::Plus | Op::Minus | Op::Bang => ((), 9),
        Op::Print | Op::Return => ((), 0),
        _ => todo!(),
    }
}

fn infix_binding_power(op: Op) -> Option<(u8, u8)> {
    let res = match op {
        Op::Equal => (0, 1),
        Op::And | Op::Or => (1, 2),
        Op::EqualEqual
        | Op::BangEqual
        | Op::Less
        | Op::LessEqual
        | Op::Greater
        | Op::GreaterEqual => (3, 4),
        Op::Plus | Op::Minus => (5, 6),
        Op::Star | Op::Slash => (7, 8),
        Op::Field => (11, 10),
        _ => return None,
    };
    Some(res)
}

fn postfix_binding_power(op: Op) -> Option<(u8, ())> {
    let res = match op {
        Op::Call => (9, ()),
        _ => return None,
    };
    Some(res)
}
