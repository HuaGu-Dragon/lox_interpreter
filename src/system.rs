use std::{
    borrow::Cow,
    io::{Write, stdout},
};

use miette::{Error, miette};

use crate::{
    eval::Value,
    parse::{Atom, TokenTree},
};

pub fn input<'de>(message: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let message = message.iter().next();
    let Some(Value::Str(str)) = message else {
        return Err(miette!("expected a string"));
    };
    write!(stdout(), "{str}").map_err(|e| miette!("{e}"))?;
    stdout().flush().map_err(|e| miette!("{e}"))?;
    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .map_err(|e| miette!("{e}"))?;

    Ok(Value::Str(Cow::Owned(input.trim().to_string())))
}

pub fn max<'de>(input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut max = None;

    for value in input {
        let number = match value {
            Value::Number(n) => *n,
            _ => return Err(miette!("max only accepts numbers, got {value}")),
        };

        max = Some(match max {
            Some(current_max) if current_max > number => current_max,
            _ => number,
        });
    }

    match max {
        Some(max) => Ok(Value::Number(max)),
        None => Err(miette!("max requires at least one argument")),
    }
}

// TODO: Error message
pub fn number<'de>(input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Str(value)) => Ok(Value::Number(value.parse().map_err(|e| miette!("{e}"))?)),
        _ => Err(miette!("Not a string")),
    }
}

// TODO: Error message
pub fn to_string<'de>(input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Number(n)) => Ok(Value::Str(Cow::Owned(n.to_string()))),
        _ => Err(miette!("Not a number")),
    }
}

// TODO: Error message
pub fn write_expr<'de>(input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Str(expr)) => {
            let parser = crate::Parser::new(None, expr);
            let expr = match parser.parse_expr() {
                Ok(expr) => expr,
                Err(e) => {
                    if e.downcast_ref::<crate::lex::Eof>().is_some() {
                        eprintln!("Error: Unexpected end in write_expr");
                        eprintln!("{e:?}");
                    };
                    return Err(e);
                }
            };
            println!("{expr}");
            Ok(Value::Nil)
        }
        _ => Err(miette!("Not a expr")),
    }
}

// TODO: Error Message
pub fn merge_const<'de>(input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Str(expr)) => {
            let parser = crate::Parser::new(None, expr);
            let expr = match parser.parse_expr() {
                Ok(expr) => expr,
                Err(e) => {
                    if e.downcast_ref::<crate::lex::Eof>().is_some() {
                        eprintln!("Error: Unexpected end in write_expr");
                        eprintln!("{e:?}");
                    };
                    return Err(e);
                }
            };
            // merge the const expr
            fn merge_expr<'de>(input: &TokenTree<'de>) -> TokenTree<'de> {
                match input {
                    TokenTree::Atom(atom) => TokenTree::Atom(*atom),
                    TokenTree::Cons(op, token_trees) => {
                        let token_trees: Vec<_> = token_trees.iter().map(merge_expr).collect();
                        match op {
                            crate::parse::Op::Minus => match token_trees.as_slice() {
                                [
                                    TokenTree::Atom(Atom::Number(left)),
                                    TokenTree::Atom(Atom::Number(right)),
                                ] => TokenTree::Atom(Atom::Number(left - right)),
                                _ => TokenTree::Cons(*op, token_trees),
                            },
                            crate::parse::Op::Plus => match token_trees.as_slice() {
                                [
                                    TokenTree::Atom(Atom::Number(left)),
                                    TokenTree::Atom(Atom::Number(right)),
                                ] => TokenTree::Atom(Atom::Number(left + right)),
                                _ => TokenTree::Cons(*op, token_trees),
                            },
                            crate::parse::Op::Star => match token_trees.as_slice() {
                                [
                                    TokenTree::Atom(Atom::Number(left)),
                                    TokenTree::Atom(Atom::Number(right)),
                                ] => TokenTree::Atom(Atom::Number(left * right)),
                                _ => TokenTree::Cons(*op, token_trees),
                            },
                            crate::parse::Op::BangEqual => match token_trees.as_slice() {
                                [
                                    TokenTree::Atom(Atom::Number(left)),
                                    TokenTree::Atom(Atom::Number(right)),
                                ] => TokenTree::Atom(Atom::Boolean(left != right)),
                                [
                                    TokenTree::Atom(Atom::String(left)),
                                    TokenTree::Atom(Atom::String(right)),
                                ] => TokenTree::Atom(Atom::Boolean(left != right)),
                                [
                                    TokenTree::Atom(Atom::Boolean(left)),
                                    TokenTree::Atom(Atom::Boolean(right)),
                                ] => TokenTree::Atom(Atom::Boolean(left != right)),
                                _ => TokenTree::Cons(*op, token_trees),
                            },
                            crate::parse::Op::EqualEqual => match token_trees.as_slice() {
                                [
                                    TokenTree::Atom(Atom::Number(left)),
                                    TokenTree::Atom(Atom::Number(right)),
                                ] => TokenTree::Atom(Atom::Boolean(left == right)),
                                [
                                    TokenTree::Atom(Atom::String(left)),
                                    TokenTree::Atom(Atom::String(right)),
                                ] => TokenTree::Atom(Atom::Boolean(left == right)),
                                [
                                    TokenTree::Atom(Atom::Boolean(left)),
                                    TokenTree::Atom(Atom::Boolean(right)),
                                ] => TokenTree::Atom(Atom::Boolean(left == right)),
                                _ => TokenTree::Cons(*op, token_trees),
                            },
                            crate::parse::Op::GreaterEqual => match token_trees.as_slice() {
                                [
                                    TokenTree::Atom(Atom::Number(left)),
                                    TokenTree::Atom(Atom::Number(right)),
                                ] => TokenTree::Atom(Atom::Boolean(left >= right)),
                                _ => TokenTree::Cons(*op, token_trees),
                            },
                            crate::parse::Op::LessEqual => match token_trees.as_slice() {
                                [
                                    TokenTree::Atom(Atom::Number(left)),
                                    TokenTree::Atom(Atom::Number(right)),
                                ] => TokenTree::Atom(Atom::Boolean(left <= right)),
                                _ => TokenTree::Cons(*op, token_trees),
                            },
                            crate::parse::Op::Greater => match token_trees.as_slice() {
                                [
                                    TokenTree::Atom(Atom::Number(left)),
                                    TokenTree::Atom(Atom::Number(right)),
                                ] => TokenTree::Atom(Atom::Boolean(left > right)),
                                _ => TokenTree::Cons(*op, token_trees),
                            },
                            crate::parse::Op::Less => match token_trees.as_slice() {
                                [
                                    TokenTree::Atom(Atom::Number(left)),
                                    TokenTree::Atom(Atom::Number(right)),
                                ] => TokenTree::Atom(Atom::Boolean(left >= right)),
                                _ => TokenTree::Cons(*op, token_trees),
                            },
                            crate::parse::Op::Slash => match token_trees.as_slice() {
                                [
                                    TokenTree::Atom(Atom::Number(left)),
                                    TokenTree::Atom(Atom::Number(right)),
                                ] => TokenTree::Atom(Atom::Number(left / right)),
                                _ => TokenTree::Cons(*op, token_trees),
                            },
                            crate::parse::Op::Bang => match token_trees.as_slice() {
                                [TokenTree::Atom(Atom::Boolean(value))] => {
                                    TokenTree::Atom(Atom::Boolean(!value))
                                }
                                _ => TokenTree::Cons(*op, token_trees),
                            },
                            crate::parse::Op::And => match token_trees.as_slice() {
                                [
                                    TokenTree::Atom(Atom::Boolean(left)),
                                    TokenTree::Atom(Atom::Boolean(right)),
                                ] => TokenTree::Atom(Atom::Boolean(*left && *right)),
                                _ => TokenTree::Cons(*op, token_trees),
                            },
                            crate::parse::Op::Or => match token_trees.as_slice() {
                                [
                                    TokenTree::Atom(Atom::Boolean(left)),
                                    TokenTree::Atom(Atom::Boolean(right)),
                                ] => TokenTree::Atom(Atom::Boolean(*left || *right)),
                                _ => TokenTree::Cons(*op, token_trees),
                            },
                            _ => TokenTree::Cons(*op, token_trees.iter().map(merge_expr).collect()),
                        }
                    }

                    TokenTree::Call { callee, arguments } => TokenTree::Call {
                        callee: callee.clone(),
                        arguments: arguments.iter().map(merge_expr).collect(),
                    },
                }
            }
            let expr = merge_expr(&expr);
            println!("{expr}");
            Ok(Value::Nil)
        }
        _ => Err(miette!("Not a expr")),
    }
}

// TODO: Error Message
pub fn sin<'de>(input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Number(n)) => Ok(Value::Number(n.sin())),
        _ => Err(miette!("Not a number")),
    }
}
// TODO: Error Message
pub fn cos<'de>(input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Number(n)) => Ok(Value::Number(n.cos())),
        _ => Err(miette!("Not a number")),
    }
}
// TODO: Error Message
pub fn tan<'de>(input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Number(n)) => Ok(Value::Number(n.tan())),
        _ => Err(miette!("Not a number")),
    }
}

pub fn exp<'de>(input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Number(n)) => Ok(Value::Number(n.exp())),
        _ => Err(miette!("Not a number")),
    }
}

// TODO: Error Message
pub fn log<'de>(input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Number(n)) => Ok(Value::Number(n.log10())),
        _ => Err(miette!("Not a number")),
    }
}

// TODO: Error Message
pub fn sqrt<'de>(input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Number(n)) => Ok(Value::Number(n.sqrt())),
        _ => Err(miette!("Not a number")),
    }
}
