use std::{
    borrow::Cow,
    io::{Write, stdout},
};

use miette::{Error, miette};

use crate::{
    eval::{Environment, Value},
    parse::{Atom, DisplayMiddle, TokenTree},
};

pub fn input<'de>(_: &mut Environment, message: &[Value<'de>]) -> Result<Value<'de>, Error> {
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

pub fn max<'de>(_: &mut Environment, input: &[Value<'de>]) -> Result<Value<'de>, Error> {
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
pub fn number<'de>(_: &mut Environment, input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Str(value)) => Ok(Value::Number(value.parse().map_err(|e| miette!("{e}"))?)),
        _ => Err(miette!("Not a string")),
    }
}

// TODO: Error message
pub fn to_string<'de>(_: &mut Environment, input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Number(n)) => Ok(Value::Str(Cow::Owned(n.to_string()))),
        _ => Err(miette!("Not a number")),
    }
}

// TODO: Error message
pub fn write_expr<'de>(_: &mut Environment, input: &[Value<'de>]) -> Result<Value<'de>, Error> {
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
            println!("{}", DisplayMiddle(&expr));

            Ok(Value::Nil)
        }
        _ => Err(miette!("Not a expr")),
    }
}

pub fn value<'de>(_: &mut Environment, input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    let mut interpreter = match input.next() {
        Some(Value::Str(expr)) => crate::eval::Interpreter::debug(None, expr),
        _ => return Err(miette!("Not a expr")),
    };

    let value = match interpreter.eval_expr() {
        Ok(expr) => expr,
        Err(e) => {
            if e.downcast_ref::<crate::lex::Eof>().is_some() {
                eprintln!("Error: Unexpected end in value");
                eprintln!("{e:?}");
            }
            return Err(e);
        }
    };
    match value {
        Value::Number(n) => Ok(Value::Number(n)),
        Value::Bool(b) => Ok(Value::Bool(b)),
        Value::Nil => Ok(Value::Nil),
        _ => Err(miette!("Invalid input")),
    }
}

// TODO: Error Message
pub fn merge_const<'de>(_: &mut Environment, input: &[Value<'de>]) -> Result<Value<'de>, Error> {
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

            let expr = merge_expr(&expr);
            println!("{expr}");
            Ok(Value::Nil)
        }
        _ => Err(miette!("Not a expr")),
    }
}

// TODO: Error Message
pub fn sin<'de>(_: &mut Environment, input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Number(n)) => Ok(Value::Number(n.sin())),
        _ => Err(miette!("Not a number")),
    }
}
// TODO: Error Message
pub fn cos<'de>(_: &mut Environment, input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Number(n)) => Ok(Value::Number(n.cos())),
        _ => Err(miette!("Not a number")),
    }
}
// TODO: Error Message
pub fn tan<'de>(_: &mut Environment, input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Number(n)) => Ok(Value::Number(n.tan())),
        _ => Err(miette!("Not a number")),
    }
}

pub fn exp<'de>(_: &mut Environment, input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Number(n)) => Ok(Value::Number(n.exp())),
        _ => Err(miette!("Not a number")),
    }
}

// TODO: Error Message
pub fn log<'de>(_: &mut Environment, input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Number(n)) => Ok(Value::Number(n.log10())),
        _ => Err(miette!("Not a number")),
    }
}

// TODO: Error Message
pub fn sqrt<'de>(_: &mut Environment, input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    match input.next() {
        Some(Value::Number(n)) => Ok(Value::Number(n.sqrt())),
        _ => Err(miette!("Not a number")),
    }
}

pub fn assign<'de>(e: &mut Environment<'de>, input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    let Some(Value::Str(ident)) = input.next() else {
        return Err(miette!("not a string"));
    };

    let Some(value) = input.next() else {
        unreachable!()
    };

    let value = value.clone();

    e.global_define(ident.clone(), value.clone())?;

    Ok(value)
}

pub fn diff<'de>(_: &mut Environment, input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = input.iter();
    let expr = match input.next() {
        Some(Value::Str(expr)) => {
            let parser = crate::Parser::new(None, expr);

            match parser.parse_expr() {
                Ok(expr) => expr,
                Err(e) => {
                    if e.downcast_ref::<crate::lex::Eof>().is_some() {
                        eprintln!("Error: Unexpected end in diff");
                        eprintln!("{e:?}");
                    };
                    return Err(e);
                }
            }
        }
        _ => return Err(miette!("First argument must be an expression string")),
    };

    let variable = match input.next() {
        Some(Value::Str(var)) => var.as_ref(),
        _ => return Err(miette!("Second argument must be a variable name string")),
    };

    let expr = merge_expr(&expr);

    let result = differentiate(&expr, variable)?;

    let result = merge_expr(&result);

    println!("{}", DisplayMiddle(&result));

    Ok(Value::Nil)
}

fn differentiate<'de>(expr: &TokenTree<'de>, variable: &str) -> Result<TokenTree<'de>, Error> {
    match expr {
        TokenTree::Atom(Atom::Number(_)) => Ok(TokenTree::Atom(Atom::Number(0.0))),

        TokenTree::Atom(Atom::Ident(name, _)) => {
            if *name == variable {
                // d(x)/dx = 1
                Ok(TokenTree::Atom(Atom::Number(1.0)))
            } else {
                Ok(TokenTree::Atom(Atom::Number(0.0)))
            }
        }

        TokenTree::Atom(_) => Ok(TokenTree::Atom(Atom::Number(0.0))),

        TokenTree::Cons(op, token_trees) => match op {
            // (f + g)' = f' + g'
            // (f - g)' = f' - g'
            op @ (crate::parse::Op::Plus | crate::parse::Op::Minus) => match token_trees.as_slice()
            {
                [f, g] => {
                    let df = differentiate(f, variable)?;
                    let dg = differentiate(g, variable)?;
                    Ok(TokenTree::Cons(*op, vec![df, dg]))
                }
                _ => Err(miette!("Plus | Minus operator expects 2 operands")),
            },

            // (f * g)' = f' * g + f * g'
            crate::parse::Op::Star => match token_trees.as_slice() {
                [f, g] => {
                    let df = differentiate(f, variable)?;
                    let dg = differentiate(g, variable)?;
                    Ok(TokenTree::Cons(
                        crate::parse::Op::Plus,
                        vec![
                            TokenTree::Cons(crate::parse::Op::Star, vec![df, g.clone()]),
                            TokenTree::Cons(crate::parse::Op::Star, vec![f.clone(), dg]),
                        ],
                    ))
                }
                _ => Err(miette!("Star operator expects 2 operands")),
            },

            // (f / g)' = (f' * g - f * g') / (g * g)
            crate::parse::Op::Slash => match token_trees.as_slice() {
                [f, g] => {
                    let df = differentiate(f, variable)?;
                    let dg = differentiate(g, variable)?;
                    Ok(TokenTree::Cons(
                        crate::parse::Op::Slash,
                        vec![
                            TokenTree::Cons(
                                crate::parse::Op::Minus,
                                vec![
                                    TokenTree::Cons(crate::parse::Op::Star, vec![df, g.clone()]),
                                    TokenTree::Cons(crate::parse::Op::Star, vec![f.clone(), dg]),
                                ],
                            ),
                            TokenTree::Cons(crate::parse::Op::Star, vec![g.clone(), g.clone()]),
                        ],
                    ))
                }
                _ => Err(miette!("Slash operator expects 2 operands")),
            },
            _ => Err(miette!(
                "Unsupported operator for differentiation: {:?}",
                op
            )),
        },

        // function call
        TokenTree::Call { callee, arguments } => {
            if arguments.len() != 1 {
                return Err(miette!(
                    "Only single-argument functions are supported for differentiation"
                ));
            }

            let arg = &arguments[0];
            let arg_prime = differentiate(arg, variable)?;

            // (f(g(x)))' = f'(g(x)) * g'(x)
            if let TokenTree::Atom(Atom::Ident(func_name, _)) = callee.as_ref() {
                let result = match *func_name {
                    // d(sin(u))/dx = cos(u) * u'
                    "sin" => {
                        let cos_call = TokenTree::Call {
                            callee: Box::new(TokenTree::Atom(Atom::Ident("cos", 0))),
                            arguments: vec![arg.clone()],
                        };
                        TokenTree::Cons(crate::parse::Op::Star, vec![cos_call, arg_prime])
                    }

                    // d(cos(u))/dx = -sin(u) * u'
                    "cos" => {
                        let sin_call = TokenTree::Call {
                            callee: Box::new(TokenTree::Atom(Atom::Ident("sin", 0))),
                            arguments: vec![arg.clone()],
                        };
                        let neg_sin = TokenTree::Cons(
                            crate::parse::Op::Star,
                            vec![TokenTree::Atom(Atom::Number(-1.0)), sin_call],
                        );
                        TokenTree::Cons(crate::parse::Op::Star, vec![neg_sin, arg_prime])
                    }

                    // d(exp(u))/dx = exp(u) * u'
                    "exp" => TokenTree::Cons(
                        crate::parse::Op::Star,
                        vec![
                            TokenTree::Call {
                                callee: Box::new(TokenTree::Atom(Atom::Ident("exp", 0))),
                                arguments: vec![arg.clone()],
                            },
                            arg_prime,
                        ],
                    ),

                    // d(log(u))/dx = (1/u) * u'
                    "log" => {
                        let one_over_u = TokenTree::Cons(
                            crate::parse::Op::Slash,
                            vec![TokenTree::Atom(Atom::Number(1.0)), arg.clone()],
                        );
                        TokenTree::Cons(crate::parse::Op::Star, vec![one_over_u, arg_prime])
                    }

                    // d(sqrt(u))/dx = 1/(2*sqrt(u)) * u'
                    "sqrt" => {
                        let sqrt_u = TokenTree::Call {
                            callee: Box::new(TokenTree::Atom(Atom::Ident("sqrt", 0))),
                            arguments: vec![arg.clone()],
                        };
                        let two_sqrt_u = TokenTree::Cons(
                            crate::parse::Op::Star,
                            vec![TokenTree::Atom(Atom::Number(2.0)), sqrt_u],
                        );
                        let derivative_coef = TokenTree::Cons(
                            crate::parse::Op::Slash,
                            vec![TokenTree::Atom(Atom::Number(1.0)), two_sqrt_u],
                        );
                        TokenTree::Cons(crate::parse::Op::Star, vec![derivative_coef, arg_prime])
                    }

                    _ => {
                        return Err(miette!(
                            "Unsupported function for differentiation: {}",
                            func_name
                        ));
                    }
                };

                Ok(result)
            } else {
                Err(miette!("Function call must have an identifier as callee"))
            }
        }
    }
}

// merge the const expr
fn merge_expr<'de>(input: &TokenTree<'de>) -> TokenTree<'de> {
    match input {
        TokenTree::Atom(atom) => TokenTree::Atom(*atom),
        TokenTree::Cons(op, token_trees) => {
            let token_trees: Vec<_> = token_trees.iter().map(merge_expr).collect();
            match op {
                crate::parse::Op::Minus => match token_trees.as_slice() {
                    [token, TokenTree::Atom(Atom::Number(0.))] => token.clone(),
                    [
                        TokenTree::Atom(Atom::Number(0.)),
                        TokenTree::Atom(Atom::Number(right)),
                    ] => TokenTree::Atom(Atom::Number(-right)),
                    [
                        TokenTree::Atom(Atom::Number(left)),
                        TokenTree::Atom(Atom::Number(right)),
                    ] => TokenTree::Atom(Atom::Number(left - right)),
                    [
                        TokenTree::Atom(Atom::Ident(left, _)),
                        TokenTree::Atom(Atom::Ident(right, _)),
                    ] if left == right => TokenTree::Atom(Atom::Number(0.)),
                    _ => TokenTree::Cons(*op, token_trees),
                },
                crate::parse::Op::Plus => match token_trees.as_slice() {
                    [TokenTree::Atom(Atom::Number(0.)), token] => token.clone(),
                    [token, TokenTree::Atom(Atom::Number(0.))] => token.clone(),
                    [
                        TokenTree::Atom(Atom::Number(left)),
                        TokenTree::Atom(Atom::Number(right)),
                    ] => TokenTree::Atom(Atom::Number(left + right)),
                    _ => TokenTree::Cons(*op, token_trees),
                },
                crate::parse::Op::Star => match token_trees.as_slice() {
                    [TokenTree::Atom(Atom::Number(0.)), _] => TokenTree::Atom(Atom::Number(0.)),
                    [_, TokenTree::Atom(Atom::Number(0.))] => TokenTree::Atom(Atom::Number(0.)),
                    [TokenTree::Atom(Atom::Number(1.)), token] => token.clone(),
                    [token, TokenTree::Atom(Atom::Number(1.))] => token.clone(),
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
                    [TokenTree::Atom(Atom::Number(0.)), _] => TokenTree::Atom(Atom::Number(0.)),
                    [token, TokenTree::Atom(Atom::Number(1.))] => token.clone(),
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
