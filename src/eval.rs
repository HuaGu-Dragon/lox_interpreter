use std::{borrow::Cow, fmt::Display};

use crate::{
    Parser,
    parse::{Atom, StatementTree, TokenTree},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Value<'de> {
    Number(f64),
    Bool(bool),
    Str(Cow<'de, str>),
    Nil,
}

pub struct Interpreter<'de> {
    parser: Parser<'de>,
}

impl<'de> Iterator for Interpreter<'de> {
    type Item = Result<(), miette::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.parser.lexer.peek().is_none() {
            None
        } else {
            Some(self.eval_statement())
        }
    }
}

impl<'de> Interpreter<'de> {
    pub fn new(filename: Option<&'de str>, whole: &'de str) -> Self {
        Self {
            parser: Parser::new(filename, whole),
        }
    }

    pub fn eval_expr(&mut self) -> Result<Value<'de>, miette::Error> {
        let expr = self.parser.parse_expression_within(0)?;
        Self::eval_expression(expr)
    }

    pub fn eval_statement(&mut self) -> Result<(), miette::Error> {
        let statement = self.parser.parse_statement_within()?;
        Self::eval_statement_tree(statement)
    }

    fn eval_expression(expr: TokenTree<'de>) -> Result<Value<'de>, miette::Error> {
        Ok(match expr {
            TokenTree::Atom(atom) => match atom {
                Atom::String(value) => Value::Str(Cow::Borrowed(value)),
                Atom::Number(value) => Value::Number(value),
                Atom::Nil => Value::Nil,
                Atom::Boolean(value) => Value::Bool(value),
                Atom::Ident(_value) => todo!(), // TODO: Handle identifiers
                Atom::Super => todo!(),         // TODO: Handle super
                Atom::This => todo!(),          // TODO: Handle this
            },
            TokenTree::Cons(op, token_trees) => {
                let values = token_trees
                    .into_iter()
                    .map(Self::eval_expression)
                    .collect::<Result<Vec<_>, _>>()?;
                // TODO: Handle error with source location
                match op {
                    crate::parse::Op::Minus => match values.as_slice() {
                        [Value::Number(num)] => Value::Number(-num),
                        [Value::Number(lhs), Value::Number(rhs)] => Value::Number(lhs - rhs),
                        _ => return Err(miette::miette!("Invalid unary minus operation")),
                    },
                    crate::parse::Op::Plus => match values.as_slice() {
                        [Value::Number(num)] => Value::Number(*num),
                        [Value::Number(lhs), Value::Number(rhs)] => Value::Number(lhs + rhs),
                        _ => return Err(miette::miette!("Invalid unary plus operation")),
                    },
                    crate::parse::Op::Star => match values.as_slice() {
                        [Value::Number(lhs), Value::Number(rhs)] => Value::Number(lhs * rhs),
                        _ => return Err(miette::miette!("Invalid multiplication operation")),
                    },
                    crate::parse::Op::BangEqual => match values.as_slice() {
                        [lhs, rhs] => Value::Bool(lhs != rhs),
                        _ => return Err(miette::miette!("Invalid inequality operation")),
                    },
                    crate::parse::Op::EqualEqual => match values.as_slice() {
                        [lhs, rhs] => Value::Bool(lhs == rhs),
                        _ => return Err(miette::miette!("Invalid equality operation")),
                    },
                    crate::parse::Op::GreaterEqual => match values.as_slice() {
                        [Value::Number(lhs), Value::Number(rhs)] => Value::Bool(lhs >= rhs),
                        _ => {
                            return Err(miette::miette!("Invalid greater than or equal operation"));
                        }
                    },
                    crate::parse::Op::LessEqual => match values.as_slice() {
                        [Value::Number(lhs), Value::Number(rhs)] => Value::Bool(lhs <= rhs),
                        _ => return Err(miette::miette!("Invalid less than or equal operation")),
                    },
                    crate::parse::Op::Greater => match values.as_slice() {
                        [Value::Number(lhs), Value::Number(rhs)] => Value::Bool(lhs > rhs),
                        _ => return Err(miette::miette!("Invalid greater than operation")),
                    },
                    crate::parse::Op::Less => match values.as_slice() {
                        [Value::Number(lhs), Value::Number(rhs)] => Value::Bool(lhs < rhs),
                        _ => return Err(miette::miette!("Invalid less than operation")),
                    },
                    crate::parse::Op::Slash => match values.as_slice() {
                        [Value::Number(lhs), Value::Number(rhs)] => {
                            if *rhs == 0.0 {
                                return Err(miette::miette!("Division by zero"));
                            }
                            Value::Number(lhs / rhs)
                        }
                        _ => return Err(miette::miette!("Invalid division operation")),
                    },
                    crate::parse::Op::Bang => match values.as_slice() {
                        [Value::Bool(value)] => Value::Bool(!value),
                        _ => return Err(miette::miette!("Invalid logical negation operation")),
                    },
                    crate::parse::Op::Equal => match values.as_slice() {
                        [lhs, rhs] => Value::Bool(lhs == rhs),
                        _ => return Err(miette::miette!("Invalid equality operation")),
                    },
                    crate::parse::Op::And => match values.as_slice() {
                        [Value::Bool(lhs), Value::Bool(rhs)] => Value::Bool(*lhs && *rhs),
                        _ => return Err(miette::miette!("Invalid logical AND operation")),
                    },
                    crate::parse::Op::Or => match values.as_slice() {
                        [Value::Bool(lhs), Value::Bool(rhs)] => Value::Bool(*lhs || *rhs),
                        _ => return Err(miette::miette!("Invalid logical OR operation")),
                    },
                    crate::parse::Op::Class => todo!(),
                    crate::parse::Op::If => todo!(),
                    crate::parse::Op::For => todo!(),
                    crate::parse::Op::Fun => todo!(),
                    crate::parse::Op::Print => match values.as_slice() {
                        [value] => {
                            println!("{value}");
                            Value::Nil
                        }
                        _ => return Err(miette::miette!("Invalid print operation")),
                    },
                    crate::parse::Op::Return => todo!(),
                    crate::parse::Op::Field => todo!(),
                    crate::parse::Op::Var => todo!(),
                    crate::parse::Op::While => todo!(),
                    crate::parse::Op::Call => todo!(),
                }
            }
            TokenTree::Call { callee, arguments } => todo!(),
        })
    }

    fn eval_statement_tree(statement: StatementTree<'de>) -> Result<(), miette::Error> {
        match statement {
            StatementTree::Block(statement_trees) => {
                for statement in statement_trees {
                    Self::eval_statement_tree(statement)?;
                }
            }
            StatementTree::Expression(token_tree) => {
                Self::eval_expression(token_tree)?;
            }
            StatementTree::Fun { name, params, body } => todo!(),
            StatementTree::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_value = Self::eval_expression(*condition)?;
                match condition_value {
                    Value::Bool(true) => Self::eval_statement_tree(*then_branch)?,
                    Value::Bool(false) => {
                        if let Some(else_branch) = else_branch {
                            Self::eval_statement_tree(*else_branch)?
                        }
                    }
                    _ => return Err(miette::miette!("Condition must evaluate to a boolean")),
                }
            }
            StatementTree::For {
                init,
                condition,
                increment,
                body,
            } => todo!(),
            StatementTree::While { condition, body } => todo!(),
            StatementTree::Class { name, father, body } => todo!(),
        };
        Ok(())
    }
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Str(s) => write!(f, "{s}"),
            Value::Nil => write!(f, "nil"),
        }
    }
}
