use crate::parse::{Atom, TokenTree};

pub fn eval_expr(expr: TokenTree) -> Result<Atom, miette::Error> {
    Ok(match expr {
        TokenTree::Atom(atom) => atom,
        TokenTree::Cons(op, token_trees) => match op {
            crate::parse::Op::Minus => todo!(),
            crate::parse::Op::Plus => todo!(),
            crate::parse::Op::Star => todo!(),
            crate::parse::Op::BangEqual => {
                let [left, right] = &token_trees[..] else {
                    panic!("Expected two operands");
                };
                if left != right {
                    Atom::Boolean(true)
                } else {
                    Atom::Boolean(false)
                }
            }
            crate::parse::Op::EqualEqual => {
                let [left, right] = &token_trees[..] else {
                    panic!("Expected two operands"); // Not fail
                };
                if left == right {
                    Atom::Boolean(true)
                } else {
                    Atom::Boolean(false)
                }
            }
            crate::parse::Op::GreaterEqual => todo!(),
            crate::parse::Op::LessEqual => todo!(),
            crate::parse::Op::Greater => todo!(),
            crate::parse::Op::Less => todo!(),
            crate::parse::Op::Slash => todo!(),
            crate::parse::Op::Bang => todo!(),
            crate::parse::Op::Equal => todo!(),
            crate::parse::Op::And => todo!(),
            crate::parse::Op::Or => todo!(),
            _ => panic!("not an expression"), // TODO: use miette::Error to have a pretty failure
        },
        TokenTree::Call { callee, arguments } => todo!(),
    })
}
