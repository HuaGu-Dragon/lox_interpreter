use std::{
    borrow::Cow,
    io::{Write, stdout},
};

use miette::{Error, miette};

use crate::eval::Value;

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
