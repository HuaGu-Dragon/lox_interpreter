use std::borrow::Cow;

use miette::{Error, miette};

use crate::eval::Value;

pub fn input<'de>(_input: &[Value<'de>]) -> Result<Value<'de>, Error> {
    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .map_err(|e| miette!("{e}"))?;

    Ok(Value::Str(Cow::Owned(input)))
}
