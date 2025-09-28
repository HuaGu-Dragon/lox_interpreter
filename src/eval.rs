use std::{borrow::Cow, cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use miette::{LabeledSpan, miette};

use crate::{
    Parser,
    parse::{Atom, Op, StatementTree, TokenTree},
    system::input,
};

#[derive(Debug, Clone)]
pub enum Value<'de> {
    Number(f64),
    Bool(bool),
    Str(Cow<'de, str>),
    Fun(Rc<Function<'de>>),
    Method {
        fun: Rc<Function<'de>>,
        this: Option<Box<Value<'de>>>,
        father: Option<Box<Value<'de>>>,
    },
    Class(Rc<Class<'de>>),
    Instance {
        class: Cow<'de, str>,
        fields: Rc<RefCell<HashMap<Cow<'de, str>, Value<'de>>>>,
    },
    Return(Box<Value<'de>>),
    Nil,
}

#[derive(Debug, Clone)]
pub struct Class<'de> {
    name: Cow<'de, str>,
    father: Option<Cow<'de, str>>,
    methods: HashMap<Cow<'de, str>, Value<'de>>,
}

#[derive(Debug, Clone)]
pub enum Function<'de> {
    Native {
        name: Cow<'de, str>,
        params: Vec<Cow<'de, str>>,
        body: fn(&[Value<'de>]) -> Result<Value<'de>, miette::Error>,
    },
    UserDefined {
        name: Cow<'de, str>,
        params: Vec<Cow<'de, str>>,
        body: Box<StatementTree<'de>>,
    },
}

pub struct Interpreter<'de> {
    parser: Parser<'de>,
    environment: Environment<'de>,
}

#[derive(Debug)]
pub struct Environment<'de> {
    stack: Stack<'de>,
}

#[derive(Debug)]
pub enum Terminate<'de> {
    Return(Value<'de>),
    End,
}

impl<'de> Environment<'de> {
    pub fn get(&self, name: &str) -> Option<&Value<'de>> {
        self.stack.iter().find_map(|frame| frame.get(name))
    }

    pub fn set(&mut self, name: Cow<'de, str>, value: Value<'de>) -> Result<(), miette::Error> {
        if let Some(entry) = self.stack.iter_mut().find_map(|frame| frame.get_mut(&name)) {
            *entry = value;
            Ok(())
        } else {
            Err(miette!("Variable not found"))
        }
    }

    pub fn define(&mut self, name: Cow<'de, str>, value: Value<'de>) -> Result<(), miette::Error> {
        self.stack
            .current_mut()
            .ok_or_else(|| miette!("No current stack frame"))?
            .insert(name, value);
        Ok(())
    }
}

#[derive(Debug)]
pub struct Stack<'de> {
    values: Vec<HashMap<Cow<'de, str>, Value<'de>>>,
}

impl<'de> Stack<'de> {
    pub fn init() -> Self {
        let mut system = HashMap::new();
        system.insert(
            Cow::Borrowed("input"),
            Value::Fun(Rc::new(Function::Native {
                name: Cow::Borrowed("input"),
                params: vec![],
                body: input,
            })),
        );
        Self {
            values: vec![system],
        }
    }
    pub fn push(&mut self) {
        self.values.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.values.pop();
    }

    pub fn iter(&self) -> impl Iterator<Item = &HashMap<Cow<'de, str>, Value<'de>>> {
        self.values.iter().rev()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut HashMap<Cow<'de, str>, Value<'de>>> {
        self.values.iter_mut().rev()
    }

    pub fn current(&self) -> Option<&HashMap<Cow<'de, str>, Value<'de>>> {
        self.iter().next()
    }

    pub fn current_mut(&mut self) -> Option<&mut HashMap<Cow<'de, str>, Value<'de>>> {
        self.iter_mut().next()
    }
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
            // TODO: handle this ugly code
            environment: Environment {
                stack: Stack::init(),
            },
        }
    }

    pub fn eval_expr(&mut self) -> Result<Value<'de>, miette::Error> {
        let expr = self.parser.parse_expression_within(0)?;
        self.eval_expression(&expr)
    }

    pub fn eval_statement(&mut self) -> Result<(), miette::Error> {
        let statement = self.parser.parse_statement_within()?;
        let terminate = self.eval_statement_tree(&statement)?;
        match terminate {
            Terminate::Return(_) => Err(miette!("Cannot return from top-level code")),
            Terminate::End => Ok(()),
        }
    }

    fn eval_expression(&mut self, expr: &TokenTree<'de>) -> Result<Value<'de>, miette::Error> {
        Ok(match expr {
            TokenTree::Atom(atom) => match atom {
                Atom::String(value) => Value::Str(Cow::Borrowed(value)),
                Atom::Number(value) => Value::Number(*value),
                Atom::Nil => Value::Nil,
                Atom::Boolean(value) => Value::Bool(*value),
                Atom::Ident(name, byte) => {
                    let Some(value) = self.environment.get(name) else {
                        let message = if let Some(key) = self
                            .environment
                            .stack
                            .current()
                            .and_then(|map| map.keys().next())
                        {
                            format!("change variable `{name}` to variable `{key}` ?")
                        } else {
                            format!("define the variable `{name}` first ?")
                        };
                        return Err(miette!(
                            help = message,
                            labels = vec![LabeledSpan::at(byte - name.len()..*byte, "here",)],
                            "unexpected variable name"
                        )
                        .with_source_code(self.parser.whole.to_string()));
                    };
                    // TODO: is a way to remove clone?
                    value.clone()
                }
                Atom::Super => {
                    if let Some(value) = self.environment.get("super") {
                        value.clone()
                    } else {
                        return Err(miette!("should use in subclass method context"));
                    }
                }
                Atom::This => {
                    if let Some(value) = self.environment.get("this") {
                        value.clone()
                    } else {
                        return Err(miette!("should use in method context"));
                    }
                }
            },
            TokenTree::Cons(Op::Var, token_trees) => {
                // TODO: Handle nil declaration
                assert!(token_trees.len() == 2);
                let mut trees = token_trees.iter();
                let Some(TokenTree::Atom(Atom::Ident(name, _))) = trees.next() else {
                    // TODO: beautiful Handle
                    panic!("")
                };
                let value = self.eval_expression(trees.next().unwrap())?;

                self.environment.define(Cow::Borrowed(name), value)?;
                Value::Nil
            }
            // TODO: Handle assignment to fields
            TokenTree::Cons(Op::Equal, token_trees) => {
                assert!(token_trees.len() == 2);
                let mut trees = token_trees.iter();
                let lhs = trees.next().unwrap();
                let rhs = trees.next().unwrap();
                if let TokenTree::Atom(Atom::Ident(name, _)) = lhs {
                    let value = self.eval_expression(rhs)?;
                    self.environment.set(Cow::Borrowed(name), value)?;
                    Value::Nil
                } else {
                    let (fields, field_name) = self.resolve_left(lhs)?;
                    let value = self.eval_expression(rhs)?;
                    fields.borrow_mut().insert(field_name, value);
                    Value::Nil
                }
            }
            TokenTree::Cons(Op::Field, token_trees) => {
                assert!(token_trees.len() == 2);
                let mut trees = token_trees.iter();
                let lhs = self.eval_expression(trees.next().unwrap())?;
                let Value::Instance { class, fields } = lhs.clone() else {
                    // TODO: error handle
                    panic!("");
                };
                let TokenTree::Atom(Atom::Ident(name, _)) = trees.next().unwrap() else {
                    // TODO: Error handle
                    panic!("");
                };

                if let Some(instance) = self.environment.get(class.as_ref()) {
                    let Value::Class(class) = instance else {
                        panic!("");
                    };

                    let father = match class.father.as_ref() {
                        Some(father) => {
                            let Some(Value::Class(father)) = self.environment.get(father) else {
                                return Err(miette!("no father class"));
                            };

                            father
                                .methods
                                .get::<str>(name)
                                .map(|init| Box::new(init.clone()))
                        }

                        None => None,
                    };
                    // So many Rc, painful
                    if let Some(method) = class.methods.get::<str>(name.as_ref()) {
                        if let Value::Fun(fun) = method {
                            Value::Method {
                                fun: fun.clone(),
                                this: Some(Box::new(lhs)),
                                father,
                            }
                        } else {
                            method.clone()
                        }
                    } else if let Some(value) = fields.borrow().get::<str>(name.as_ref()) {
                        if let Value::Fun(fun) = value {
                            Value::Method {
                                fun: fun.clone(),
                                this: Some(Box::new(lhs)),
                                father: None,
                            }
                        } else {
                            value.clone()
                        }
                    } else {
                        return Err(miette!("no value"));
                    }
                } else {
                    return Err(miette!("not a class"));
                }
            }
            TokenTree::Cons(op, token_trees) => {
                let values = token_trees
                    .iter()
                    .map(|tree| self.eval_expression(tree))
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
                        [Value::Str(lhs), Value::Str(rhs)] => {
                            Value::Str(Cow::Owned(format!("{lhs}{rhs}")))
                        }
                        _ => return Err(miette::miette!("Invalid unary plus operation")),
                    },
                    crate::parse::Op::Star => match values.as_slice() {
                        [Value::Number(lhs), Value::Number(rhs)] => Value::Number(lhs * rhs),
                        _ => return Err(miette::miette!("Invalid multiplication operation")),
                    },
                    crate::parse::Op::BangEqual => match values.as_slice() {
                        [Value::Str(lhs), Value::Str(rhs)] => Value::Bool(lhs != rhs),
                        [Value::Bool(lhs), Value::Bool(rhs)] => Value::Bool(lhs != rhs),
                        [Value::Number(lhs), Value::Number(rhs)] => Value::Bool(lhs != rhs),
                        [Value::Nil, Value::Nil] => Value::Bool(false),
                        _ => return Err(miette::miette!("Invalid inequality operation")),
                    },
                    crate::parse::Op::EqualEqual => match values.as_slice() {
                        [Value::Str(lhs), Value::Str(rhs)] => Value::Bool(lhs == rhs),
                        [Value::Bool(lhs), Value::Bool(rhs)] => Value::Bool(lhs == rhs),
                        [Value::Number(lhs), Value::Number(rhs)] => Value::Bool(lhs == rhs),
                        [Value::Nil, Value::Nil] => Value::Bool(true),
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
                    crate::parse::Op::Equal => unreachable!(),
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
                    crate::parse::Op::Return => match values.as_slice() {
                        [value] => Value::Return(Box::new(value.clone())),
                        _ => return Err(miette::miette!("Invalid return operation")),
                    },
                    crate::parse::Op::Field => unreachable!(),
                    crate::parse::Op::Var => unreachable!(),
                    crate::parse::Op::While => unreachable!(),
                    crate::parse::Op::Call => todo!(),
                }
            }
            TokenTree::Call { callee, arguments } => {
                let callee_value = self.eval_expression(callee)?;
                let argument_values = arguments
                    .iter()
                    .map(|arg| self.eval_expression(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                match callee_value {
                    Value::Fun(fun) => match fun.as_ref() {
                        Function::Native { params, body, .. } => {
                            if params.len() != argument_values.len() {
                                return Err(miette::miette!("Argument count mismatch"));
                            }

                            body(&argument_values)?
                        }
                        Function::UserDefined { params, body, .. } => {
                            if params.len() != argument_values.len() {
                                return Err(miette::miette!("Argument count mismatch"));
                            }
                            self.environment.stack.push();

                            for (param, arg) in params.iter().zip(argument_values.into_iter()) {
                                // param is all borrow, so the clone is cheap
                                self.environment.define(param.clone(), arg)?;
                            }

                            let return_value = self.eval_statement_tree(body.as_ref())?;
                            self.environment.stack.pop();
                            if let Terminate::Return(value) = return_value {
                                value
                            } else {
                                Value::Nil
                            }
                        }
                    },
                    Value::Class(class) => {
                        if argument_values.is_empty() {
                            Value::Instance {
                                class: class.name.clone(),
                                fields: Rc::new(RefCell::new(HashMap::new())),
                            }
                        } else {
                            let instance = Value::Instance {
                                class: class.name.clone(),
                                fields: Rc::new(RefCell::new(HashMap::new())),
                            };

                            let father = match class.father.as_ref() {
                                Some(father) => {
                                    let Some(Value::Class(father)) = self.environment.get(father)
                                    else {
                                        return Err(miette!("no father class"));
                                    };

                                    let Some(Value::Fun(init)) = father.methods.get("init") else {
                                        return Err(miette!("father init is not a function"));
                                    };

                                    Some(Value::Method {
                                        fun: init.clone(),
                                        this: Some(Box::new(Value::Class(father.clone()))),
                                        father: None,
                                    })
                                }
                                None => None,
                            };

                            self.eval_method_call(
                                class
                                    .methods
                                    .get("init")
                                    .ok_or(miette!("don't have init method"))?
                                    .clone(),
                                Some(instance),
                                father,
                                argument_values,
                            )?
                        }
                    }
                    Value::Method {
                        fun,
                        mut this,
                        mut father,
                    } => {
                        if let Some(class) = this.as_ref()
                            && let Value::Class(class) = class.as_ref()
                        {
                            father = match class.father.as_ref() {
                                Some(father) => {
                                    let Some(Value::Class(father)) = self.environment.get(father)
                                    else {
                                        return Err(miette!("no father class"));
                                    };

                                    let Some(Value::Fun(init)) = father.methods.get("init") else {
                                        return Err(miette!("father init is not a function"));
                                    };

                                    Some(Box::new(Value::Method {
                                        fun: init.clone(),
                                        this: Some(Box::new(Value::Class(father.clone()))),
                                        father: None,
                                    }))
                                }
                                None => None,
                            };
                            this = None;
                        }
                        self.eval_method_call(
                            Value::Fun(fun),
                            this.map(|this| *this),
                            father.map(|father| *father),
                            argument_values,
                        )?
                    }
                    // TODO: error handle
                    Value::Nil => {
                        return Err(miette!("Cannot call a nil value"));
                    }
                    _ => panic!(""),
                }
            }
        })
    }

    fn eval_method_call(
        &mut self,
        method: Value<'de>,
        instance: Option<Value<'de>>,
        father: Option<Value<'de>>, // should be a function instead of class
        args: Vec<Value<'de>>,
    ) -> Result<Value<'de>, miette::Error> {
        // TODO: Error Handle
        let Value::Fun(fun) = method else { panic!("") };
        match fun.as_ref() {
            Function::UserDefined { name, params, body } => {
                if params.len() != args.len() {
                    return Err(miette!("Argument count mismatch"));
                }
                self.environment.stack.push();

                let instance = if let Some(instance) = instance {
                    self.environment
                        .define(Cow::Borrowed("this"), instance.clone())?;
                    Some(instance)
                } else {
                    None
                };

                if let Some(father) = father {
                    self.environment.define(Cow::Borrowed("super"), father)?;
                } else {
                    self.environment
                        .define(Cow::Borrowed("super"), Value::Nil)?;
                }

                for (param, arg) in params.iter().zip(args.into_iter()) {
                    self.environment.define(param.clone(), arg)?;
                }
                let ret = self.eval_statement_tree(body)?;

                self.environment.stack.pop();

                // TODO: handle init with return value, too ugly now
                if name.eq("init") {
                    if let Some(instance) = instance {
                        Ok(instance)
                    } else {
                        Ok(Value::Nil)
                    }
                } else if let Terminate::Return(value) = ret {
                    Ok(value)
                } else {
                    Ok(Value::Nil)
                }
            }
            _ => unreachable!(),
        }
    }

    fn eval_statement_tree(
        &mut self,
        statement: &StatementTree<'de>,
    ) -> Result<Terminate<'de>, miette::Error> {
        let terminate = match statement {
            StatementTree::Block(statement_trees) => {
                self.environment.stack.push();
                for statement in statement_trees {
                    let terminate = self.eval_statement_tree(statement)?;
                    if let Terminate::Return(value) = terminate {
                        self.environment.stack.pop();
                        return Ok(Terminate::Return(value));
                    }
                }
                self.environment.stack.pop();

                Terminate::End
            }
            StatementTree::Expression(token_tree) => {
                let value = self.eval_expression(token_tree)?;
                if let Value::Return(value) = value {
                    Terminate::Return(*value)
                } else {
                    Terminate::End
                }
            }
            StatementTree::Fun { name, params, body } => {
                let function = Value::Fun(Rc::new(Function::UserDefined {
                    name: Cow::Borrowed(match name {
                        Atom::Ident(name, _) => name,
                        // TODO: Error Handling
                        _ => panic!(""),
                    }),
                    params: params.iter().map(|s| Cow::Borrowed(s.literal)).collect(),
                    // TODO: change to a reference?
                    body: body.clone(),
                }));
                self.environment.define(
                    Cow::Borrowed(match name {
                        Atom::Ident(name, _) => name,
                        _ => panic!("Function name must be an identifier"),
                    }),
                    function,
                )?;

                Terminate::End
            }
            StatementTree::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_value = self.eval_expression(condition.as_ref())?;
                match condition_value {
                    Value::Bool(true) => self.eval_statement_tree(then_branch)?,
                    Value::Bool(false) => {
                        if let Some(else_branch) = else_branch {
                            self.eval_statement_tree(else_branch)?
                        } else {
                            Terminate::End
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
            } => {
                self.environment.stack.push();

                self.eval_statement_tree(init.as_ref())?;

                while match self.eval_expression(condition)? {
                    Value::Bool(true) => true,
                    Value::Bool(false) => false,
                    _ => return Err(miette::miette!("Condition must evaluate to a boolean")),
                } {
                    if let Terminate::Return(value) = self.eval_statement_tree(body.as_ref())? {
                        return Ok(Terminate::Return(value));
                    }
                    self.eval_expression(increment)?;
                }

                self.environment.stack.pop();

                Terminate::End
            }
            StatementTree::While { condition, body } => {
                while match self.eval_expression(condition.as_ref())? {
                    Value::Bool(true) => true,
                    Value::Bool(false) => false,
                    _ => return Err(miette::miette!("Condition must evaluate to a boolean")),
                } {
                    if let Terminate::Return(value) = self.eval_statement_tree(body.as_ref())? {
                        return Ok(Terminate::Return(value));
                    }
                }
                Terminate::End
            }
            StatementTree::Class { name, father, body } => {
                // TODO: Handle error
                let Atom::Ident(name, _) = name else {
                    return Err(miette!("Expected a ident"));
                };

                let father = if let Some(father) = father {
                    // TODO: Handle error
                    let Atom::Ident(name, _) = father else {
                        return Err(miette!("Expected a ident"));
                    };
                    Some(Cow::Borrowed(*name))
                } else {
                    None
                };

                let StatementTree::Block(body) = body.as_ref() else {
                    return Err(miette!("Expected a block"));
                };

                let mut methods = HashMap::new();

                body.iter().for_each(|statement| {
                    let StatementTree::Fun { name, params, body } = statement else {
                        panic!("");
                    };
                    let name = match name {
                        Atom::Ident(name, _) => Cow::Borrowed(*name),
                        _ => panic!("Function name must be an identifier"),
                    };
                    let method = Value::Fun(Rc::new(Function::UserDefined {
                        name: name.clone(),
                        params: params.iter().map(|s| Cow::Borrowed(s.literal)).collect(),
                        body: body.clone(),
                    }));
                    methods.insert(name, method);
                });

                let class = Rc::new(Class {
                    name: Cow::Borrowed(name),
                    father,
                    methods,
                });

                let class = Value::Class(class);

                self.environment.define(Cow::Borrowed(name), class)?;

                Terminate::End
            }
        };
        Ok(terminate)
    }

    // TODO: improve this method
    fn resolve_left<'a>(
        &mut self,
        expr: &'a TokenTree<'de>,
    ) -> EvalResult<'de, InstanceFieldRef<'de>> {
        match expr {
            TokenTree::Cons(Op::Field, token_trees) => {
                assert!(token_trees.len() == 2);
                let object = &token_trees[0];
                let field = &token_trees[1];
                let value = self.eval_expression(object)?;
                let Value::Instance { fields, .. } = value else {
                    return Err(miette!("Left side is not an instance"));
                };
                let TokenTree::Atom(Atom::Ident(field_name, _)) = field else {
                    return Err(miette!("Field name must be identifier"));
                };
                Ok((fields, Cow::Borrowed(field_name)))
            }
            TokenTree::Call { .. } => {
                let value = self.eval_expression(expr)?;
                let Value::Instance { .. } = value else {
                    return Err(miette!("Call did not return an instance"));
                };
                Err(miette!("Cannot assign to a function call directly"))
            }
            TokenTree::Atom(Atom::Ident(name, _)) => {
                let value = self
                    .environment
                    .get(name)
                    .ok_or(miette!("Variable not found"))?;
                let Value::Instance { fields, .. } = value else {
                    return Err(miette!("Variable is not an instance"));
                };
                Ok((fields.clone(), Cow::Borrowed(name)))
            }
            _ => Err(miette!("Invalid assignment target")),
        }
    }
}

type InstanceFields<'de> = Rc<RefCell<HashMap<Cow<'de, str>, Value<'de>>>>;
type InstanceFieldRef<'de> = (InstanceFields<'de>, Cow<'de, str>);
type EvalResult<'de, T> = Result<T, miette::Error>;

impl Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::Str(s) => write!(f, "{s}"),
            Value::Fun(_) => write!(f, "<fun>"),
            Value::Nil => write!(f, "nil"),
            Value::Return(value) => write!(f, "return {value}"),
            Value::Class { .. } => write!(f, "<class>"),
            Value::Instance { .. } => write!(f, "<instance>"),
            Value::Method { fun, this, .. } => {
                write!(f, "<method {:?} bound to {:?}>", fun, this)
            }
        }
    }
}
