pub mod environment;

use crate::ast::{expression::Identifier, statement::BlockStatement};
use crate::evaluator::builtin::Builtin;
use environment::Env;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result};
use std::hash::{Hash, Hasher};

#[derive(Clone, PartialEq, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Function(Function),
    String(String),
    Builtin(Builtin),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Object::Integer(val) => write!(f, "{}", val),
            Object::Boolean(val) => write!(f, "{}", val),
            Object::ReturnValue(val) => write!(f, "{}", val),
            Object::Function(val) => write!(f, "{}", val),
            Object::String(val) => write!(f, "{}", val),
            Object::Builtin(val) => write!(f, "{}", val),
            Object::Array(elements) => {
                let elements: Vec<String> = elements
                    .clone()
                    .into_iter()
                    .map(|elem| format!("{}", elem))
                    .collect();
                let elements = elements.clone().join(", ");
                write!(f, "[{}]", elements)
            }
            Object::Hash(hash) => {
                let pairs: Vec<String> = hash
                    .clone()
                    .into_iter()
                    .map(|(key, value)| format!("{}:{}", key, value))
                    .collect();
                let pairs = pairs.join(", ");
                write!(f, "{{ {} }}", pairs)
            }
            Object::Null => write!(f, "null"),
        }
    }
}

impl Object {
    pub fn object_type(&self) -> &str {
        match self {
            Object::Boolean(_) => "BOOLEAN",
            Object::Integer(_) => "INTEGER",
            Object::ReturnValue(_) => "RETURN_VALUE",
            Object::Function(_) => "FUNCTION",
            Object::String(_) => "STRING",
            Object::Builtin(_) => "BUILTIN",
            Object::Array(_) => "ARRAY",
            Object::Hash(_) => "HASH",
            Object::Null => "NULL",
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Object::Boolean(value) => format!("{}", value),
            Object::Integer(value) => format!("{}", value),
            Object::ReturnValue(value) => format!("{}", value),
            Object::Function(function) => format!("{}", function),
            Object::String(value) => format!("{}", value),
            Object::Array(arr) => format!("{}", Object::Array(arr.clone())),
            Object::Hash(hash) => format!("{}", Object::Hash(hash.clone())),
            Object::Builtin(_) => format!("builtin function"),
            Object::Null => String::from("null"),
        }
    }

    pub fn is_hashable(&self) -> bool {
        match self {
            Object::Boolean(_) | Object::String(_) | Object::Integer(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Env,
}

impl Function {
    pub fn new(parameters: Vec<Identifier>, body: BlockStatement, env: Env) -> Function {
        Function {
            parameters,
            body,
            env,
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let parameters: Vec<String> = self
            .parameters
            .clone()
            .into_iter()
            .map(|elem| format!("{}", elem))
            .collect();
        let parameters = parameters.join(", ");

        write!(f, "fn({}) {{ {} }}", parameters, self.body)
    }
}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::Integer(val) => val.hash(state),
            Object::Boolean(val) => val.hash(state),
            Object::String(val) => val.hash(state),
            _ => unreachable!(),
        }
    }
}
