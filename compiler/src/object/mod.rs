pub mod environment;

use crate::ast::{expression::Identifier, statement::BlockStatement};
use environment::Env;
use std::fmt::{Display, Formatter, Result};

pub const BOOLEAN_OBJ: &str = "BOOLEAN";
pub const INTEGER_OBJ: &str = "INTEGER";
pub const RETURN_VALUE_OBJ: &str = "RETURN_VALUE";
pub const ERROR_OBJ: &str = "ERROR";
pub const NULL_OBJ: &str = "NULL";
pub const FUNCTION_OBJ: &str = "FUNCTION";
pub const STRING_OBJ: &str = "STRING";

#[derive(Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Error(String),
    Function(Function),
    String(String),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Object::Integer(_) => write!(f, "TODO"),
            Object::Boolean(_) => write!(f, "TODO"),
            Object::ReturnValue(_) => write!(f, "TODO"),
            Object::Function(_) => write!(f, "TODO"),
            Object::Error(_) => write!(f, "TODO"),
            Object::String(_) => write!(f, "TODO"),
            Object::Null => write!(f, "TODO"),
        }
    }
}

impl Object {
    pub fn object_type(&self) -> &str {
        match self {
            Object::Boolean(_) => BOOLEAN_OBJ,
            Object::Integer(_) => INTEGER_OBJ,
            Object::ReturnValue(_) => RETURN_VALUE_OBJ,
            Object::Function(_) => FUNCTION_OBJ,
            Object::Error(_) => ERROR_OBJ,
            Object::String(_) => STRING_OBJ,
            Object::Null => NULL_OBJ,
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Object::Boolean(value) => format!("{}", value),
            Object::Integer(value) => format!("{}", value),
            Object::ReturnValue(value) => format!("{}", value),
            Object::Function(function) => {
                let params: Vec<String> = function
                    .parameters
                    .clone()
                    .into_iter()
                    .map(|param| format!("{}", param))
                    .collect();
                let params = params.join(", ");
                format!("fn({}) {{\n{}\n}}", params, function.body)
            }
            Object::String(value) => format!("{}", value),
            Object::Error(msg) => format!("ERROR: {}", msg),
            Object::Null => String::from("null"),
        }
    }
}

#[derive(Clone)]
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
