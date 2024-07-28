pub mod environment;

use crate::ast::{expression::Identifier, statement::BlockStatement};
use crate::evaluator::builtin::Builtin;
use environment::Env;
use std::fmt::{Display, Formatter, Result};

pub const BOOLEAN_OBJ: &str = "BOOLEAN";
pub const INTEGER_OBJ: &str = "INTEGER";
pub const RETURN_VALUE_OBJ: &str = "RETURN_VALUE";
pub const ERROR_OBJ: &str = "ERROR";
pub const NULL_OBJ: &str = "NULL";
pub const FUNCTION_OBJ: &str = "FUNCTION";
pub const STRING_OBJ: &str = "STRING";
pub const BUILTIN_OBJ: &str = "BUILTIN";
pub const ARRAY_OBJ: &str = "ARRAY";

#[derive(Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Error(String),
    Function(Function),
    String(String),
    Builtin(Builtin),
    Array(Vec<Object>),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Object::Integer(val) => write!(f, "{}", val),
            Object::Boolean(val) => write!(f, "{}", val),
            Object::ReturnValue(val) => write!(f, "{}", val),
            Object::Function(val) => write!(f, "{}", val),
            Object::Error(val) => write!(f, "{}", val),
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
            Object::Null => write!(f, "null"),
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
            Object::Builtin(_) => BUILTIN_OBJ,
            Object::Array(_) => ARRAY_OBJ,
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
            Object::Builtin(_) => format!("builtin function"),
            Object::Array(arr) => {
                let elements: Vec<String> =
                    arr.into_iter().map(|elem| format!("{}", elem)).collect();
                let elements = elements.join(", ");
                format!("[{}]", elements)
            }
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
