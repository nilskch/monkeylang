use std::fmt;

use crate::object::Object;

use super::{error::EvaluationError, EvaluationResult};

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Builtin {
    Len,
    First,
    Last,
    Rest,
    Push,
    Print,
}

fn check_argument_len(got: usize, expected: usize) -> Result<(), EvaluationError> {
    if expected != got {
        Err(EvaluationError::new(format!(
            "wrong number of arguments. want={}. got={}",
            expected, got
        )))
    } else {
        Ok(())
    }
}

impl Builtin {
    pub fn lookup(ident: &str) -> Option<Object> {
        match ident {
            "len" => Some(Object::Builtin(Builtin::Len)),
            "first" => Some(Object::Builtin(Builtin::First)),
            "last" => Some(Object::Builtin(Builtin::Last)),
            "rest" => Some(Object::Builtin(Builtin::Rest)),
            "push" => Some(Object::Builtin(Builtin::Push)),
            "print" => Some(Object::Builtin(Builtin::Print)),
            _ => None,
        }
    }

    pub fn apply_func(&self, args: Vec<Object>) -> EvaluationResult {
        match self {
            Builtin::Len => {
                check_argument_len(args.len(), 1)?;
                match &args[0] {
                    Object::String(val) => Ok(Object::Integer(val.len() as i64)),
                    Object::Array(arr) => Ok(Object::Integer(arr.len() as i64)),
                    _ => Err(EvaluationError::new(format!(
                        "argument to `len` not supported, got {}",
                        &args[0].object_type()
                    ))),
                }
            }
            Builtin::First => {
                check_argument_len(args.len(), 1)?;

                let arr = match &args[0] {
                    Object::Array(arr) => arr,
                    _ => {
                        return Err(EvaluationError::new(format!(
                            "argument to `first` must be `ARRAY, got {}",
                            args[0].object_type()
                        )))
                    }
                };

                if arr.len() > 0 {
                    Ok(arr[0].clone())
                } else {
                    Ok(Object::Null)
                }
            }
            Builtin::Last => {
                check_argument_len(args.len(), 1)?;

                let arr = match &args[0] {
                    Object::Array(arr) => arr,
                    _ => {
                        return Err(EvaluationError::new(format!(
                            "argument to `last` must be `ARRAY, got {}",
                            args[0].object_type()
                        )))
                    }
                };

                if arr.len() > 0 {
                    Ok(arr[arr.len() - 1].clone())
                } else {
                    Ok(Object::Null)
                }
            }
            Builtin::Rest => {
                check_argument_len(args.len(), 1)?;

                let arr = match &args[0] {
                    Object::Array(arr) => arr,
                    _ => {
                        return Err(EvaluationError::new(format!(
                            "argument to `rest` must be `ARRAY, got {}",
                            args[0].object_type()
                        )))
                    }
                };

                if arr.len() > 0 {
                    Ok(Object::Array(arr[1..].to_vec()))
                } else {
                    Ok(Object::Null)
                }
            }
            Builtin::Push => {
                check_argument_len(args.len(), 2)?;

                let arr = match &args[0] {
                    Object::Array(arr) => arr,
                    _ => {
                        return Err(EvaluationError::new(format!(
                            "argument to `rest` must be `ARRAY, got {}",
                            args[0].object_type()
                        )))
                    }
                };
                let mut new_arr = arr.clone();
                new_arr.push(args[1].clone());

                Ok(Object::Array(new_arr))
            }
            Builtin::Print => {
                for arg in args.iter() {
                    println!("{}", arg);
                }
                Ok(Object::Null)
            }
        }
    }
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Builtin::Len => write!(f, "len"),
            Builtin::First => write!(f, "first"),
            Builtin::Last => write!(f, "last"),
            Builtin::Rest => write!(f, "rest"),
            Builtin::Push => write!(f, "push"),
            Builtin::Print => write!(f, "print"),
        }
    }
}
