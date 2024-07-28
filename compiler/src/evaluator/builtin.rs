use std::fmt::{Display, Formatter, Result};

use crate::object::Object;

#[derive(Clone)]
pub enum Builtin {
    Len,
    First,
    Last,
    Rest,
}

impl Builtin {
    pub fn lookup(ident: &str) -> Option<Object> {
        match ident {
            "len" => Some(Object::Builtin(Builtin::Len)),
            "first" => Some(Object::Builtin(Builtin::First)),
            "last" => Some(Object::Builtin(Builtin::Last)),
            "rest" => Some(Object::Builtin(Builtin::Rest)),
            _ => None,
        }
    }

    pub fn apply_func(&self, args: Vec<Object>) -> Object {
        match self {
            Builtin::Len => {
                if args.len() != 1 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}. want=1",
                        args.len()
                    ));
                }
                match &args[0] {
                    Object::String(val) => Object::Integer(val.len() as i64),
                    Object::Array(arr) => Object::Integer(arr.len() as i64),
                    _ => Object::Error(format!(
                        "argument to `len` not supported, got {}",
                        &args[0].object_type()
                    )),
                }
            }
            Builtin::First => {
                if args.len() != 1 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}. want=1",
                        args.len()
                    ));
                }

                let arr = match &args[0] {
                    Object::Array(arr) => arr,
                    _ => {
                        return Object::Error(format!(
                            "argument to `first` must be `ARRAY, got {}",
                            args[0].object_type()
                        ))
                    }
                };

                if arr.len() > 0 {
                    arr[0].clone()
                } else {
                    Object::Null
                }
            }
            Builtin::Last => {
                if args.len() != 1 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}. want=1",
                        args.len()
                    ));
                }

                let arr = match &args[0] {
                    Object::Array(arr) => arr,
                    _ => {
                        return Object::Error(format!(
                            "argument to `last` must be `ARRAY, got {}",
                            args[0].object_type()
                        ))
                    }
                };

                if arr.len() > 0 {
                    arr[arr.len() - 1].clone()
                } else {
                    Object::Null
                }
            }
            Builtin::Rest => {
                if args.len() != 1 {
                    return Object::Error(format!(
                        "wrong number of arguments. got={}. want=1",
                        args.len()
                    ));
                }

                let arr = match &args[0] {
                    Object::Array(arr) => arr,
                    _ => {
                        return Object::Error(format!(
                            "argument to `rest` must be `ARRAY, got {}",
                            args[0].object_type()
                        ))
                    }
                };

                if arr.len() > 0 {
                    Object::Array(arr[1..].to_vec())
                } else {
                    Object::Null
                }
            }
        }
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Builtin::Len => write!(f, "len"),
            Builtin::First => write!(f, "first"),
            Builtin::Last => write!(f, "last"),
            Builtin::Rest => write!(f, "rest"),
        }
    }
}
