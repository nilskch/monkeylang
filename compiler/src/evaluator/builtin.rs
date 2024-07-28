use std::fmt::{Display, Formatter, Result};

use crate::object::Object;

#[derive(Clone)]
pub enum Builtin {
    Len,
}

impl Builtin {
    pub fn lookup(ident: &str) -> Option<Object> {
        match ident {
            "len" => Some(Object::Builtin(Builtin::Len)),
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
                    _ => Object::Error(format!(
                        "argument to `len` not supported, got {}",
                        &args[0].object_type()
                    )),
                }
            }
        }
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Builtin::Len => write!(f, "len"),
        }
    }
}
