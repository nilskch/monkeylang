use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::Object;

#[derive(Clone)]
pub struct Environment {
    store: HashMap<String, Rc<Object>>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed_environment(outer: &Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(Rc::clone(outer)),
        }
    }

    pub fn get(&self, name: &str) -> Option<Rc<Object>> {
        match self.store.get(name) {
            Some(object) => Some(Rc::clone(object)),
            None => match &self.outer {
                Some(env) => env.borrow().get(name),
                None => None,
            },
        }
    }

    pub fn set(&mut self, name: String, value: Rc<Object>) {
        self.store.insert(name, value);
    }
}
