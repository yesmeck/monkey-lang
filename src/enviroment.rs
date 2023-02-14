use std::{collections::HashMap, rc::Rc, cell::RefCell};

use crate::object::Object;

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Enviroment {
    store: HashMap<String, Object>,
    pub outer: Option<Rc<RefCell<Enviroment>>>,
}

impl Enviroment {
    pub fn new(outer: Option<Rc<RefCell<Enviroment>>>) -> Self {
        Self {
            store: HashMap::new(),
            outer
        }
    }

    pub fn get(&self, name: String) -> Option<Object> {
        match self.store.get(name.as_str()) {
            Some(value) => Some(value.clone()),
            None => {
                match self.outer {
                    Some(ref outer) => {
                        let env = outer.borrow();
                       env.get(name)
                    },
                    None => None,
                }
            }
        }
    }

    pub fn set(&mut self, name: String, object: Object) {
        self.store.insert(name, object);
    }
}
