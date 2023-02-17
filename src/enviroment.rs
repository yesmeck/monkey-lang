use std::{collections::HashMap, rc::Rc, cell::RefCell};

use crate::object::{Object, Boolean, Null};

#[derive(Debug, PartialEq, Clone)]
pub struct Enviroment {
    store: HashMap<String, Rc<Object>>,
    pub outer: Option<Rc<RefCell<Enviroment>>>,
    pub true_object: Rc<Object>,
    pub false_object: Rc<Object>,
    pub null_object: Rc<Object>,
}

impl Default for Enviroment {
    fn default() -> Self {
        Self { 
            store: HashMap::new(),
            outer: None,
            true_object: Rc::new(Object::Boolean(Boolean::new(true))),
            false_object: Rc::new(Object::Boolean(Boolean::new(false))),
            null_object: Rc::new(Object::Null(Null::default())),
        }
    }
}

impl Enviroment {
    pub fn new(outer: Rc<RefCell<Enviroment>>) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(Rc::clone(&outer)),
            true_object: Rc::clone(&outer.borrow().true_object),
            false_object: Rc::clone(&outer.borrow().false_object),
            null_object: Rc::clone(&outer.borrow().null_object),
        }
    }

    pub fn get(&self, name: String) -> Option<Rc<Object>> {
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

    pub fn set(&mut self, name: String, object: Rc<Object>) {
        self.store.insert(name, object);
    }
}
