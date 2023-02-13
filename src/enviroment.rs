use std::collections::HashMap;

use crate::object::Object;

#[derive(Debug)]
pub struct Enviroment {
    store: HashMap<String, Object>,
}

impl Enviroment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: String) -> Option<&Object> {
        self.store.get(name.as_str())
    }

    pub fn set(&mut self, name: String, object: Object) {
        self.store.insert(name, object);
    }
}
