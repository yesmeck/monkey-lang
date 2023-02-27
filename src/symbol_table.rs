use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolScope {
    Global,
    Local,
    Builtin,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: u16,
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct SymbolTable {
    pub outer: Option<Rc<RefCell<SymbolTable>>>,
    store: HashMap<String, Rc<Symbol>>,
    pub num_definitions: u16,
}

impl SymbolTable {
    pub fn new_enclosed(global: Rc<RefCell<SymbolTable>>) -> Self {
        Self {
            outer: Some(global),
            ..Default::default()
        }
    }

    pub fn define(&mut self, name: &str) -> Rc<Symbol> {
        let scope = match self.outer {
            Some(_) => SymbolScope::Local,
            None => SymbolScope::Global,
        };
        self.store.insert(
            name.into(),
            Rc::new(Symbol {
                name: name.into(),
                scope,
                index: self.num_definitions,
            }),
        );
        self.num_definitions += 1;
        self.resolve(name).unwrap()
    }

    pub fn define_builtin(&mut self, index: u16, name: &str) -> Rc<Symbol> {
        let symbol = Symbol {  name: name.into(), scope: SymbolScope::Builtin, index };
        self.store.insert(name.into(), Rc::new(symbol));
        self.resolve(name).unwrap()
    }

    pub fn resolve(&self, name: &str) -> Option<Rc<Symbol>> {
        match self.store.get(name) {
            Some(value) => Some(Rc::clone(value)),
            None => match self.outer {
                Some(ref outer) => {
                    let table = outer.borrow();
                    table.resolve(name)
                }
                None => None,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    #[test]
    fn test_define() {
        let expected = HashMap::from([
            (
                "a",
                Symbol {
                    name: "a".into(),
                    scope: SymbolScope::Global,
                    index: 0,
                },
            ),
            (
                "b",
                Symbol {
                    name: "b".into(),
                    scope: SymbolScope::Global,
                    index: 1,
                },
            ),
            (
                "c",
                Symbol {
                    name: "c".into(),
                    scope: SymbolScope::Local,
                    index: 0,
                },
            ),
            (
                "d",
                Symbol {
                    name: "d".into(),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ),
            (
                "e",
                Symbol {
                    name: "e".into(),
                    scope: SymbolScope::Local,
                    index: 0,
                },
            ),
            (
                "f",
                Symbol {
                    name: "f".into(),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ),
        ]);

        let global = RefCell::new(SymbolTable::default());
        global.borrow_mut().define("a");
        global.borrow_mut().define("b");

        assert_eq!(
            *global.borrow().resolve("a").unwrap(),
            *expected.get("a").unwrap()
        );
        assert_eq!(
            *global.borrow().resolve("b").unwrap(),
            *expected.get("b").unwrap()
        );

        let first_local = RefCell::new(SymbolTable::new_enclosed(Rc::new(global)));
        first_local.borrow_mut().define("c");
        first_local.borrow_mut().define("d");

        assert_eq!(
            *first_local.borrow().resolve("c").unwrap(),
            *expected.get("c").unwrap()
        );
        assert_eq!(
            *first_local.borrow().resolve("d").unwrap(),
            *expected.get("d").unwrap()
        );

        let second_local = RefCell::new(SymbolTable::new_enclosed(Rc::new(first_local)));
        second_local.borrow_mut().define("e");
        second_local.borrow_mut().define("f");

        assert_eq!(
            *second_local.borrow().resolve("e").unwrap(),
            *expected.get("e").unwrap()
        );
        assert_eq!(
            *second_local.borrow().resolve("f").unwrap(),
            *expected.get("f").unwrap()
        );
    }

    #[test]
    fn test_resolve_global() {
        let mut global = SymbolTable::default();
        global.define("a");
        global.define("b");

        let expected = [
            Symbol {
                name: "a".into(),
                scope: SymbolScope::Global,
                index: 0,
            },
            Symbol {
                name: "b".into(),
                scope: SymbolScope::Global,
                index: 1,
            },
        ];

        for sym in expected.iter() {
            assert_eq!(*global.resolve(&sym.name).unwrap(), *sym);
        }
    }

    #[test]
    fn test_resolve_local() {
        let global = RefCell::new(SymbolTable::default());
        global.borrow_mut().define("a");
        global.borrow_mut().define("b");

        let mut local = SymbolTable::new_enclosed(Rc::new(global));
        local.define("c");
        local.define("d");

        let expected = [
            Symbol {
                name: "a".into(),
                scope: SymbolScope::Global,
                index: 0,
            },
            Symbol {
                name: "b".into(),
                scope: SymbolScope::Global,
                index: 1,
            },
            Symbol {
                name: "c".into(),
                scope: SymbolScope::Local,
                index: 0,
            },
            Symbol {
                name: "d".into(),
                scope: SymbolScope::Local,
                index: 1,
            },
        ];

        for sym in expected.iter() {
            assert_eq!(*local.resolve(&sym.name).unwrap(), *sym);
        }
    }

    #[test]
    fn test_resolve_nested_local() {
        let global = Rc::new(RefCell::new(SymbolTable::default()));
        global.borrow_mut().define("a");
        global.borrow_mut().define("b");

        let first_local = Rc::new(RefCell::new(SymbolTable::new_enclosed(Rc::clone(&global))));
        first_local.borrow_mut().define("c");
        first_local.borrow_mut().define("d");

        let second_local = Rc::new(RefCell::new(SymbolTable::new_enclosed(Rc::clone(
            &first_local,
        ))));
        second_local.borrow_mut().define("e");
        second_local.borrow_mut().define("f");

        let tests = [
            (
                Rc::clone(&first_local),
                [
                    Symbol {
                        name: "a".into(),
                        scope: SymbolScope::Global,
                        index: 0,
                    },
                    Symbol {
                        name: "b".into(),
                        scope: SymbolScope::Global,
                        index: 1,
                    },
                    Symbol {
                        name: "c".into(),
                        scope: SymbolScope::Local,
                        index: 0,
                    },
                    Symbol {
                        name: "d".into(),
                        scope: SymbolScope::Local,
                        index: 1,
                    },
                ],
            ),
            (
                Rc::clone(&second_local),
                [
                    Symbol {
                        name: "a".into(),
                        scope: SymbolScope::Global,
                        index: 0,
                    },
                    Symbol {
                        name: "b".into(),
                        scope: SymbolScope::Global,
                        index: 1,
                    },
                    Symbol {
                        name: "e".into(),
                        scope: SymbolScope::Local,
                        index: 0,
                    },
                    Symbol {
                        name: "f".into(),
                        scope: SymbolScope::Local,
                        index: 1,
                    },
                ],
            ),
        ];

        for (table, expected) in tests.iter() {
            for sym in expected.iter() {
                assert_eq!(*(**table).borrow().resolve(&sym.name).unwrap(), *sym);
            }
        }
    }

    #[test]
    fn test_define_resolve_builtins() {
        let global = Rc::new(RefCell::new(SymbolTable::default()));
        let first_local = Rc::new(RefCell::new(SymbolTable::new_enclosed(Rc::clone(&global))));
        let second_local = Rc::new(RefCell::new(SymbolTable::new_enclosed(Rc::clone(&first_local))));

        let expected = [
            Symbol {
                name: "a".into(),
                scope: SymbolScope::Builtin,
                index: 0,
            },
            Symbol {
                name: "c".into(),
                scope: SymbolScope::Builtin,
                index: 1,
            },
            Symbol {
                name: "e".into(),
                scope: SymbolScope::Builtin,
                index: 2,
            },
            Symbol {
                name: "f".into(),
                scope: SymbolScope::Builtin,
                index: 3,
            },
        ];

        for (i, v) in expected.iter().enumerate() {
            global.borrow_mut().define_builtin(i as u16, &v.name);
        }

        for table in [global, first_local, second_local].iter() {
            for sym in expected.iter() {
                assert_eq!(*(**table).borrow().resolve(&sym.name).unwrap(), *sym);
            }
        }
    }
}
