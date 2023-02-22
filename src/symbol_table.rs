use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolScope {
    Global,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: u16,
}

#[derive(Debug, Default, Clone)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: u16,
}

impl SymbolTable {
    pub fn define(&mut self, name: &str) -> &Symbol {
        self.store.insert(
            name.into(),
            Symbol {
                name: name.into(),
                scope: SymbolScope::Global,
                index: self.num_definitions,
            },
        );
        self.num_definitions += 1;
        self.resolve(name).unwrap()
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.store.get(name)
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
        ]);

        let mut global = SymbolTable::default();

        global.define("a");
        global.define("b");

        assert_eq!(global.resolve("a").unwrap(), expected.get("a").unwrap());
        assert_eq!(global.resolve("b").unwrap(), expected.get("b").unwrap());
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
}
