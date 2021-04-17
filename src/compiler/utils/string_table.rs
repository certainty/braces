use lasso::{Rodeo, Spur};
pub type Key = Spur;

#[repr(transparent)]
#[derive(Debug)]
pub struct StringTable {
    implementation: Rodeo<Key>,
}

impl Default for StringTable {
    fn default() -> StringTable {
        StringTable {
            implementation: Rodeo::default(),
        }
    }
}

impl StringTable {
    pub fn string_set(&self) -> Vec<String> {
        self.implementation
            .iter()
            .map(|e| String::from(e.1))
            .collect()
    }

    pub fn str_set<'a>(&'a self) -> Vec<&'a str> {
        self.implementation.iter().map(|e| e.1).collect()
    }

    pub fn get<'a>(&'a self, key: &Key) -> &'a str {
        self.implementation.resolve(key)
    }

    pub fn get_or_intern<T: AsRef<str>>(&mut self, v: T) -> Key {
        self.implementation.get_or_intern(v)
    }

    pub fn absorb(&mut self, other: &StringTable) {
        if self as *const _ == other as *const _ {
            return ();
        } else {
            for string in other.implementation.iter() {
                self.get_or_intern(string.1);
            }
        }
    }
}

impl From<&Vec<String>> for StringTable {
    fn from(strings: &Vec<String>) -> StringTable {
        let mut table = StringTable::default();
        for s in strings {
            table.get_or_intern(&s);
        }

        table
    }
}
