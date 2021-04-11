use lasso::{Rodeo, RodeoReader, Spur};
pub type Key = Spur;

type StringTableView = RodeoReader<Key>;

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
    pub fn view(&self) -> StringTableView {
        self.implementation.into_reader()
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
