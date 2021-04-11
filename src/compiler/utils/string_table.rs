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
    pub fn get<'a>(&'a self, key: &Key) -> &'a str {
        self.implementation.resolve(key)
    }

    pub fn get_or_intern<T: AsRef<str>>(&mut self, v: T) -> Key {
        self.implementation.get_or_intern(v)
    }
}
