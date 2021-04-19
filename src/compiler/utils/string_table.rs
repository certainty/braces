use rustc_hash::FxHashSet;
use std::hash::Hash;
use std::rc::Rc;

#[repr(transparent)]
#[derive(Debug, Eq, Hash, Clone, PartialEq)]
pub struct Interned(Rc<String>);

impl Interned {
    pub fn as_str<'a>(&'a self) -> &'a str {
        &(*self.0)
    }

    pub fn as_string(&self) -> String {
        (*self.0).clone()
    }
}

impl From<&Rc<String>> for Interned {
    fn from(rc: &Rc<String>) -> Interned {
        Interned(Rc::clone(rc))
    }
}

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct StringTable {
    implementation: FxHashSet<Rc<String>>,
}

impl Default for StringTable {
    fn default() -> StringTable {
        StringTable {
            implementation: FxHashSet::default(),
        }
    }
}

impl StringTable {
    pub fn interned_vec(&self) -> Vec<Interned> {
        self.implementation.iter().map(Interned::from).collect()
    }

    pub fn add(&mut self, interned: Interned) {
        self.implementation.insert(interned.0);
    }

    pub fn get_or_intern(&mut self, v: String) -> Interned {
        let e = Rc::new(v);

        if let Some(existing) = self.implementation.get(&e) {
            Interned(Rc::clone(existing))
        } else {
            self.implementation.insert(Rc::clone(&e));
            Interned(e)
        }
    }

    pub fn absorb(&mut self, other: StringTable) {
        for string in other.implementation.iter() {
            self.get_or_intern(string.as_str().to_string());
        }
    }
}

impl Extend<Interned> for StringTable {
    fn extend<T: IntoIterator<Item = Interned>>(&mut self, iter: T) {
        for elem in iter {
            self.add(elem);
        }
    }
}

impl From<Vec<Interned>> for StringTable {
    fn from(interned: Vec<Interned>) -> StringTable {
        let mut table = StringTable::default();
        for i in interned {
            table.get_or_intern(i.as_string());
        }
        table
    }
}

impl From<&Vec<String>> for StringTable {
    fn from(strings: &Vec<String>) -> StringTable {
        let mut table = StringTable::default();
        for s in strings {
            table.get_or_intern(s.clone());
        }

        table
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_or_intern() {
        let mut table = StringTable::default();
        let first = table.get_or_intern(String::from("foo"));
        let second = table.get_or_intern(String::from("foo"));
        let third = table.get_or_intern(String::from("bar"));

        assert!(
            std::ptr::eq(first.0.as_ptr(), second.0.as_ptr()),
            "expected equal pointers"
        );

        assert!(
            !std::ptr::eq(first.0.as_ptr(), third.0.as_ptr()),
            "expected unequal pointers"
        );
    }
}
