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
}

#[repr(transparent)]
#[derive(Debug)]
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
    pub fn string_set(&self) -> Vec<String> {
        self.implementation
            .iter()
            .map(|e| e.as_str().to_string())
            .collect()
    }

    pub fn get_or_intern(&mut self, v: String) -> Interned {
        let e = Rc::new(v);

        if let Some(existing) = self.implementation.get(&e) {
            Interned(existing.clone())
        } else {
            self.implementation.insert(e.clone());
            Interned(e.clone())
        }
    }

    pub fn absorb(&mut self, other: &StringTable) {
        if self as *const _ == other as *const _ {
            return ();
        } else {
            for string in other.implementation.iter() {
                self.get_or_intern(string.as_str().to_string());
            }
        }
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
