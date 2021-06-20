use rustc_hash::FxHashMap;

type SyntaxInformation = ();

pub struct Environment {
    bindings: FxHashMap<String, SyntaxInformation>,
    parent: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            bindings: FxHashMap::default(),
            parent: None,
        }
    }

    pub fn child(parent: Environment) -> Self {
        Environment {
            bindings: FxHashMap::default(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn bind(&mut self, name: String, value: SyntaxInformation) {
        self.bindings.insert(name, value);
    }

    pub fn get(&self, k: &String) -> Option<&SyntaxInformation> {
        match (self.bindings.get(k), &self.parent) {
            (None, Some(p)) => p.get(k),
            (None, None) => None,
            (v, _) => v,
        }
    }
    pub fn is_bound(&self, k: &String) -> bool {
        self.get(k).is_some()
    }
}
