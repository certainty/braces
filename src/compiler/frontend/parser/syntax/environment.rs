use rustc_hash::FxHashMap;

/*

  https://github.com/larcenists/larceny/blob/fef550c7d3923deb7a5a1ccd5a628e54cf231c75/src/Compiler/syntaxenv.sch

 A syntactic environment maps identifiers to denotations,
 where a denotation is one of

    (special <special>)
    (macro <rules> <env>)
    (identifier <id> <references> <assignments> <calls>)

 and where <special> is one of

    quote
    lambda
    if
    set!
    begin
    define
    define-syntax
    let-syntax
    letrec-syntax
    syntax-rules
 and where <rules> is a compiled <transformer spec> (see R4RS),
 <env> is a syntactic environment, and <id> is an identifier.
*/

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Identifier {
    Id(String),
    // Unforgeable identifier which is only equal to itself
    SpecialLambda,
    SpecialSet,
}

impl<T: Into<String>> From<T> for Identifier {
    fn from(id: T) -> Identifier {
        Identifier::Id(id.into())
    }
}

#[derive(Debug, Clone)]
pub enum Denotation {
    Special(Identifier),
    Macro,
    Id,
}

pub struct SyntaxEnvironment {
    bindings: FxHashMap<Identifier, Denotation>,
    parent: Option<Box<Self>>,
}

impl SyntaxEnvironment {
    pub fn standard() -> Self {
        let mut env = Self::new();
        env.extend("quote", Denotation::Special("quote".into()));

        env
    }

    pub fn basic() -> Self {
        todo!()
    }

    pub fn minimal() -> Self {
        todo!()
    }

    pub fn new() -> Self {
        Self {
            bindings: FxHashMap::default(),
            parent: None,
        }
    }

    pub fn child(parent: Self) -> Self {
        Self {
            bindings: FxHashMap::default(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn extend<T: Into<Identifier>>(&mut self, id: T, denotation: Denotation) {
        self.bindings.insert(id.into(), denotation);
    }

    pub fn get(&self, id: &Identifier) -> Option<&Denotation> {
        match (self.bindings.get(id), &self.parent) {
            (None, Some(p)) => p.get(id),
            (None, None) => None,
            (v, _) => v,
        }
    }

    pub fn is_bound(&self, id: &Identifier) -> bool {
        self.bindings.get(id).is_some()
    }
}
