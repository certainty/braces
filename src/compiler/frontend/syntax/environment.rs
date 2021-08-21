use super::identifier::{Identifier, Transformer};
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

#[derive(Debug, Clone)]
pub enum Denotation {
    Special(Special),
    Macro(Transformer),
    Id(Identifier),
    Global(Identifier),
}

impl Denotation {
    pub fn identifier(id: Identifier) -> Self {
        Self::Id(id)
    }

    pub fn global(id: Identifier) -> Self {
        Self::Global(id)
    }
}

#[derive(Debug, Clone)]
pub enum Special {
    Lambda,
    Quote,
    QuasiQuote,
    Unquote,
    UnquoteSplicing,
    Set,
    If,
    Begin,
    Define,
    DefineSyntax,
    LetSyntax,
    LetrecSyntax,
}

type Renaming = (Identifier, Identifier); // from, to

#[derive(Debug, Clone)]
pub struct SyntaxEnvironment {
    scopes: Vec<FxHashMap<Identifier, Denotation>>,
}

impl SyntaxEnvironment {
    pub fn standard() -> Self {
        let mut env = Self::empty();
        env.extend(
            Identifier::forged("quote"),
            Denotation::Special(Special::Quote),
        );
        env.extend(
            Identifier::forged("quasi-quote"),
            Denotation::Special(Special::QuasiQuote),
        );
        env.extend(
            Identifier::forged("unquote"),
            Denotation::Special(Special::Unquote),
        );
        env.extend(
            Identifier::forged("unquote-splicing"),
            Denotation::Special(Special::UnquoteSplicing),
        );

        env.extend(
            Identifier::forged("lambda"),
            Denotation::Special(Special::Lambda),
        );
        env.extend(
            Identifier::forged("set!"),
            Denotation::Special(Special::Set),
        );
        env.extend(Identifier::forged("if"), Denotation::Special(Special::If));
        env.extend(
            Identifier::forged("begin"),
            Denotation::Special(Special::Begin),
        );

        env.extend(
            Identifier::forged("define"),
            Denotation::Special(Special::Define),
        );
        env.extend(
            Identifier::forged("define-syntax"),
            Denotation::Special(Special::DefineSyntax),
        );
        env.extend(
            Identifier::forged("let-syntax"),
            Denotation::Special(Special::LetSyntax),
        );
        env.extend(
            Identifier::forged("letrec-syntax"),
            Denotation::Special(Special::LetrecSyntax),
        );
        env
    }

    // the basic environment is the standard environment
    // extended with the unforgable set! and lambda
    pub fn basic() -> Self {
        let mut env = Self::standard();
        env.extend(
            Identifier::unforgeable("lambda"),
            Denotation::Special(Special::Lambda),
        );
        env.extend(
            Identifier::unforgeable("set!"),
            Denotation::Special(Special::Set),
        );
        env
    }

    // minimal is just the unforgable set! and lambda
    pub fn minimal() -> Self {
        let mut env = Self::empty();
        env.extend(
            Identifier::unforgeable("lambda"),
            Denotation::Special(Special::Lambda),
        );
        env.extend(
            Identifier::unforgeable("set!"),
            Denotation::Special(Special::Set),
        );
        env
    }

    pub fn empty() -> Self {
        Self {
            scopes: vec![FxHashMap::default()],
        }
    }

    pub fn top_scope_mut(&mut self) -> &mut FxHashMap<Identifier, Denotation> {
        self.scopes.last_mut().unwrap()
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(FxHashMap::default());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn extend<T: Into<Identifier>>(&mut self, id: T, denotation: Denotation) {
        self.top_scope_mut().insert(id.into(), denotation);
    }

    pub fn get(&self, id: &Identifier) -> Denotation {
        for scope in &self.scopes {
            if let Some(denotation) = scope.get(id) {
                return denotation.clone();
            }
        }
        Denotation::Id(id.clone())
    }

    // Given a syntactic environment env to be extended, an alist returned
    // by rename-vars, and a syntactic environment env2, extends env by
    // binding the fresh identifiers to the denotations of the original
    // identifiers in env2.
    pub fn alias(&mut self, source: SyntaxEnvironment, renamed_ids: Vec<Renaming>) {
        for (old, new) in renamed_ids {
            let denotation = source.get(&old);
            self.extend(new, denotation.clone());
        }
    }

    // Given a syntactic environment and an alist returned by rename-vars,
    // extends the environment by binding the old identifiers to the fresh
    // identifiers.
    pub fn rename(&mut self, renamed_ids: Vec<Renaming>) {
        for (old, new) in renamed_ids {
            let denotation = Denotation::identifier(new.clone());
            self.extend(old, denotation.clone());
            self.extend(new, denotation);
        }
    }
}

pub struct Renamer {
    counter: u64,
}

impl Renamer {
    pub fn new() -> Self {
        Renamer { counter: 0 }
    }

    pub fn rename(&mut self, id: Identifier) -> Identifier {
        self.counter += 1;
        Identifier::renamed(id.string().clone(), self.counter)
    }
}
