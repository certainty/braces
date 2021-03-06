use super::symbol::Symbol;
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
    Macro(super::Transformer),
    Id,
}

impl Denotation {
    pub fn identifier() -> Self {
        Self::Id
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

#[derive(Debug, Clone)]
pub struct SyntaxEnvironment {
    scopes: Vec<FxHashMap<Symbol, Denotation>>,
}

impl SyntaxEnvironment {
    pub fn standard() -> Self {
        let mut env = Self::empty();

        env.extend(Symbol::forged("quote"), Denotation::Special(Special::Quote));
        env.extend(
            Symbol::unforgeable("quote"),
            Denotation::Special(Special::Quote),
        );
        env.extend(
            Symbol::forged("quasi-quote"),
            Denotation::Special(Special::QuasiQuote),
        );
        env.extend(
            Symbol::forged("unquote"),
            Denotation::Special(Special::Unquote),
        );
        env.extend(
            Symbol::forged("unquote-splicing"),
            Denotation::Special(Special::UnquoteSplicing),
        );

        env.extend(
            Symbol::forged("lambda"),
            Denotation::Special(Special::Lambda),
        );

        env.extend(Symbol::forged("set!"), Denotation::Special(Special::Set));
        env.extend(Symbol::forged("if"), Denotation::Special(Special::If));
        env.extend(Symbol::forged("begin"), Denotation::Special(Special::Begin));

        env.extend(
            Symbol::forged("define"),
            Denotation::Special(Special::Define),
        );
        env.extend(
            Symbol::forged("define-syntax"),
            Denotation::Special(Special::DefineSyntax),
        );
        env.extend(
            Symbol::forged("let-syntax"),
            Denotation::Special(Special::LetSyntax),
        );
        env.extend(
            Symbol::forged("letrec-syntax"),
            Denotation::Special(Special::LetrecSyntax),
        );
        env
    }

    // the basic environment is the standard environment
    // extended with the unforgable set! and lambda
    pub fn basic() -> Self {
        let mut env = Self::standard();

        env.extend(
            Symbol::unforgeable("lambda"),
            Denotation::Special(Special::Lambda),
        );
        env.extend(
            Symbol::unforgeable("set!"),
            Denotation::Special(Special::Set),
        );
        env
    }

    // minimal is just the unforgable set! and lambda
    pub fn minimal() -> Self {
        let mut env = Self::empty();
        env.extend(
            Symbol::unforgeable("lambda"),
            Denotation::Special(Special::Lambda),
        );
        env.extend(
            Symbol::unforgeable("set!"),
            Denotation::Special(Special::Set),
        );
        env
    }

    pub fn empty() -> Self {
        Self {
            scopes: vec![FxHashMap::default()],
        }
    }

    pub fn top_scope_mut(&mut self) -> &mut FxHashMap<Symbol, Denotation> {
        self.scopes.last_mut().unwrap()
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(FxHashMap::default());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn extend<T: Into<Symbol>>(&mut self, id: T, denotation: Denotation) {
        self.top_scope_mut().insert(id.into(), denotation);
    }

    pub fn get(&self, id: &Symbol) -> Denotation {
        for scope in self.scopes.iter().rev() {
            if let Some(denotation) = scope.get(id) {
                return denotation.clone();
            }
        }
        Denotation::Id
    }

    /// Creates an alias, that binds the symbol to the denotation of the other symbol
    pub fn alias(&mut self, id: Symbol, alias: Symbol) {
        let denotation = self.get(&id);
        self.extend(alias, denotation.clone())
    }
}

#[derive(Clone, Debug)]
pub struct Renamer {
    counter: u64,
}

impl Renamer {
    pub fn new() -> Self {
        Renamer { counter: 0 }
    }

    pub fn rename(&mut self, id: &Symbol) -> Symbol {
        self.counter += 1;
        Symbol::renamed(id.clone().string(), self.counter)
    }
}
