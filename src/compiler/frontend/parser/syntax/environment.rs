use super::Symbol;
use super::Transformer;
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
    Id(Symbol),
    Global(Symbol),
}

impl Denotation {
    pub fn identifier(id: Symbol) -> Self {
        Self::Id(id)
    }

    pub fn global(id: Symbol) -> Self {
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

type Renaming = (Symbol, Symbol); // from, to

pub struct SyntaxEnvironment {
    bindings: FxHashMap<Symbol, Denotation>,
}

impl SyntaxEnvironment {
    pub fn standard() -> Self {
        let mut env = Self::empty();
        env.extend(Symbol::forged("quote"), Denotation::Special(Special::Quote));
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
            bindings: FxHashMap::default(),
        }
    }

    pub fn divert(env_lhs: Self, env_rhs: Self) -> Self {
        let mut bindings = env_lhs.bindings;
        bindings.extend(env_rhs.bindings.into_iter());
        Self { bindings }
    }

    pub fn extend<T: Into<Symbol>>(&mut self, id: T, denotation: Denotation) {
        self.bindings.insert(id.into(), denotation);
    }

    pub fn get(&self, id: &Symbol) -> Denotation {
        match self.bindings.get(id) {
            Some(denotation) => denotation.clone(),
            _ => Denotation::Global(id.clone()),
        }
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

    pub fn rename(&mut self, id: Symbol) -> Symbol {
        self.counter += 1;
        Symbol::renamed(id.string().clone(), self.counter)
    }
}

pub struct SyntacticContext {
    pub global: SyntaxEnvironment,
    pub standard: SyntaxEnvironment,
    pub usual: SyntaxEnvironment,
}

impl SyntacticContext {
    pub fn new(global: SyntaxEnvironment) -> Self {
        Self {
            global,
            standard: SyntaxEnvironment::standard(),
            usual: SyntaxEnvironment::basic(),
        }
    }

    pub fn usual_env(&mut self) -> &mut SyntaxEnvironment {
        &mut self.usual
    }
}

impl Default for SyntacticContext {
    fn default() -> SyntacticContext {
        SyntacticContext {
            global: SyntaxEnvironment::minimal(),
            standard: SyntaxEnvironment::standard(),
            usual: SyntaxEnvironment::basic(),
        }
    }
}
