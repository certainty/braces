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
    Special(Special),
    Macro,
    Id,
}

#[derive(Debug, Clone)]
pub enum Special {
    Lambda,
    Quote,
    Set,
    Begin,
    Define,
    DefineSyntax,
    LetSyntax,
    LetrecSyntax,
    SyntaxRules,
}

pub struct SyntaxEnvironment {
    bindings: FxHashMap<Identifier, Denotation>,
}

impl SyntaxEnvironment {
    pub fn standard() -> Self {
        let mut env = Self::empty();
        env.extend("quote", Denotation::Special(Special::Quote));
        env.extend("lambda", Denotation::Special(Special::Lambda));
        env.extend("set!", Denotation::Special(Special::Set));
        env.extend("begin", Denotation::Special(Special::Begin));
        env.extend("define", Denotation::Special(Special::Define));
        env.extend("define-syntax", Denotation::Special(Special::DefineSyntax));
        env.extend("let-syntax", Denotation::Special(Special::LetSyntax));
        env.extend("letrec-syntax", Denotation::Special(Special::LetrecSyntax));
        env.extend("syntax-rules", Denotation::Special(Special::SyntaxRules));
        env
    }

    // the basic environment is the standard environment
    // extended with the unforgable set! and lambda
    pub fn basic() -> Self {
        let mut env = Self::standard();
        env.extend(
            Identifier::SpecialLambda,
            Denotation::Special(Special::Lambda),
        );
        env.extend(Identifier::SpecialSet, Denotation::Special(Special::Set));
        env
    }

    // minimal is just the unforgable set! and lambda
    pub fn minimal() -> Self {
        let mut env = Self::empty();
        env.extend(
            Identifier::SpecialLambda,
            Denotation::Special(Special::Lambda),
        );
        env.extend(Identifier::SpecialSet, Denotation::Special(Special::Set));
        env
    }

    pub fn empty() -> Self {
        Self {
            bindings: FxHashMap::default(),
        }
    }

    pub fn divert(env_lhs: Self, env_rhs: Self) -> Self {
        let mut bindings = env_lhs.bindings;
        bindings.extend(env_lhs.bindings.into_iter());

        Self {
            bindings: env_lhs.bindings,
        }
    }

    pub fn extend<T: Into<Identifier>>(&mut self, id: T, denotation: Denotation) {
        self.bindings.insert(id.into(), denotation);
    }

    pub fn get(&self, id: &Identifier) -> Option<&Denotation> {
        self.bindings.get(id)
    }

    pub fn is_bound(&self, id: &Identifier) -> bool {
        self.bindings.get(id).is_some()
    }
}

pub struct SyntacticContext {
    pub global: SyntaxEnvironment,
    pub standard: SyntaxEnvironment,
}
