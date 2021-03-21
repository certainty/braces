/*
 * Syntax represents everything that is part of the external representation. So a datum in that sense.
 * Scheme programs are made out of syntax at this level. There are higher order constructs created from these, which
 * form the `Expressions`.
 */

#[derive(PartialEq, Debug)]
pub enum Syntax {
    SelfEvaluatingSyntax(SelfEvaluating),
}

#[derive(PartialEq, Debug)]
pub enum SelfEvaluating {
    FixNum(i64),
    Bool(bool),
    Symbol(String),
}

pub fn fixnum(value: i64) -> Syntax {
    Syntax::SelfEvaluatingSyntax(SelfEvaluating::FixNum(value))
}

pub fn boolean(value: bool) -> Syntax {
    Syntax::SelfEvaluatingSyntax(SelfEvaluating::Bool(value))
}

pub fn symbol(value: &str) -> Syntax {
    Syntax::SelfEvaluatingSyntax(SelfEvaluating::Symbol(String::from(value)))
}
