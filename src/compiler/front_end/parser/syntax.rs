/*
 * Syntax represents everything that is part of the external representation. So a datum in that sense.
 * Scheme programs are made out of syntax at this level. There are higher order constructs created from these, which
 * form the `Expressions`.
 */

#[derive(PartialEq, Debug)]
pub enum Syntax {
    SelfEvaluatingSyntax(SelfEvaluating),
    ProperList(Vec<Syntax>),
    ImproperList(Vec<Syntax>, Box<Syntax>),
}

#[derive(PartialEq, Debug)]
pub enum SelfEvaluating {
    FixNum(i64),
    Bool(bool),
    Symbol(String),
    Vector(Vec<Syntax>),
}

pub fn fixnum(value: i64) -> Syntax {
    Syntax::SelfEvaluatingSyntax(SelfEvaluating::FixNum(value))
}

pub fn boolean(value: bool) -> Syntax {
    Syntax::SelfEvaluatingSyntax(SelfEvaluating::Bool(value))
}

pub fn symbol(value: &str) -> Syntax {
    Syntax::SelfEvaluatingSyntax(SelfEvaluating::Symbol(String::from(value).to_lowercase()))
}

pub fn vector(value: Vec<Syntax>) -> Syntax {
    Syntax::SelfEvaluatingSyntax(SelfEvaluating::Vector(value))
}

pub fn proper_list(value: Vec<Syntax>) -> Syntax {
    Syntax::ProperList(value)
}

pub fn improper_list(head: Vec<Syntax>, tail: Syntax) -> Syntax {
    Syntax::ImproperList(head, Box::new(tail))
}
