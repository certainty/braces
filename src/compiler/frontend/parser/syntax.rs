use super::Location;
/*
 * Syntax represents everything that is part of the external representation. So a datum in that sense.
 * Scheme programs are made out of syntax at this level. There are higher order constructs created from these, which
 * form the `Expressions`.
 */

// TODO: add position tracking - can be extracted from the pest spans

#[derive(PartialEq, Debug)]
pub enum Syntax {
    SelfEvaluatingSyntax(SelfEvaluating, Location),
    ProperList(Vec<Syntax>, Location),
    ImproperList(Vec<Syntax>, Box<Syntax>, Location),
}

#[derive(PartialEq, Debug)]
pub enum SelfEvaluating {
    FixNum(i64),
    Bool(bool),
    Symbol(String),
    Vector(Vec<Syntax>),
}

pub fn fixnum(value: i64, location: Location) -> Syntax {
    Syntax::SelfEvaluatingSyntax(SelfEvaluating::FixNum(value), location)
}

pub fn boolean(value: bool, location: Location) -> Syntax {
    Syntax::SelfEvaluatingSyntax(SelfEvaluating::Bool(value), location)
}

pub fn symbol(value: &str, location: Location) -> Syntax {
    Syntax::SelfEvaluatingSyntax(
        SelfEvaluating::Symbol(String::from(value).to_lowercase()),
        location,
    )
}

pub fn vector(value: Vec<Syntax>, location: Location) -> Syntax {
    Syntax::SelfEvaluatingSyntax(SelfEvaluating::Vector(value), location)
}

pub fn proper_list(value: Vec<Syntax>, location: Location) -> Syntax {
    Syntax::ProperList(value, location)
}

pub fn improper_list(head: Vec<Syntax>, tail: Syntax, location: Location) -> Syntax {
    Syntax::ImproperList(head, Box::new(tail), location)
}
