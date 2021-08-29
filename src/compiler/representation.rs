use super::frontend::parser::Expression;
use crate::compiler::frontend::reader::datum::Datum;

#[derive(Clone, Debug)]
pub struct CoreAST {
    pub expressions: Vec<Expression>,
}

impl CoreAST {
    pub fn new(expressions: Vec<Expression>) -> Self {
        Self { expressions }
    }
}

#[derive(Clone, Debug)]
pub struct SexpAST {
    sexps: Vec<Datum>,
}

impl SexpAST {
    pub fn new(sexps: Vec<Datum>) -> Self {
        Self { sexps }
    }

    pub fn to_vec(&self) -> &Vec<Datum> {
        &self.sexps
    }

    pub fn first(&self) -> &Datum {
        debug_assert!(!self.sexps.is_empty());
        &self.sexps[0]
    }
}
