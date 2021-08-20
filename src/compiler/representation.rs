use super::frontend::parser::Expression;
use super::frontend::reader::sexp::datum::Datum;

#[derive(Clone, Debug)]
pub struct CoreAST {
    expressions: Vec<Expression>,
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
}
