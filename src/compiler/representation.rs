use super::frontend::expression::Expression;
use super::frontend::reader::sexp::datum::Datum;

pub struct CoreAST {
    expressions: Vec<Expression>,
}

pub struct SexpAST {
    sexps: Vec<Datum>,
}
