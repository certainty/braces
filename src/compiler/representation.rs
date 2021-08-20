use super::frontend::parser::Expression;
use super::frontend::reader::sexp::datum::Datum;

pub struct CoreAST {
    expressions: Vec<Expression>,
}

pub struct SexpAST {
    sexps: Vec<Datum>,
}
