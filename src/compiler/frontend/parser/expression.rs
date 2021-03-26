use super::syntax;

#[repr(transparent)]
#[derive(PartialEq, Debug)]
pub struct Symbol {
    inner: String,
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Variable(Symbol),
    Literal(syntax::SelfEvaluating),
}
