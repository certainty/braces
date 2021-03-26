use super::syntax;

#[derive(PartialEq, Debug)]
pub enum Expression {
    Literal(syntax::SelfEvaluating),
}
