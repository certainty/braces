use super::syntax;

pub enum Expression {
    Literal(syntax::SelfEvaluating),
}
