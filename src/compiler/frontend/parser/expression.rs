use super::{syntax, SourceInformation};

#[repr(transparent)]
#[derive(PartialEq, Debug)]
pub struct Symbol {
    inner: String,
}

#[derive(PartialEq, Debug)]
pub struct Variable(pub Symbol);

#[derive(PartialEq, Debug)]
pub enum Expression {
    Variable(Variable, SourceInformation),
    Literal(syntax::SelfEvaluating, SourceInformation),
    Begin(Vec<Expression>, SourceInformation),
    Assign(Variable, Box<Expression>),
    If(
        Box<Expression>,
        Box<Expression>,
        Option<Box<Expression>>,
        SourceInformation,
    ),
    Application(Box<Expression>, Vec<Expression>, SourceInformation),
}

pub fn variable(symbol: &str, source_info: SourceInformation) -> Expression {
    Expression::Variable(Variable(self::symbol(symbol.into())), source_info)
}

pub fn symbol(str: String) -> Symbol {
    Symbol { inner: str }
}

pub fn literal(syn: syntax::SelfEvaluating, source_info: SourceInformation) -> Expression {
    Expression::Literal(syn, source_info)
}
