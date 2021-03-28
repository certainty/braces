use super::SourceInformation;
use crate::vm::value::Value;

#[repr(transparent)]
#[derive(PartialEq, Debug, Clone)]
pub struct Symbol {
    inner: String,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Variable(pub Symbol);

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Variable(Variable, SourceInformation),
    Literal(Value, SourceInformation),
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

pub fn literal(value: Value, source_info: SourceInformation) -> Expression {
    Expression::Literal(value, source_info)
}

pub fn application(
    op: Expression,
    operands: Vec<Expression>,
    source_info: SourceInformation,
) -> Expression {
    Expression::Application(Box::new(op), operands, source_info)
}
