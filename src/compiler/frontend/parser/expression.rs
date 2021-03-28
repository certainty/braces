use super::SourceInformation;
use crate::vm::value::symbol::Symbol;
use crate::vm::value::Value;

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Variable(Symbol, SourceInformation),
    Literal(Value, SourceInformation),
    Begin(Vec<Expression>, SourceInformation),
    //Assign(Symbol, Box<Expression>),
    If(
        Box<Expression>,
        Box<Expression>,
        Option<Box<Expression>>,
        SourceInformation,
    ),
    Application(Box<Expression>, Vec<Expression>, SourceInformation),
}

pub fn variable(symbol: &str, source_info: SourceInformation) -> Expression {
    Expression::Variable(Symbol(symbol.to_string()), source_info)
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
