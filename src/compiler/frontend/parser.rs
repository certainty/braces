pub mod ast;
pub mod expression;
pub mod syntax;
use super::reader;
use super::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};
use ast::Ast;
use expression::Expression;
use syntax::environment::{Denotation, Special, SyntacticContext, SyntaxEnvironment};
use thiserror::Error;

type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug, Clone)]
pub enum Error {
    #[error(transparent)]
    ReadError(#[from] reader::Error),
    #[error("ParseError")]
    ParseError(String, SourceLocation),
    #[error("DomainError")]
    DomainError(String, SourceLocation),
    #[error("ExpansionError: {} {:#?}", 0, 1)]
    ExpansionError(String, Datum),
}

impl Error {
    pub fn parse_error<T>(message: &str, source: SourceLocation) -> Result<T> {
        Err(Error::ParseError(message.to_string(), source))
    }

    pub fn domain_error<T>(message: &str, source: SourceLocation) -> Result<T> {
        Err(Error::DomainError(message.to_string(), source))
    }
}

pub struct ParserContext {
    syntax_ctx: SyntacticContext,
}

impl ParserContext {
    pub fn denotation_of(&mut self, datum: &Datum) -> Denotation {
        if let Sexp::Symbol(sym) = datum.sexp() {
            self.syntax_ctx.current_env().get(&sym.clone().into())
        } else {
            panic!("[BUG] expected symbol")
        }
    }

    pub fn define_syntax(&mut self, op: &Datum, operands: &[Datum]) -> Result<()> {
        todo!()
    }
}

impl Default for ParserContext {
    fn default() -> Self {
        ParserContext {
            syntax_ctx: SyntacticContext::default(),
        }
    }
}

// expand and parse
pub fn parse_all(data: Vec<Datum>, ctx: &mut ParserContext) -> Result<Ast> {
    let mut parsed: Vec<Expression> = vec![];

    for datum in data {
        if let Some(core_expression) = exparse0(datum, ctx)? {
            parsed.push(core_expression)
        }
    }

    Ok(Ast {
        expressions: parsed,
    })
}

pub fn parse(datum: Datum, ctx: &mut ParserContext) -> Result<Ast> {
    if let Some(parsed) = exparse0(datum, ctx)? {
        Ok(Ast {
            expressions: vec![parsed],
        })
    } else {
        Ok(Ast {
            expressions: vec![],
        })
    }
}

fn exparse0(datum: Datum, ctx: &mut ParserContext) -> Result<Option<Expression>> {
    match datum.sexp() {
        Sexp::List(ls) => match &ls[..] {
            [operator, operands @ ..] if operator.is_symbol() => {
                match ctx.denotation_of(operator) {
                    Denotation::Special(special) => {
                        exparse_special(special, operator, operands, ctx)
                    }
                    Denotation::Macro => Ok(Some(transcribe(&datum, ctx)?)),
                    Denotation::Global(id) => Ok(Some(exparse_apply(&datum, ctx)?)),
                    Denotation::Id(id) => Ok(Some(exparse_apply(&datum, ctx)?)),
                }
            }
            [_operator, _operands @ ..] => Ok(Some(exparse_apply(&datum, ctx)?)),
            _ => Err(Error::ExpansionError(
                "Unexpected unquoted list".to_string(),
                datum.clone(),
            )),
        },
        _ => Ok(Some(Expression::parse(&datum, ctx)?)),
    }
}

fn exparse_special(
    special_form: Special,
    operator: &Datum,
    operands: &[Datum],
    ctx: &mut ParserContext,
) -> Result<Option<Expression>> {
    match special_form {
        Special::Define => Ok(Some(transcribe_define(&operator, &operands, ctx)?)),
        Special::Quote => Ok(Some(exparse_quote(&operator, &operands)?)),
        Special::Lambda => todo!(),
        Special::Set => todo!(),
        Special::Begin => todo!(),
        Special::If => todo!(),
        Special::DefineSyntax => {
            ctx.define_syntax(&operator, &operands)?;
            Ok(None)
        }
        Special::LetSyntax => todo!(),
        Special::LetrecSyntax => todo!(),
    }
}

fn exparse_quote(operator: &Datum, operands: &[Datum]) -> Result<Expression> {
    todo!()
}

fn exparse_apply(datum: &Datum, ctx: &mut ParserContext) -> Result<Expression> {
    expression::apply::parse(&datum, ctx).map_non_applicable(Error::parse_error(
        "Invalid application",
        datum.source_location().clone(),
    ))
}

fn transcribe(datum: &Datum, ctx: &mut ParserContext) -> Result<Expression> {
    todo!()
}

fn transcribe_define(
    operator: &Datum,
    operands: &[Datum],
    ctx: &mut ParserContext,
) -> Result<Expression> {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::reader::sexp::datum::Sexp;
    use crate::compiler::frontend::test_helpers::expressions::*;

    #[test]
    fn test_expand_atoms() {
        let expr = exparse_form("#t");
        assert_eq!(
            expr,
            Some(Expression::constant(make_datum(Sexp::Bool(true), 1, 1)))
        );

        let expr = exparse_form("#\\a");
        assert_eq!(
            expr,
            Some(Expression::constant(make_datum(Sexp::Char('a'), 1, 1)))
        )
    }

    #[test]
    fn test_expand_special_if() {
        let expr = exparse_form("(if #t 0 1)");

        assert_eq!(
            expr,
            Some(Expression::conditional(
                Expression::constant(make_datum(Sexp::Bool(true), 1, 3)),
                Expression::constant(make_datum(Sexp::number(0), 1, 4)),
                Some(Expression::constant(make_datum(Sexp::number(1), 1, 4))),
                location(1, 1)
            ))
        )
    }

    fn exparse_form(form: &str) -> Option<Expression> {
        let mut ctx = ParserContext::default();
        exparse0(parse_datum(&form), &mut ctx).unwrap()
    }
}
