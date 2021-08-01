pub mod ast;
pub mod expression;
pub mod syntax;
use super::reader;
use super::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::frontend::expander::MacroExpander;
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};
use ast::Ast;
use expression::Expression;
use syntax::environment::{Denotation, Special, SyntacticContext, SyntaxEnvironment};
use thiserror::Error;

type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug, Clone)]
pub enum Error {
    #[error("ParserBug: {}", 0)]
    Bug(String),
    #[error(transparent)]
    ReadError(#[from] reader::Error),
    #[error("ParseError")]
    ParseError(String, SourceLocation),
    #[error("DomainError")]
    DomainError(String, SourceLocation),
    #[error("ExpansionError: {} {:#?}", 0, 1)]
    ExpansionError(String, Datum),
    #[error(transparent)]
    ExpanderError(#[from] super::expander::Error),
}

impl Error {
    pub fn parse_error<T>(message: &str, source: SourceLocation) -> Result<T> {
        Err(Error::ParseError(message.to_string(), source))
    }

    pub fn domain_error<T>(message: &str, source: SourceLocation) -> Result<T> {
        Err(Error::DomainError(message.to_string(), source))
    }

    pub fn parser_bug<T>(message: &str) -> Result<T> {
        Err(Error::Bug(message.to_string()))
    }
}

pub struct ParserContext {
    syntax_ctx: SyntacticContext,
    expander: MacroExpander,
}

impl ParserContext {
    pub fn denotation_of(&mut self, datum: &Datum) -> Denotation {
        if let Sexp::Symbol(sym) = datum.sexp() {
            self.syntax_ctx.usual_env().get(&sym.clone().into())
        } else {
            panic!("[BUG] expected symbol")
        }
    }

    pub fn expand_macro(&mut self, datum: &Datum) -> Result<Expression> {
        let expr = self.expander.expand(&datum)?;
        Ok(expr)
    }

    pub fn define_syntax(&mut self, definition: &Datum) -> Result<()> {
        todo!()
    }
}

impl Default for ParserContext {
    fn default() -> Self {
        ParserContext {
            syntax_ctx: SyntacticContext::default(),
            expander: MacroExpander::new(),
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

// Expand and parse form to core form
//
// The code has to keep track of the lexical scope while it's walking the datum.
// This is required to identify operators correctly and accurately decided if we're dealing
// with a core form or a user defined operator. As an example considere the following valid form
//
// (let ((if (lambda (x) x)))
//   (if #f))
//
// The application of `if` at that point refers to the binding introduced by `let`
// and not the core `if` form. This is the same for every other operator.
//
// So what this does is:
// 1: if `datum` is a list
//    1a: determine if `car` of list is a special form, a macro or an identifier
//    1b: if it's a special form, transform it and parse it as a special form
//    1c: if it's a macro, apply the macro to the datum and desugar the resuling definition
//    1d: if it's an identifier parse it as an application or a quote
// 2: if `datum` is not a list
//    2a: parse it as to core form
//
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
            [operator, _operands @ ..] if operator.is_symbol() => {
                let denotation = ctx.denotation_of(operator);
                println!("Denotation is: {:?}", denotation);
                match denotation {
                    Denotation::Special(special) => exparse_special(special, &datum, ctx),
                    Denotation::Macro => Ok(Some(ctx.expand_macro(&datum)?)),
                    Denotation::Global(_) => Ok(Some(expression::apply::parse(&datum, ctx).res()?)),
                    Denotation::Id(_) => Ok(Some(expression::apply::parse(&datum, ctx).res()?)),
                }
            }
            [_operator, _operands @ ..] => Ok(Some(expression::apply::parse(&datum, ctx).res()?)),
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
    datum: &Datum,
    ctx: &mut ParserContext,
) -> Result<Option<Expression>> {
    match special_form {
        Special::Define => Ok(Some(transcribe_define(&datum, ctx)?)),
        Special::Quote => Ok(Some(exparse_quote(&datum)?)),
        Special::Lambda => Ok(Some(expression::lambda::parse(&datum, ctx).res()?)),
        Special::Set => Ok(Some(expression::assignment::parse(&datum, ctx).res()?)),
        Special::Begin => Ok(Some(expression::sequence::parse(&datum, ctx).res()?)),
        Special::If => Ok(Some(expression::conditional::parse(&datum, ctx).res()?)),
        Special::DefineSyntax => {
            ctx.define_syntax(&datum)?;
            Ok(None)
        }
        Special::LetSyntax => todo!(),
        Special::LetrecSyntax => todo!(),
    }
}

fn exparse_quote(datum: &Datum) -> Result<Expression> {
    todo!()
}

fn exparse_apply(datum: &Datum, ctx: &mut ParserContext) -> Result<Expression> {
    expression::apply::parse(&datum, ctx).map_non_applicable(Error::parse_error(
        "Invalid application",
        datum.source_location().clone(),
    ))
}

fn transcribe_define(datum: &Datum, ctx: &mut ParserContext) -> Result<Expression> {
    let transcribed = ctx.expander.expand_define(&datum)?;
    println!("transcribed define {:#?}", transcribed);
    expression::define::parse(&transcribed, ctx).res()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::expression::identifier::Identifier;
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
                Expression::constant(make_datum(Sexp::Bool(true), 1, 5)),
                Expression::constant(make_datum(Sexp::number(0), 1, 8)),
                Some(Expression::constant(make_datum(Sexp::number(1), 1, 10))),
                location(1, 1)
            ))
        )
    }

    #[test]
    fn test_parse_define_simple() {
        let expr = exparse_form("(define x 10)");

        assert_eq!(
            expr,
            Some(Expression::define(
                Identifier::synthetic("x"),
                Expression::constant(make_datum(Sexp::number(10), 1, 11)),
                location(1, 1)
            ))
        )
    }

    #[test]
    fn test_parse_define_simple_symbol() {
        let expr = exparse_form("(define x 'foo)");

        assert_eq!(
            expr,
            Some(Expression::define(
                Identifier::synthetic("x"),
                Expression::quoted_value(make_datum(Sexp::symbol("foo"), 1, 12)),
                location(1, 1)
            ))
        )
    }

    #[test]
    fn test_parse_define_procedure() {
        let expr = exparse_form("(define (foo x y) x)");

        assert_eq!(
            expr,
            Some(Expression::define(
                Identifier::synthetic("foo"),
                Expression::lambda(
                    expression::lambda::Formals::ArgList(vec![
                        Identifier::synthetic("x"),
                        Identifier::synthetic("y")
                    ]),
                    Expression::identifier("x", location(1, 19)).to_body_expression(),
                    Some(String::from("lambda")),
                    location(1, 10)
                ),
                location(1, 2)
            ))
        )
    }

    fn exparse_form(form: &str) -> Option<Expression> {
        let mut ctx = ParserContext::default();
        exparse0(parse_datum(&form), &mut ctx).unwrap()
    }
}
