pub mod ast;
pub mod expression;
pub mod syntax;
use super::expand_only;
use super::reader;
use super::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::frontend::expander::MacroExpander;
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};
use crate::compiler::Compiler;
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
    ExpanderError(#[from] expand_only::Error),
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

// TODO: design this properly
//
// The Parser, ParserContext and Expander are currently weiredly intertwined with no
// clear boundaries for responsibilities. This needs untangling to be easier to understand.
// A few things for thought:
// 1. the parser context is handed into every parser function at this level. Why not make parser have state and have associated functions?
// 2. What state does the expander need? How is it different from the parser state?
//
//

pub struct ParserContext {
    // for parser?
    syntax_ctx: SyntacticContext,
    expander: expand_only::Expander,
}

impl ParserContext {
    pub fn new(ctx: SyntacticContext, expander: MacroExpander, compiler: Compiler) -> Self {
        let mut parser_ctx = ParserContext {
            syntax_ctx: ctx,
            expander: expand_only::Expander::new(),
        };
        parser_ctx.register_transformers();
        parser_ctx
    }

    fn register_transformers(&mut self) {
        // register transformers
        ()
    }

    pub fn denotation_of(&mut self, datum: &Datum) -> Result<Denotation> {
        if let Sexp::Symbol(sym) = datum.sexp() {
            Ok(self.syntax_ctx.usual_env().get(&sym.clone().into()))
        } else {
            Error::parser_bug("[BUG] expected symbol")
        }
    }
}

impl Default for ParserContext {
    fn default() -> Self {
        ParserContext::new(
            SyntacticContext::default(),
            MacroExpander::new(),
            Compiler::new(),
        )
    }
}

// expand and parse
pub fn parse_all(data: Vec<Datum>, ctx: &mut ParserContext) -> Result<Ast> {
    let mut parsed: Vec<Expression> = vec![];

    for datum in data {
        parsed.push(parse(datum, ctx)?);
    }

    Ok(Ast {
        expressions: parsed,
    })
}

pub fn parse(datum: Datum, ctx: &mut ParserContext) -> Result<Expression> {
    let expanded = ctx.expander.expand(&datum)?;
    parse_core(expanded, ctx)
}

pub fn parse_core(datum: Datum, ctx: &mut ParserContext) -> Result<Expression> {
    match datum.sexp() {
        Sexp::List(ls) => match &ls[..] {
            [operator, _operands @ ..] if operator.is_symbol() => {
                let denotation = ctx.denotation_of(operator)?;
                match denotation {
                    Denotation::Special(special) => exparse_special(special, &datum, ctx),
                    Denotation::Global(_) => expression::apply::parse(&datum, ctx).res(),
                    Denotation::Id(_) => expression::apply::parse(&datum, ctx).res(),
                    _ => {
                        return Error::parser_bug(&format!(
                            "Unexpected denotation for datum: {:?}",
                            denotation.clone()
                        ))
                    }
                }
            }
            [_operator, _operands @ ..] => expression::apply::parse(&datum, ctx).res(),
            _ => Err(Error::ExpansionError(
                "Unexpected unquoted list".to_string(),
                datum.clone(),
            )),
        },
        _ => Expression::parse(&datum, ctx),
    }
}

fn exparse_special(
    special_form: Special,
    datum: &Datum,
    ctx: &mut ParserContext,
) -> Result<Expression> {
    match special_form {
        Special::Define => expression::define::parse(&datum, ctx).res(),
        Special::Quote => expression::quotation::parse(&datum, ctx).res(),
        Special::QuasiQuote => expression::quotation::parse(&datum, ctx).res(),
        Special::Lambda => expression::lambda::parse(&datum, ctx).res(),
        Special::Set => expression::assignment::parse(&datum, ctx).res(),
        Special::Begin => expression::sequence::parse(&datum, ctx).res(),
        Special::If => expression::conditional::parse(&datum, ctx).res(),
        _ => return Error::parser_bug(&format!("Unexpected special form: {:?}", special_form)),
    }
}

/*
fn define_syntax(datum: &Datum, ctx: &mut ParserContext) -> Result<()> {
    match datum.sexp() {
        Sexp::List(ls) => match &ls[..] {
            [_, definition @ ..] => define_syntax0(datum, definition, ctx),
            _ => Error::parse_error(
                "Invalid (define-syntax) form",
                datum.source_location().clone(),
            ),
        },
        _ => Error::parser_bug("Unrecognized (define-syntax)"),
    }
}

fn define_syntax0(
    definition: &Datum,
    macro_definition: &[Datum],
    ctx: &mut ParserContext,
) -> Result<()> {
    match macro_definition {
        // (define-syntax foo (er-macro-transformer (lambda (exp rename compare?) ...))
        [macro_def, name, def] => match (name.sexp(), def.sexp()) {
            (Sexp::Symbol(name), Sexp::List(ls)) => match &ls[..] {
                [expander, lambda_def] => {
                    if let Some("er-macro-transformer") = self::match_symbol(expander) {
                        println!("Compiling lambda def: {:#?}", lambda_def);
                        match ctx.compiler.compile_lambda(lambda_def) {
                            Ok(procedure) => {
                                println!("Transformer closure: {:#?}", procedure);
                                ctx.define_syntax(
                                    name.clone(),
                                    syntax::Transformer::ExplicitRenaming(procedure),
                                )?;
                                Ok(())
                            }
                            Err(_) => Error::parse_error(
                                "Couldn't compile transformer procedure",
                                definition.source_location().clone(),
                            ),
                        }
                    } else {
                        Error::parse_error(
                            "Invalid macro expander",
                            definition.source_location().clone(),
                        )
                    }
                }
                _ => Error::parse_error(
                    "Invalid macro definition",
                    macro_def.source_location().clone(),
                ),
            },
            _ => Error::parse_error(
                "Invalid macro definition",
                macro_def.source_location().clone(),
            ),
        },
        _ => Error::parse_error(
            "Invalid macro definition",
            definition.source_location().clone(),
        ),
    }
}

fn match_symbol<'a>(datum: &'a Datum) -> Option<&'a str> {
    match datum.sexp() {
        Sexp::Symbol(s) => Some(s.as_str()),
        _ => None,
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
 */

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

    #[test]
    fn test_parse_let_simple() {
        let expr = exparse_form("(let ((x 1) (y 2)) x)");
        let expected_lambda = Expression::lambda(
            expression::lambda::Formals::ArgList(vec![
                Identifier::synthetic("x"),
                Identifier::synthetic("y"),
            ]),
            Expression::identifier("x", location(1, 10)).to_body_expression(),
            Some(String::from("let")),
            location(1, 10),
        );

        assert_eq!(
            expr,
            Some(Expression::apply(
                expected_lambda,
                vec![
                    Expression::constant(make_datum(Sexp::number(1), 1, 7)),
                    Expression::constant(make_datum(Sexp::number(2), 1, 13))
                ],
                location(1, 1)
            ))
        )
    }

    fn exparse_form(form: &str) -> Option<Expression> {
        let mut ctx = ParserContext::default();
        exparse0(parse_datum(&form), &mut ctx).unwrap()
    }
}
