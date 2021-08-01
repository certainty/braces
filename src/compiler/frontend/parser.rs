pub mod ast;
pub mod expression;
pub mod syntax;
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
    syntax_ctx: SyntacticContext,
    expander: MacroExpander,
    compiler: Compiler,
}

impl ParserContext {
    pub fn new(ctx: SyntacticContext, expander: MacroExpander, compiler: Compiler) -> Self {
        let mut parser_ctx = ParserContext {
            syntax_ctx: ctx,
            expander: expander,
            compiler: compiler,
        };
        parser_ctx.register_transformers();
        parser_ctx
    }

    fn register_transformers(&mut self) {
        self.expander
            .register_transformers(self.syntax_ctx.usual_env());
    }

    pub fn denotation_of(&mut self, datum: &Datum) -> Denotation {
        if let Sexp::Symbol(sym) = datum.sexp() {
            self.syntax_ctx.usual_env().get(&sym.clone().into())
        } else {
            panic!("[BUG] expected symbol")
        }
    }

    pub fn expand_macro(
        &mut self,
        datum: &Datum,
        transformer: syntax::Transformer,
    ) -> Result<Datum> {
        let transformed = self.expander.expand(&datum, transformer)?;
        Ok(transformed)
    }

    pub fn define_syntax(
        &mut self,
        name: syntax::Symbol,
        transformer: syntax::Transformer,
    ) -> Result<()> {
        let denotation = Denotation::Macro(transformer);
        self.syntax_ctx.usual.extend(name, denotation);
        Ok(())
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
                    Denotation::Macro(transformer) => {
                        let expanded = ctx.expand_macro(&datum, transformer)?;
                        exparse0(expanded, ctx)
                    }
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
            define_syntax(&datum, ctx)?;
            Ok(None)
        }
        Special::LetSyntax => todo!(),
        Special::LetrecSyntax => todo!(),
    }
}

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
