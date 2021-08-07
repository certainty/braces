// experiment if I can write an expander that does just SEXP -> SEXP transformation
// without duplicating too much parser logic

use super::parser::syntax;
use super::parser::syntax::environment::{
    Denotation, Special, SyntacticContext, SyntaxEnvironment,
};
use crate::compiler::frontend::parser::syntax::Symbol;
use crate::compiler::frontend::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::source_location::{HasSourceLocation, SourceLocation};
use crate::compiler::Compiler;
use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum Error {
    #[error("ExpansionError: {} {:#?}", 0, 1)]
    ExpansionError(String, Datum),
    #[error("Bug in expander: {}", 0)]
    Bug(String),
}

impl Error {
    pub fn expansion_error<T: Into<String>, O>(msg: T, datum: Datum) -> Result<O> {
        Err(Error::ExpansionError(msg.into(), datum))
    }

    pub fn bug<T: Into<String>, O>(msg: T) -> Result<O> {
        Err(Error::Bug(msg.into()))
    }
}

type Result<T> = std::result::Result<T, Error>;

struct Expander {
    compiler: Compiler,
    syntax_ctx: SyntacticContext,
}

impl Expander {
    pub fn expand(&mut self, datum: &Datum) -> Result<Datum> {
        match datum.list_slice() {
            Some([operator, operands @ ..]) if operator.is_symbol() => {
                let denotation = self.denotation_of(operator)?;
                match denotation {
                    Denotation::Special(special) => match special {
                        Special::Define => self.expand_define(&datum, &operands),
                        Special::Quote => todo!(),
                        Special::Lambda => todo!(),
                        Special::Set => todo!(),
                        Special::Begin => todo!(),
                        Special::If => todo!(),
                        Special::DefineSyntax => todo!(),
                        Special::LetSyntax => todo!(),
                        Special::LetrecSyntax => todo!(),
                    },
                    Denotation::Macro(transformer) => self.expand_macro(&datum, &transformer),
                    Denotation::Id(_) => {
                        self.expand_apply(operator, operands, datum.source_location().clone())
                    }
                    Denotation::Global(_) => {
                        panic!("REMOVE GLOBAL")
                    }
                }
            }
            Some([operator, operands @ ..]) => {
                self.expand_apply(operator, operands, datum.source_location().clone())
            }
            Some(_) => Error::expansion_error("Unexpected unquoted list", datum.clone()),
            None => Ok(datum.clone()),
        }
    }

    fn expand_all(&mut self, all: &[Datum]) -> Result<Vec<Datum>> {
        all.iter().map(|d| self.expand(d)).collect()
    }

    fn expand_apply(
        &mut self,
        operator: &Datum,
        operands: &[Datum],
        loc: SourceLocation,
    ) -> Result<Datum> {
        let mut new_ls = vec![self.expand(operator)?];
        new_ls.extend(self.expand_all(operands)?);

        Ok(Datum::new(Sexp::list(new_ls.into_iter()), loc))
    }

    fn expand_macro(&mut self, datum: &Datum, transformer: &syntax::Transformer) -> Result<Datum> {
        todo!()
    }

    fn expand_define(&mut self, datum: &Datum, operands: &[Datum]) -> Result<Datum> {
        match operands {
            //(define (...) <body>)
            [definition, exprs @ ..] if definition.sexp().is_proper_list() => {
                match definition.list_slice() {
                    //(define (<ID> <def-formals> <body>)
                    Some([identifier, def_formals @ ..]) => {
                        let lambda =
                            self.build_lambda(def_formals, exprs, datum.source_location().clone());

                        Ok(self.build_define(
                            identifier.clone(),
                            lambda,
                            datum.source_location().clone(),
                        ))
                    }

                    // (define ((<ID> <def-formals>) <body>))
                    _ => Error::expansion_error(
                        "Expected procedure definition of higher order procedure definition",
                        datum.clone(),
                    ),
                }
            }
            //(define <id> <expr>)
            [identifier, expr] => Ok(self.build_define(
                identifier.clone(),
                expr.clone(),
                datum.source_location().clone(),
            )),
            _ => {
                Error::expansion_error("Expected definition or procedure definition", datum.clone())
            }
        }
    }

    fn build_define(&self, id: Datum, expr: Datum, loc: SourceLocation) -> Datum {
        Datum::new(
            Sexp::list(vec![Datum::new(Sexp::symbol("define"), loc.clone()), id, expr].into_iter()),
            loc,
        )
    }

    fn build_lambda(&self, def_formals: &[Datum], body: &[Datum], loc: SourceLocation) -> Datum {
        let mut lambda = vec![
            Datum::new(Sexp::Symbol(Symbol::unforgeable("lambda")), loc.clone()),
            Datum::new(Sexp::List(def_formals.to_vec()), loc.clone()),
        ];

        lambda.extend_from_slice(body);
        Datum::new(Sexp::List(lambda), loc.clone())
    }

    fn denotation_of(&mut self, datum: &Datum) -> Result<Denotation> {
        if let Sexp::Symbol(sym) = datum.sexp() {
            Ok(self.syntax_ctx.usual_env().get(&sym.clone().into()))
        } else {
            Error::bug("expected symbol")
        }
    }
}
