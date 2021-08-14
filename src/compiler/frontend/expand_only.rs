// experiment if I can write an expander that does just SEXP -> SEXP transformation
// without duplicating too much parser logic

use super::parser::syntax;
use super::parser::syntax::environment::{Denotation, Special, SyntacticContext};
use crate::compiler::frontend::parser::expression;
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

pub type Result<T> = std::result::Result<T, Error>;

pub struct Expander {
    compiler: Compiler,
    syntax_ctx: SyntacticContext,
}

impl Expander {
    pub fn new() -> Self {
        Expander {
            compiler: Compiler::new(),
            syntax_ctx: SyntacticContext::default(),
        }
    }

    pub fn expand(&mut self, datum: &Datum) -> Result<Datum> {
        self.expand_macros(&datum)
    }

    pub fn expand_quotations(&mut self, datum: &Datum) -> Result<Datum> {
        match datum.list_slice() {
            Some([operator, operands @ ..]) if operator.is_symbol() => {
                match self.denotation_of(operator)? {
                    Denotation::Special(Special::Quote) => self.expand_quote(&datum, &operands),
                    Denotation::Special(Special::QuasiQuote) => {
                        self.expand_quasi_quote(&datum, &operands)
                    }
                    Denotation::Special(Special::Unquote) => Error::expansion_error(
                        "Unexpected unquote outside of quasi-quote",
                        datum.clone(),
                    ),
                    Denotation::Special(Special::UnquoteSplicing) => Error::expansion_error(
                        "Unexpected unquote-splicing outside of quasi-quote",
                        datum.clone(),
                    ),
                    _ => Ok(datum.clone()),
                }
            }
            _ => Ok(datum.clone()),
        }
    }

    pub fn expand_macros(&mut self, datum: &Datum) -> Result<Datum> {
        match datum.list_slice() {
            Some([operator, operands @ ..]) if operator.is_symbol() => {
                let denotation = self.denotation_of(operator)?;
                match denotation {
                    Denotation::Special(special) => match special {
                        Special::Define => self.expand_define(&datum, &operator, &operands),
                        Special::Lambda => self.expand_lambda(&datum, &operator, &operands),
                        Special::DefineSyntax => todo!(),
                        Special::LetSyntax => todo!(),
                        Special::LetrecSyntax => todo!(),
                        Special::Quote => Ok(datum.clone()), // already expanded in phase 0
                        _ => self.expand_apply(operator, operands, datum.source_location().clone()),
                    },
                    Denotation::Macro(transformer) => self.expand_macro(&datum, &transformer),
                    _ => self.expand_apply(operator, operands, datum.source_location().clone()),
                }
            }
            Some([operator, operands @ ..]) => {
                self.expand_apply(operator, operands, datum.source_location().clone())
            }
            Some(_) => Error::expansion_error("Unexpected unquoted list", datum.clone()),
            None => Ok(datum.clone()),
        }
    }

    fn expand_quote(&mut self, datum: &Datum, operands: &[Datum]) -> Result<Datum> {
        if operands.len() != 1 {
            Error::expansion_error("Invalid quoted expression", datum.clone())
        } else {
            // just return the quoted expression as is
            Ok(datum.clone())
        }
    }

    // `3      -> (quasi-quote 3)               -> 3
    // `,3     -> (quasi-quote (unquote 3))     -> 3
    // `(1 2)  -> (quasi-quote (1 2))           -> (list 1 2)
    // `(1 ,2) -> (quasi-quote (1 (unquote 2))) -> (list 1 2)
    fn expand_quasi_quote(&mut self, datum: &Datum, operands: &[Datum]) -> Result<Datum> {
        if operands.len() != 1 {
            return Error::expansion_error("Invalid quasi-quoted expression", datum.clone());
        }

        let quasi_quoted_sexp = operands[0].clone();
        match quasi_quoted_sexp.sexp() {
            // (quasi-quote (..))
            Sexp::List(elements) => {
                let mut output = vec![Datum::new(
                    Sexp::Symbol(Symbol::unforgeable("list")),
                    datum.source_location().clone(),
                )];
                let expanded_elements = self.expand_quasi_quoted_elements(elements)?;
                output.extend(expanded_elements);

                Ok(Datum::new(
                    Sexp::List(output),
                    datum.source_location().clone(),
                ))
            }
            // (quasi-quote (_ . _))
            Sexp::ImproperList(head, tail) => {
                let expanded_head = self.expand_quasi_quoted_elements(head)?;
                let expanded_tail = self.expand_quasi_quoted_element(tail)?;

                let mut output = vec![Datum::new(
                    Sexp::Symbol(Symbol::unforgeable("cons")),
                    datum.source_location().clone(),
                )];
                output.extend(expanded_head);
                output.extend(expanded_tail);

                Ok(self.quote_datum(Datum::new(
                    Sexp::List(output),
                    datum.source_location().clone(),
                )))
            }
            // (quasi-quote #(..))
            Sexp::Vector(elements) => {
                let mut output = vec![Datum::new(
                    Sexp::Symbol(Symbol::unforgeable("vector")),
                    datum.source_location().clone(),
                )];

                let expanded_elements = self.expand_quasi_quoted_elements(elements)?;
                output.extend(expanded_elements);

                Ok(self.quote_datum(Datum::new(
                    Sexp::List(output),
                    datum.source_location().clone(),
                )))
            }
            // (quasi-quote datum)
            other => Ok(self.quote_datum(quasi_quoted_sexp)),
        }
    }

    fn expand_quasi_quoted_elements(&mut self, elements: &Vec<Datum>) -> Result<Vec<Datum>> {
        let mut output: Vec<Datum> = vec![];

        for element in elements {
            let expanded = self.expand_quasi_quoted_element(&element)?;
            output.extend(expanded)
        }

        Ok(output)
    }

    // unquote an element in a quasi-quoted list/cons/vector
    fn expand_quasi_quoted_element(&mut self, datum: &Datum) -> Result<Vec<Datum>> {
        match datum.list_slice() {
            Some([operator, operand]) if operator.is_symbol() => {
                match self.denotation_of(operator)? {
                    Denotation::Special(Special::QuasiQuote) => {
                        Error::expansion_error("Quasiquote can't be nested", datum.clone())
                    }
                    Denotation::Special(Special::Unquote) => {
                        Ok(vec![self.expand_quotations(operand)?])
                    }
                    Denotation::Special(Special::UnquoteSplicing) => {
                        let expanded = self.expand_quotations(operand)?;

                        match expanded.sexp() {
                            Sexp::List(inner) => Ok(inner.to_vec()),
                            Sexp::ImproperList(_, _) => {
                                return Error::expansion_error(
                                    "Unexpected improper list in unquote-splicing",
                                    expanded,
                                )
                            }
                            _ => Ok(vec![expanded]),
                        }
                    }
                    _ => Ok(vec![datum.clone()]),
                }
            }
            _ => Ok(vec![self.quote_datum(datum.clone())]),
        }
    }

    fn quote_datum(&mut self, datum: Datum) -> Datum {
        let loc = datum.source_location().clone();
        Datum::new(
            Sexp::List(vec![
                Datum::new(Sexp::forged_symbol("quote"), loc.clone()),
                datum,
            ]),
            loc,
        )
    }

    fn expand_all(&mut self, all: &[Datum]) -> Result<Vec<Datum>> {
        all.iter().map(|d| self.expand_macros(d)).collect()
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

    // expand a lambda expression
    fn expand_lambda(
        &mut self,
        datum: &Datum,
        _operator: &Datum,
        operands: &[Datum],
    ) -> Result<Datum> {
        match operands {
            [formals, body @ ..] => match expression::lambda::parse_formals(&formals) {
                Ok(parsed_formals) => {
                    self.syntax_ctx.expansion.push_scope();
                    for identifier in parsed_formals.identifiers() {
                        let sym = identifier.symbol().clone();
                        self.syntax_ctx
                            .expansion
                            .extend(sym.clone(), Denotation::identifier(sym));
                    }
                    let expanded_body = self.expand_all(body)?;
                    self.syntax_ctx.expansion.pop_scope();

                    Ok(self.build_lambda(
                        &[formals.clone()],
                        &expanded_body,
                        datum.source_location().clone(),
                    ))
                }
                Err(_) => Error::expansion_error("Malformed formals of lambda", datum.clone()),
            },
            // ignore invalid form?
            _ => Error::expansion_error("Malformed lambda expression", datum.clone()),
        }
    }

    fn expand_macro(&mut self, datum: &Datum, transformer: &syntax::Transformer) -> Result<Datum> {
        todo!()
    }

    fn expand_define(
        &mut self,
        datum: &Datum,
        operator: &Datum,
        operands: &[Datum],
    ) -> Result<Datum> {
        match operands {
            //(define (...) <body>)
            [definition, exprs @ ..] if definition.sexp().is_proper_list() => {
                match definition.list_slice() {
                    //(define (<ID> <def-formals> <body>)
                    Some([identifier, def_formals @ ..]) => {
                        let lambda =
                            self.build_lambda(def_formals, exprs, datum.source_location().clone());

                        Ok(Datum::new(
                            Sexp::List(vec![operator.clone(), identifier.clone(), lambda]),
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
            [identifier, expr] => Ok(Datum::new(
                Sexp::List(vec![operator.clone(), identifier.clone(), expr.clone()]),
                datum.source_location().clone(),
            )),
            _ => {
                Error::expansion_error("Expected definition or procedure definition", datum.clone())
            }
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::expression::identifier::Identifier;
    use crate::compiler::frontend::parser::syntax::Symbol;
    use crate::compiler::frontend::reader::sexp::datum::Sexp;
    use crate::compiler::frontend::test_helpers::expressions::*;
    use std::fmt::Debug;

    #[test]
    fn test_expand_atoms() -> Result<()> {
        assert_expands_equal("#t", "#t", true)?;
        assert_expands_equal("#\\a", "#\\a", true)?;
        Ok(())
    }

    #[test]
    fn test_expand_define_simple() -> Result<()> {
        assert_expands_equal("(define x #t)", "(define x #t)", true)?;
        Ok(())
    }

    #[test]
    fn test_expand_define_procedure() -> Result<()> {
        assert_expands_equal(
            "(define (foo x y) x)",
            "(define foo (lambda (x y) x))",
            false,
        )?;
        Ok(())
    }

    #[test]
    fn test_expand_quote() -> Result<()> {
        assert_expands_equal("'3", "'3", true)?;
        assert_expands_equal("'(1 2 3)", "'(1 2 3)", true)?;
        Ok(())
    }

    #[test]
    fn test_expand_quasi_quote() -> Result<()> {
        assert_expands_equal("`3", "'3", false)?;
        assert_expands_equal("`(3 4 ,5)", "(list '3 '4 5)", false)?;
        assert_expands_equal("`(3 4 ,@(5 6 7 8))", "(list '3 '4 5 6 7 8)", false)?;
        assert_expands_equal("`(3 4 ,(car x y))", "(list '3 '4 (car x y))", false)?;
        assert_expands_equal("`(3 4 ,'(1 2))", "(list '3 '4 '(1 2))", false)?;
        assert_expands_equal("''foo", "'(quote foo)", false)?;

        assert!(expand_form("`(1 2 `3)").is_err(), "expected error");
        Ok(())
    }

    fn assert_expands_equal(lhs: &str, rhs: &str, pedantic: bool) -> Result<()> {
        let mut exp = Expander::new();
        let actual_sexp = parse_datum(lhs);
        let expected_sexp = parse_datum(rhs);
        let expanded = exp.expand(&actual_sexp)?;

        assert_struct_eq(&expanded, &expected_sexp, pedantic);
        Ok(())
    }

    fn expand_form(form: &str) -> Result<Datum> {
        let mut exp = Expander::new();
        exp.expand(&parse_datum(form))
    }

    fn assert_struct_eq(lhs: &Datum, rhs: &Datum, pedantic: bool) {
        match (lhs.sexp(), rhs.sexp()) {
            (Sexp::List(inner_lhs), Sexp::List(inner_rhs)) => {
                assert_vec_eq(&inner_lhs, &inner_rhs, |l, r| {
                    assert_struct_eq(l, r, pedantic)
                })
            }
            (Sexp::ImproperList(head_lhs, tail_lhs), Sexp::ImproperList(head_rhs, tail_rhs)) => {
                assert_vec_eq(&head_lhs, &head_rhs, |l, r| {
                    assert_struct_eq(l, r, pedantic)
                });
                assert_struct_eq(&tail_lhs, &tail_rhs, pedantic);
            }
            (Sexp::Vector(inner_lhs), Sexp::Vector(inner_rhs)) => {
                assert_vec_eq(&inner_lhs, &inner_rhs, |l, r| {
                    assert_struct_eq(l, r, pedantic)
                })
            }
            (Sexp::Symbol(inner_lhs), Sexp::Symbol(inner_rhs)) if !pedantic => {
                assert_eq!(inner_lhs.as_str(), inner_rhs.as_str())
            }
            (sexp_lhs, sexp_rhs) => {
                assert_eq!(sexp_lhs, sexp_rhs)
            }
        }
    }

    fn assert_vec_eq<T, F>(lhs: &Vec<T>, rhs: &Vec<T>, assertion: F)
    where
        F: Copy + FnOnce(&T, &T),
        T: Debug,
    {
        if lhs.len() == 0 {
            assert_eq!(0, rhs.len(), "expected length of both vectors to be 0")
        } else {
            for i in 0..lhs.len() {
                assertion(&lhs[i], &rhs[i])
            }
        }
    }
}
