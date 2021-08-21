// experiment if I can write an expander that does just SEXP -> SEXP transformation
// without duplicating too much parser logic
use super::error::Error;
use super::syntax::environment::{Denotation, Special, SyntaxEnvironment};
use super::Result;
use crate::compiler::frontend::parser::Parser;
use crate::compiler::frontend::reader::sexp::datum::{Datum, Sexp};
use crate::compiler::frontend::syntax;
use crate::compiler::frontend::syntax::symbol::Symbol;
use crate::compiler::source::{HasSourceLocation, Location};
use crate::compiler::Compiler;

#[derive(Debug)]
pub struct Expander {
    compiler: Compiler,
    parser: Box<Parser>,
    expansion_env: SyntaxEnvironment,
}

impl Expander {
    pub fn new() -> Self {
        Expander {
            compiler: Compiler::new(),
            parser: Box::new(Parser::new()),
            expansion_env: SyntaxEnvironment::basic(),
        }
    }

    pub fn expand(&mut self, datum: &Datum) -> Result<Datum> {
        self.expand_macros(&datum)
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
            Some(_) => Err(Error::expansion_error("Unexpected unquoted list", &datum)),
            None => Ok(datum.clone()),
        }
    }

    fn expand_all(&mut self, all: &[Datum]) -> Result<Vec<Datum>> {
        all.iter().map(|d| self.expand_macros(d)).collect()
    }

    fn expand_apply(
        &mut self,
        operator: &Datum,
        operands: &[Datum],
        loc: Location,
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
            [formals, body @ ..] => match self.parser.parse_formals(&formals) {
                Ok(parsed_formals) => {
                    self.expansion_env.push_scope();
                    for identifier in parsed_formals.identifiers() {
                        let sym = identifier.symbol().clone();
                        self.expansion_env
                            .extend(sym.clone(), Denotation::identifier(sym));
                    }
                    let expanded_body = self.expand_all(body)?;
                    self.expansion_env.pop_scope();

                    Ok(self.build_lambda(
                        &[formals.clone()],
                        &expanded_body,
                        datum.source_location().clone(),
                    ))
                }
                Err(_) => Err(Error::expansion_error(
                    "Malformed formals of lambda",
                    &datum,
                )),
            },
            // ignore invalid form?
            _ => Err(Error::expansion_error(
                "Malformed lambda expression",
                &datum,
            )),
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
                    _ => Err(Error::expansion_error(
                        "Expected procedure definition of higher order procedure definition",
                        &datum,
                    )),
                }
            }
            //(define <id> <expr>)
            [identifier, expr] => Ok(Datum::new(
                Sexp::List(vec![operator.clone(), identifier.clone(), expr.clone()]),
                datum.source_location().clone(),
            )),
            _ => Err(Error::expansion_error(
                "Expected definition or procedure definition",
                &datum,
            )),
        }
    }

    fn build_lambda(&self, def_formals: &[Datum], body: &[Datum], loc: Location) -> Datum {
        let mut lambda = vec![
            Datum::new(Sexp::Symbol(Symbol::unforgeable("lambda")), loc.clone()),
            Datum::new(Sexp::List(def_formals.to_vec()), loc.clone()),
        ];

        lambda.extend_from_slice(body);
        Datum::new(Sexp::List(lambda), loc.clone())
    }

    pub fn denotation_of(&mut self, datum: &Datum) -> Result<Denotation> {
        if let Sexp::Symbol(sym) = datum.sexp() {
            Ok(self.expansion_env.get(&sym.clone().into()))
        } else {
            Err(Error::bug("unexpected symbol to determine denotation"))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::frontend::parser::identifier::Identifier;
    use crate::compiler::frontend::reader::sexp::datum::Sexp;
    use crate::compiler::frontend::reader::tests::*;
    use crate::compiler::frontend::syntax::symbol::Symbol;
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
