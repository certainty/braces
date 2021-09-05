pub mod define;
pub mod lambda;
pub mod quotation;

use super::error::Error;
use super::syntax::environment::{Denotation, Special, SyntaxEnvironment};
use super::Result;
use crate::compiler::core_compiler::CoreCompiler;
use crate::compiler::frontend::parser::core_parser::CoreParser;
use crate::compiler::frontend::reader::datum::Datum;
use crate::compiler::frontend::syntax;
use crate::compiler::frontend::syntax::symbol::Symbol;
use crate::compiler::source::{HasSourceLocation, Location};

#[derive(Debug)]
pub struct Expander {
    compiler: CoreCompiler,
    parser: CoreParser,
    expansion_env: SyntaxEnvironment,
}

impl Expander {
    pub fn new() -> Self {
        Self {
            compiler: CoreCompiler::new(),
            parser: CoreParser::new(),
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
                log::trace!("denotation of {:?} is {:?}", datum, denotation);

                match denotation {
                    Denotation::Special(special) => match special {
                        Special::Define => self.expand_define(&datum, &operator, &operands),
                        Special::Lambda => self.expand_lambda(&datum, &operator, &operands),
                        Special::DefineSyntax => todo!(),
                        Special::LetSyntax => todo!(),
                        Special::LetrecSyntax => todo!(),
                        Special::Unquote => Err(Error::expansion_error(
                            "unexpected unquote outside of quasi-quote",
                            &datum,
                        )),
                        Special::UnquoteSplicing => Err(Error::expansion_error(
                            "unexpected unquote-splicing outside of quasi-quote",
                            &datum,
                        )),
                        Special::QuasiQuote => {
                            self.expand_quasi_quotation(&datum, &operator, &operands)
                        }
                        Special::Quote => Ok(datum.clone()),
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
            None => {
                log::trace!("nothing to expand. Returning datum as is.");
                Ok(datum.clone())
            }
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

        Ok(Datum::list(new_ls.into_iter(), loc))
    }

    fn expand_macro(
        &mut self,
        _datum: &Datum,
        _transformer: &syntax::Transformer,
    ) -> Result<Datum> {
        todo!()
    }

    fn build_apply(&mut self, op: Symbol, args: Vec<Datum>, loc: Location) -> Datum {
        let mut inner = vec![Datum::symbol(op, loc.clone())];
        inner.extend(args);
        Datum::list(inner, loc)
    }

    fn empty_list(&mut self, loc: Location) -> Datum {
        self.quote_datum(Datum::list(Vec::<Datum>::new(), loc))
    }

    fn empty_vector(&mut self, loc: Location) -> Datum {
        self.quote_datum(Datum::vector(Vec::<Datum>::new(), loc))
    }

    fn quote_datum(&mut self, datum: Datum) -> Datum {
        let loc = datum.source_location().clone();
        Datum::list(
            vec![
                Datum::Symbol(Symbol::unforgeable("quote"), loc.clone()),
                datum,
            ],
            loc.clone(),
        )
    }

    fn build_lambda(&self, def_formals: &Datum, body: &[Datum], loc: Location) -> Datum {
        let mut lambda = vec![
            Datum::Symbol(Symbol::unforgeable("lambda"), loc.clone()),
            def_formals.clone(),
        ];

        lambda.extend_from_slice(body);
        Datum::list(lambda, loc.clone())
    }

    pub fn denotation_of(&mut self, datum: &Datum) -> Result<Denotation> {
        if let Datum::Symbol(sym, _) = datum {
            Ok(self.expansion_env.get(&sym.clone().into()))
        } else {
            Err(Error::bug("unexpected symbol to determine denotation"))
        }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::compiler::frontend::reader::tests::*;
    use std::fmt::Debug;

    use super::*;

    #[test]
    fn test_expand_atoms() -> Result<()> {
        assert_expands_equal("#t", "#t", true)?;
        assert_expands_equal("#\\a", "#\\a", true)?;
        Ok(())
    }

    #[test]
    fn test_expand_quote() -> Result<()> {
        assert_expands_equal("'3", "'3", true)?;
        assert_expands_equal("'(1 2 3)", "'(1 2 3)", true)?;
        Ok(())
    }

    pub fn assert_expands_equal(lhs: &str, rhs: &str, pedantic: bool) -> Result<()> {
        let mut exp = Expander::new();
        let actual_datum = parse_datum(lhs);
        let expected_datum = parse_datum(rhs);
        let expanded = exp.expand(&actual_datum)?;

        assert_struct_eq(&expanded, &expected_datum, pedantic);
        Ok(())
    }

    pub fn expand_form(form: &str) -> Result<Datum> {
        let mut exp = Expander::new();
        exp.expand(&parse_datum(form))
    }

    pub fn assert_struct_eq(lhs: &Datum, rhs: &Datum, pedantic: bool) {
        if pedantic {
            assert_eq!(
                lhs.source_location(),
                rhs.source_location(),
                "locations differ"
            );
        }

        match (lhs, rhs) {
            (Datum::List(inner_lhs, _), Datum::List(inner_rhs, _)) => {
                assert_vec_eq(&inner_lhs, &inner_rhs, |l, r| {
                    assert_struct_eq(l, r, pedantic)
                })
            }
            (
                Datum::ImproperList(head_lhs, tail_lhs, _),
                Datum::ImproperList(head_rhs, tail_rhs, _),
            ) => {
                assert_vec_eq(&head_lhs, &head_rhs, |l, r| {
                    assert_struct_eq(l, r, pedantic)
                });
                assert_struct_eq(&tail_lhs, &tail_rhs, pedantic);
            }
            (Datum::Vector(inner_lhs, _), Datum::Vector(inner_rhs, _)) => {
                assert_vec_eq(&inner_lhs, &inner_rhs, |l, r| {
                    assert_struct_eq(l, r, pedantic)
                })
            }
            (Datum::Symbol(inner_lhs, _), Datum::Symbol(inner_rhs, _)) if !pedantic => {
                assert_eq!(inner_lhs.as_str(), inner_rhs.as_str())
            }
            (datum_lhs, datum_rhs) if !pedantic => {
                assert_eq_ignore_location(datum_lhs, datum_rhs);
            }
            (datum_lhs, datum_rhs) => {
                assert_eq!(datum_lhs, datum_rhs)
            }
        }
    }

    fn assert_eq_ignore_location(lhs: &Datum, rhs: &Datum) {
        match (lhs, rhs) {
            (Datum::Bool(l, _), Datum::Bool(r, _)) => assert_eq!(l, r),
            (Datum::Symbol(l, _), Datum::Symbol(r, _)) => assert_eq!(l, r),
            (Datum::String(l, _), Datum::String(r, _)) => assert_eq!(l, r),
            (Datum::Char(l, _), Datum::Char(r, _)) => assert_eq!(l, r),
            (Datum::Number(l, _), Datum::Number(r, _)) => assert_eq!(l, r),
            (Datum::ByteVector(l, _), Datum::ByteVector(r, _)) => assert_eq!(l, r),
            (l, r) => assert_struct_eq(l, r, false),
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
