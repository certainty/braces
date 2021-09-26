pub mod define;
pub mod lambda;
pub mod letexp;
pub mod quotation;

use super::error::Error;
use super::syntax::environment::{Denotation, Special, SyntaxEnvironment};
use super::Result;
use crate::compiler::core_compiler::CoreCompiler;
use crate::compiler::frontend::parser::core_parser::CoreParser;
use crate::compiler::frontend::parser::Expression;
use crate::compiler::frontend::reader::datum::Datum;
use crate::compiler::frontend::syntax;
use crate::compiler::frontend::syntax::symbol::Symbol;
use crate::compiler::source::{HasSourceLocation, Location};
use crate::vm::scheme::ffi::{binary_procedure, unary_procedure};
use crate::vm::value::access::Access;
use crate::vm::value::procedure::{foreign, Arity, Procedure};
use crate::vm::value::Value;
use crate::vm::{value, VM};

#[derive(Debug)]
pub struct Expander {
    vm: VM,
    compiler: CoreCompiler,
    parser: CoreParser,
    expansion_env: SyntaxEnvironment,
}

impl Expander {
    pub fn new() -> Self {
        let mut expander = Self {
            vm: VM::for_expansion(),
            compiler: CoreCompiler::new(),
            parser: CoreParser::new(),
            expansion_env: SyntaxEnvironment::basic(),
        };

        letexp::register_macros(&mut expander);
        expander
    }

    pub fn expand(&mut self, datum: &Datum) -> Result<Option<Datum>> {
        self.expand_macros(&datum)
    }

    pub fn expand_macros(&mut self, datum: &Datum) -> Result<Option<Datum>> {
        match datum.list_slice() {
            Some([operator, operands @ ..]) if operator.is_symbol() => {
                let denotation = self.denotation_of(operator)?;
                //println!("Denotation of {} is {:?}", operator, denotation);
                log::trace!("denotation of {:?} is {:?}", datum, denotation);
                match denotation {
                    Denotation::Special(special) => match special {
                        Special::Define => {
                            Ok(Some(self.expand_define(&datum, &operator, &operands)?))
                        }
                        Special::Lambda => {
                            Ok(Some(self.expand_lambda(&datum, &operator, &operands)?))
                        }
                        Special::DefineSyntax if operands.len() == 2 => {
                            self.define_syntax(&datum, &operands[0], &operands[1])?;
                            Ok(None)
                        }
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
                        Special::QuasiQuote => Ok(Some(
                            self.expand_quasi_quotation(&datum, &operator, &operands)?,
                        )),
                        Special::Quote => Ok(Some(datum.clone())),
                        _ => Ok(Some(self.expand_apply(
                            operator,
                            operands,
                            datum.source_location().clone(),
                        )?)),
                    },
                    Denotation::Macro(transformer) => {
                        let expanded = self.expand_macro(&datum, &transformer)?;
                        // recursively expand
                        self.expand_macros(&expanded)
                    }
                    _ => Ok(Some(self.expand_apply(
                        operator,
                        operands,
                        datum.source_location().clone(),
                    )?)),
                }
            }
            Some([operator, operands @ ..]) => Ok(Some(self.expand_apply(
                operator,
                operands,
                datum.source_location().clone(),
            )?)),
            Some(_) => Err(Error::expansion_error("Unexpected unquoted list", &datum)),
            None => {
                log::trace!("nothing to expand. Returning datum as is.");
                Ok(Some(datum.clone()))
            }
        }
    }

    fn expand_all(&mut self, all: &[Datum]) -> Result<Vec<Datum>> {
        let data: Result<Vec<Option<_>>> = all.iter().map(|d| self.expand_macros(d)).collect();
        Ok(data?.into_iter().flatten().collect())
    }

    fn expand_apply(
        &mut self,
        operator: &Datum,
        operands: &[Datum],
        loc: Location,
    ) -> Result<Datum> {
        let mut new_ls = vec![self.expand(operator)?.unwrap()];
        new_ls.extend(self.expand_all(operands)?);

        Ok(Datum::list(new_ls.into_iter(), loc))
    }

    fn expand_macro(&mut self, datum: &Datum, transformer: &syntax::Transformer) -> Result<Datum> {
        match transformer {
            syntax::Transformer::ExplicitRenaming(expander) => {
                let renamer = self.create_renamer();
                let cmp = self.create_comparator();
                match self.vm.interpret_expander(
                    expander.clone(),
                    datum,
                    &[Value::Procedure(renamer), Value::Procedure(cmp)],
                    self.expansion_env.clone(),
                    datum.source_location().clone(),
                ) {
                    Ok(expanded) => Ok(expanded),
                    Err(e) => Err(Error::expansion_error(
                        format!("Invocation of macro expander failed: {:?}", e),
                        &datum,
                    )),
                }
            }
        }
    }

    fn define_syntax(&mut self, datum: &Datum, id: &Datum, operand: &Datum) -> Result<()> {
        match id {
            Datum::Symbol(sym, _) => match operand.list_slice() {
                Some([Datum::Symbol(transformer_type, _), procedure]) => {
                    match transformer_type.as_str() {
                        "er-macro-transformer" => {
                            let transformer = syntax::Transformer::ExplicitRenaming(self.compile_lambda(&procedure)?);
                            self.expansion_env.extend(sym.clone(), Denotation::Macro(transformer));
                            Ok(())
                        }
                        _ => Err(Error::expansion_error("Invalid macro transformer type. Must be one of (er-macro-transformer, lowlevel-macro-transformer)", datum))
                    }
                }
                _ => Err(Error::expansion_error(
                    "Transformer type must be a symbol",
                    &datum,
                )),
            },
            _ => Err(Error::expansion_error(
                "Expected (define-syntax <id> (<transformer-type> <transformer-procedure>))",
                &datum,
            )),
        }
    }

    fn compile_lambda(&mut self, lambda: &Datum) -> Result<value::procedure::Procedure> {
        let code = self.expand(&lambda)?.expect("empty definition");
        let ast = self.parser.parse(&code)?;

        match ast {
            Expression::Lambda(lambda_expr) => {
                let compiled = self.compiler.compile_lambda(&lambda_expr);
                match compiled {
                    Ok(proc) => Ok(proc),
                    _ => Err(Error::expansion_error(
                        "couldn't compile transformer",
                        &lambda,
                    )),
                }
            }
            _ => Err(Error::expansion_error(
                "expected code to parse as lambda expression",
                &lambda,
            )),
        }
    }

    fn create_renamer(&mut self) -> Procedure {
        Procedure::foreign(foreign::Procedure::new(
            "rename",
            |_ctx, values| unary_procedure(&values).map(|v| Access::ByVal(v.clone())),
            Arity::Exactly(1),
        ))
    }

    fn create_comparator(&mut self) -> Procedure {
        Procedure::foreign(foreign::Procedure::new(
            "compare",
            |_ctx, values| {
                binary_procedure(&values).map(|(l, r)| Access::from(Value::Bool(l == r)))
            },
            Arity::Exactly(2),
        ))
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

    #[test]
    fn expand_lowlevel_macro() -> Result<()> {
        assert_expands_all_equal(
            r#"
            (define-syntax my-cons (er-macro-transformer (lambda (form rename compare) `(cons 1 2)))) 
            (my-cons)
            "#,
            "(cons 1 2)",
            false,
        )?;
        Ok(())
    }

    pub fn assert_expands_all_equal(lhs: &str, rhs: &str, pedantic: bool) -> Result<()> {
        let mut exp = Expander::new();
        let actual_datum = parse_datum_all(lhs);
        let expected_datum = parse_datum_all(rhs);
        let expanded_datum = exp.expand_all(&actual_datum)?;

        //println!("expected: {}", expected_datum);
        //println!("expanded: {}", expanded_datum);
        assert_vec_eq(&expanded_datum, &expected_datum, |l, r| {
            assert_struct_eq(l, r, pedantic)
        });
        Ok(())
    }

    pub fn assert_expands_equal(lhs: &str, rhs: &str, pedantic: bool) -> Result<()> {
        let mut exp = Expander::new();
        let actual_datum = parse_datum(lhs);
        let expected_datum = parse_datum(rhs);
        let expanded_datum = exp.expand(&actual_datum)?.unwrap();

        println!("expected: {}", expected_datum);
        println!("expanded: {}", expanded_datum);

        assert_struct_eq(&expanded_datum, &expected_datum, pedantic);
        Ok(())
    }

    pub fn expand_form(form: &str) -> Result<Datum> {
        let mut exp = Expander::new();
        exp.expand(&parse_datum(form)).map(|e| e.unwrap())
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
                    assert_struct_eq(l, r, pedantic);
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
            (Datum::Bool(l, _), Datum::Bool(r, _)) => assert_eq!(l, r),
            (Datum::Symbol(l, _), Datum::Symbol(r, _)) => assert_eq!(l, r),
            (Datum::String(l, _), Datum::String(r, _)) => assert_eq!(l, r),
            (Datum::Char(l, _), Datum::Char(r, _)) => assert_eq!(l, r),
            (Datum::Number(l, _), Datum::Number(r, _)) => assert_eq!(l, r),
            (Datum::ByteVector(l, _), Datum::ByteVector(r, _)) => assert_eq!(l, r),
            _ => assert!(false, "Expected datum to be of the same kind"),
        }
    }

    fn assert_vec_eq<F>(lhs: &Vec<Datum>, rhs: &Vec<Datum>, assertion: F)
    where
        F: Copy + FnOnce(&Datum, &Datum),
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
