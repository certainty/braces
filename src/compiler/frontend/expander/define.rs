use super::{Error, Expander, Result};
use crate::compiler::frontend::reader::datum::Datum;
use crate::compiler::source::HasSourceLocation;

impl Expander {
    pub fn expand_define(
        &mut self,
        datum: &Datum,
        operator: &Datum,
        operands: &[Datum],
    ) -> Result<Datum> {
        match operands {
            //(define (...) <body>)
            [definition, exprs @ ..] if definition.is_proper_list() => {
                match definition.list_slice() {
                    //(define (<ID> <def-formals> <body>)
                    Some([identifier, def_formals @ ..]) => {
                        let lambda = self.build_lambda(
                            &Datum::list(def_formals.to_vec(), datum.source_location().clone()),
                            exprs,
                            datum.source_location().clone(),
                        );

                        Ok(Datum::list(
                            vec![operator.clone(), identifier.clone(), lambda],
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
            [identifier, expr] => Ok(Datum::list(
                vec![operator.clone(), identifier.clone(), expr.clone()],
                datum.source_location().clone(),
            )),
            _ => Err(Error::expansion_error(
                "Expected definition or procedure definition",
                &datum,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::*;
    use super::super::Result;

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
}
