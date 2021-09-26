use super::{Error, Expander, Result};
use crate::compiler::frontend::reader::datum::Datum;
use crate::compiler::frontend::syntax::environment::Denotation;
use crate::compiler::source::HasSourceLocation;

impl Expander {
    // expand a lambda expression
    pub fn expand_lambda(
        &mut self,
        datum: &Datum,
        _operator: &Datum,
        operands: &[Datum],
    ) -> Result<Datum> {
        match operands {
            [formals, body @ ..] => {
                match self.parser.parse_formals(&formals) {
                    Ok(parsed_formals) => {
                        self.push_scope();
                        for identifier in parsed_formals.identifiers() {
                            let sym = identifier.symbol().clone();
                            self.extend_scope(sym.clone(), Denotation::Id);
                        }
                        let expanded_body = self.expand_all(body)?;
                        self.pop_scope();

                        Ok(self.build_lambda(
                            &formals,
                            &expanded_body,
                            datum.source_location().clone(),
                        ))
                    }
                    Err(_) => Err(Error::expansion_error(
                        "Malformed formals of lambda",
                        &datum,
                    )),
                }
            }
            // ignore invalid form?
            _ => Err(Error::expansion_error(
                "Malformed lambda expression",
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
    fn test_expand_lambda() -> Result<()> {
        assert_expands_equal("(lambda all #t)", "(lambda all #t)", false)?;

        Ok(())
    }
}
