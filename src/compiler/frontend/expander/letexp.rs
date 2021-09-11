use super::{Error, Expander, Result};
use crate::compiler::frontend::reader::datum::Datum;
use crate::compiler::frontend::syntax::environment::Denotation;
use crate::compiler::frontend::syntax::symbol::Symbol;
use crate::compiler::frontend::syntax::Transformer;
use crate::compiler::source::HasSourceLocation;
use crate::vm::scheme::ffi::{explicit_rename_transformer, ternary_procedure, FunctionResult};
use crate::vm::value::error;
use crate::vm::value::procedure::{foreign, Arity, Procedure};
use crate::vm::value::Value;

pub fn register_macros(expander: &mut Expander) {
    expander.expansion_env.extend(
        Symbol::forged("let"),
        Denotation::Macro(Transformer::ExplicitRenaming(self::make_let_expander())),
    );
}

fn make_let_expander() -> Procedure {
    Procedure::foreign(foreign::Procedure::new(
        "expand_let",
        self::expand_let,
        Arity::Exactly(3),
    ))
}

fn expand_let(args: Vec<Value>) -> FunctionResult<Value> {
    explicit_rename_transformer(&args).and_then({
        |(datum, _rename, _compare)| match datum.list_slice() {
            // (let ((v e) ...) b ...)
            Some([_let, bindings, body @ ..]) => match bindings.list_slice() {
                Some([bindings @ ..]) => {
                    let mut identifiers = vec![];
                    let mut values = vec![];

                    for binding in bindings {
                        match binding.list_slice() {
                            Some([id, value]) => {
                                identifiers.push(id.clone());
                                values.push(value.clone());
                            }
                            _ => return Err(error::syntax_error("invalid binding form")),
                        }
                    }

                    // now we build the corresponding lambda expression
                    let mut lambda = vec![
                        Datum::forged_symbol("lambda", datum.source_location().clone()),
                        Datum::list(identifiers, datum.source_location().clone()),
                    ];
                    lambda.extend_from_slice(body);

                    let mut application =
                        vec![Datum::list(lambda, datum.source_location().clone())];
                    application.extend(values);

                    Ok(Value::syntax(Datum::list(
                        application,
                        datum.source_location().clone(),
                    )))
                }
                _ => Err(error::syntax_error("Invalid let form")),
            },
            _ => Err(error::argument_error(
                Value::Syntax(datum.clone()),
                "expected syntax object to be proper list",
            )),
        }
    })
}

#[cfg(test)]
mod tests {
    use super::super::tests::*;
    use super::super::Result;

    #[test]
    fn test_expand_simple_let() -> Result<()> {
        assert_expands_equal("(let ((x #t)) x)", "((lambda (x) x) #t)", false)?;
        Ok(())
    }
}
