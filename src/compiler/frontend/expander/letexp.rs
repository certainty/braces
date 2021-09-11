use super::Expander;
use crate::compiler::frontend::reader::datum::Datum;
use crate::compiler::frontend::syntax::environment::Denotation;
use crate::compiler::frontend::syntax::symbol::Symbol;
use crate::compiler::frontend::syntax::Transformer;
use crate::compiler::source::{HasSourceLocation, Location};
use crate::vm::scheme::ffi::{explicit_rename_transformer, FunctionResult};
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
            // Named let
            // (let f ((v e) ...) b ...)
            Some([_let, f, bindings, body @ ..]) if bindings.is_proper_list() && f.is_symbol() => {
                let (lambda, values) =
                    self::make_lambda(bindings, body, datum.source_location().clone())?;

                // (define f (lambda ...))
                let definition = Datum::list(
                    vec![
                        Datum::forged_symbol("define", datum.source_location().clone()),
                        f.clone(),
                        lambda,
                    ],
                    datum.source_location().clone(),
                );

                let mut application = vec![f.clone()];
                application.extend(values);

                //(begin (define f (lambda ...)) (f initial_arg))
                let begin = Datum::list(
                    vec![
                        Datum::forged_symbol("begin", datum.source_location().clone()),
                        definition,
                        Datum::list(application, datum.source_location().clone()),
                    ],
                    datum.source_location().clone(),
                );
                Ok(Value::syntax(begin))
            }
            // let
            // (let ((v e) ...) b ...)
            Some([_let, bindings, body @ ..]) => {
                let (lambda, values) =
                    self::make_lambda(bindings, body, datum.source_location().clone())?;

                let mut application = vec![lambda];
                application.extend(values);

                println!(
                    "Expanding to: {}",
                    Datum::list(application.clone(), datum.source_location().clone())
                );

                Ok(Value::syntax(Datum::list(
                    application,
                    datum.source_location().clone(),
                )))
            }

            _ => Err(error::argument_error(
                Value::Syntax(datum.clone()),
                "expected syntax object to be proper list",
            )),
        }
    })
}

pub fn make_lambda(
    bindings: &Datum,
    body: &[Datum],
    loc: Location,
) -> FunctionResult<(Datum, Vec<Datum>)> {
    match bindings.list_slice() {
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
                Datum::forged_symbol("lambda", loc.clone()),
                Datum::list(identifiers, loc.clone()),
            ];
            lambda.extend_from_slice(body);
            Ok((Datum::list(lambda, loc.clone()), values))
        }
        _ => Err(error::syntax_error("Invalid let form")),
    }
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

    #[test]
    fn test_expand_simple_let_empty_bindings() -> Result<()> {
        assert_expands_equal("(let () #t)", "((lambda () #t))", false)?;
        Ok(())
    }

    #[test]
    fn test_named_let() -> Result<()> {
        assert_expands_equal(
            "(let lp ((x 0)) (lp (+ x 1)))",
            "(begin (define lp (lambda (x) (lp (+ x 1)))) (lp 0))",
            false,
        )?;
        Ok(())
    }
}
