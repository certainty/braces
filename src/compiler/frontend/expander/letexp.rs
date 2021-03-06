use super::Expander;
use crate::compiler::frontend::reader::datum::Datum;
use crate::compiler::frontend::syntax::environment::Denotation;
use crate::compiler::frontend::syntax::symbol::Symbol;
use crate::compiler::frontend::syntax::Transformer;
use crate::compiler::source::{HasSourceLocation, Location};
use crate::vm::instance::Instance;
use crate::vm::scheme::ffi::unary_procedure;
use crate::vm::scheme::ffi::FunctionResult;
use crate::vm::value::access::Access;
use crate::vm::value::error;
use crate::vm::value::procedure::{foreign, Arity, Procedure};
use crate::vm::value::Value;

pub fn register_macros(expander: &mut Expander) {
    expander.extend_scope(
        Symbol::forged("let"),
        Denotation::Macro(Transformer::LowLevel(self::make_let_expander())),
    );
}

fn make_let_expander() -> Procedure {
    Procedure::foreign(foreign::Procedure::new(
        "expand_let",
        self::expand_let,
        Arity::Exactly(1),
    ))
}

fn expand_let(_vm: &mut Instance, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    unary_procedure(&args).and_then({
        |form| {
            let datum = Datum::from_value(form, Location::for_syntax_transformer()).unwrap();
            match datum.list_slice() {
                // Named let
                // (let f ((v e) ...) b ...)
                Some([_let, f, bindings, _body @ ..])
                    if bindings.is_proper_list() && f.is_symbol() =>
                {
                    // named let not yet supported. We first need a working implementation of letrec
                    todo!()
                }
                // let
                // (let ((v e) ...) b ...)
                Some([_let, bindings, body @ ..]) if body.len() >= 1 => {
                    let (lambda, values) =
                        self::make_lambda(bindings, body, datum.source_location().clone())?;

                    let mut application = vec![lambda];
                    application.extend(values);

                    Ok(
                        Value::Syntax(Datum::list(application, datum.source_location().clone()))
                            .into(),
                    )
                }
                _ => Err(error::argument_error(
                    form.clone(),
                    "expansion of let failed. Incorrect form given",
                )),
            }
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
    fn test_expand_nested_let() -> Result<()> {
        assert_expands_equal(
            "(let ((x #t)) (let ((y x)) y))",
            "((lambda (x) ((lambda (y) y) x)) #t)",
            false,
        )?;
        Ok(())
    }

    #[test]
    fn test_expand_simple_let_empty_bindings() -> Result<()> {
        assert_expands_equal("(let () #t)", "((lambda () #t))", false)?;
        Ok(())
    }

    #[test]
    fn test_let_after_define() -> Result<()> {
        assert_expands_equal(
            "(define x (let ((f #t)) f))",
            "(define x ((lambda (f) f) #t))",
            false,
        )?;
        Ok(())
    }
}
