use crate::vm::environment;
use crate::vm::value;
use crate::vm::value::numeric;
use crate::vm::value::procedure;
use crate::vm::value::procedure::Arity;
use crate::vm::value::symbol;
use crate::vm::value::Value;

pub fn interactive_environment() -> environment::Environment {
    let mut env = environment::Environment::empty();

    env.set(
        &symbol::Symbol::intern("+"),
        value::foreign_lambda(Arity::Fixed(2), fx_plus),
    );
    env
}

pub fn fx_plus(args: &procedure::ForeignLambdaArgs) -> procedure::LambdaResult {
    match args[..] {
        [Value::Number(numeric::Number::Fixnum(lhs)), Value::Number(numeric::Number::Fixnum(rhs))] => {
            Ok(value::fixnum(lhs + rhs))
        }
        _ => Err(procedure::ApplicationError::Exception),
    }
}
