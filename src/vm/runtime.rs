use crate::vm::environment;
use crate::vm::value;
use crate::vm::value::numeric;
use crate::vm::value::procedure;
use crate::vm::value::procedure::Arity;
use crate::vm::value::symbol::SymbolTable;
use crate::vm::value::Value;

pub fn default_environment(symbols: &mut SymbolTable) -> environment::Environment {
    let mut env = environment::Environment::empty();

    env.set(
        &symbols.interned("+".into()),
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
