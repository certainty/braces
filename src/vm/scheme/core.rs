use super::ffi::*;
use crate::vm::value::procedure::foreign;
use crate::vm::value::Value;
use crate::vm::value::{equality::SchemeEqual, procedure::Arity};
use crate::vm::VM;

macro_rules! register_core {
    ($vm:expr, $name:literal, $func:expr, $arity:expr) => {
        $vm.register_foreign(foreign::Procedure::new($name, $func, $arity))
            .unwrap()
    };
}

pub fn register(vm: &mut VM) {
    register_core!(vm, "eqv?", eqv, Arity::Exactly(2));
    register_core!(vm, "eq?", eq, Arity::Exactly(2));
    register_core!(vm, "equal?", equal, Arity::Exactly(2));

    register_core!(vm, "char?", char_p, Arity::Exactly(1));
    register_core!(vm, "symbol?", symbol_p, Arity::Exactly(1));
    register_core!(vm, "list?", string_p, Arity::Exactly(1));
    register_core!(vm, "procedure?", procedure_p, Arity::Exactly(1));
    register_core!(vm, "null?", null_p, Arity::Exactly(1));

    register_core!(vm, "not", bool_not, Arity::Exactly(1));

    register_core!(vm, "inspect", inspect, Arity::Exactly(1));
}

//  R7RS 6.1
//  (eqv? obj1 obj2
pub fn eqv(args: Vec<Value>) -> FunctionResult<Value> {
    match binary_procedure(&args)? {
        (lhs, rhs) => Ok(Value::Bool(lhs.is_eqv(rhs))),
    }
}

pub fn eq(args: Vec<Value>) -> FunctionResult<Value> {
    match binary_procedure(&args)? {
        (lhs, rhs) => Ok(Value::Bool(lhs.is_eq(rhs))),
    }
}

pub fn equal(args: Vec<Value>) -> FunctionResult<Value> {
    match binary_procedure(&args)? {
        (lhs, rhs) => Ok(Value::Bool(lhs.is_equal(rhs))),
    }
}

// predicates
pub fn string_p(args: Vec<Value>) -> FunctionResult<Value> {
    match unary_procedure(&args)? {
        Value::InternedString(_) => Ok(Value::Bool(true)),
        Value::UninternedString(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn symbol_p(args: Vec<Value>) -> FunctionResult<Value> {
    match unary_procedure(&args)? {
        Value::Symbol(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn char_p(args: Vec<Value>) -> FunctionResult<Value> {
    match unary_procedure(&args)? {
        Value::Char(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn list_p(args: Vec<Value>) -> FunctionResult<Value> {
    match unary_procedure(&args)? {
        Value::ProperList(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn null_p(args: Vec<Value>) -> FunctionResult<Value> {
    match unary_procedure(&args)? {
        Value::ProperList(ls) => Ok(Value::Bool(ls.is_null())),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn procedure_p(args: Vec<Value>) -> FunctionResult<Value> {
    match unary_procedure(&args)? {
        Value::Procedure(_) => Ok(Value::Bool(true)),
        Value::Closure(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn bool_not(args: Vec<Value>) -> FunctionResult<Value> {
    match unary_procedure(&args)? {
        Value::Bool(v) => Ok(Value::Bool(!v)),
        _ => Ok(Value::Bool(false)),
    }
}

pub fn inspect(args: Vec<Value>) -> FunctionResult<Value> {
    println!("Debug: {:?}", args.first().unwrap());
    Ok(Value::Unspecified)
}
