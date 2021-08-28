pub mod numbers;
use super::ffi::*;
use crate::vm::value::error;
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
use crate::vm::value::list::List;
pub(crate) use register_core;

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

    register_core!(vm, "cons", list_cons, Arity::Exactly(2));
    register_core!(vm, "append", list_append, Arity::Exactly(2));

    register_core!(vm, "vector-cons", vector_cons, Arity::Exactly(2));
    register_core!(vm, "vector-append", vector_append, Arity::Exactly(2));

    numbers::register(vm);
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

pub fn list_cons(args: Vec<Value>) -> FunctionResult<Value> {
    match binary_procedure(&args)? {
        (v, Value::ProperList(elts)) => Ok(Value::ProperList(elts.cons(v.clone()))),
        (lhs, rhs) => Ok(Value::ImproperList(
            List::Cons(vec![lhs.clone()].into()),
            Box::new(rhs.clone()),
        )),
    }
}

pub fn list_append(args: Vec<Value>) -> FunctionResult<Value> {
    match binary_procedure(&args)? {
        (Value::ProperList(lhs), Value::ProperList(rhs)) => Ok(Value::ProperList(lhs.append(rhs))),
        (Value::ProperList(lhs), Value::ImproperList(rhs_head, rhs_tail)) => {
            Ok(Value::ImproperList(lhs.append(rhs_head), rhs_tail.clone()))
        }
        (lhs, Value::ProperList(_)) => Err(error::argument_error(lhs.clone(), "Expected list")),
        (lhs, Value::ImproperList(rhs_head, rhs_tail)) => {
            Err(error::argument_error(lhs.clone(), "Expected list"))
        }
        (Value::ProperList(_), rhs) => Err(error::argument_error(rhs.clone(), "Expected list")),
        (lhs, _) => Err(error::argument_error(lhs.clone(), "Expected list")),
    }
}
pub fn vector_cons(args: Vec<Value>) -> FunctionResult<Value> {
    match binary_procedure(&args)? {
        (v, Value::Vector(elts)) => {
            let mut new_vec = elts.clone();
            new_vec.push(v.clone());
            Ok(Value::Vector(new_vec))
        }
        (_, other) => Err(error::argument_error(other.clone(), "Expected vector")),
    }
}

pub fn vector_append(args: Vec<Value>) -> FunctionResult<Value> {
    match binary_procedure(&args)? {
        (Value::Vector(lhs), Value::Vector(rhs)) => {
            let mut out = lhs.clone();
            out.extend(rhs.clone());
            Ok(Value::Vector(out))
        }
        (lhs, Value::Vector(_)) => Err(error::argument_error(lhs.clone(), "Expected vector")),
        (Value::Vector(_), rhs) => Err(error::argument_error(rhs.clone(), "Expected vector")),
        (lhs, _) => Err(error::argument_error(lhs.clone(), "Expected vector")),
    }
}
