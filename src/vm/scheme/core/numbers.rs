use crate::vm::scheme::ffi::*;
use crate::vm::value::number::SchemeNumber;
use crate::vm::value::procedure::foreign;
use crate::vm::value::{equality::SchemeEqual, procedure::Arity};
use crate::vm::value::{error, number, Value};
use crate::vm::VM;

macro_rules! define_predicate {
    ($name:ident, $number:ident, $pred:expr) => {
        fn $name(args: Vec<Value>) -> FunctionResult<Value> {
            match unary_procedure(&args)? {
                Value::Number($number) => Ok(Value::Bool($pred)),
                _ => Ok(Value::Bool(false)),
            }
        }
    };
}

pub fn register(vm: &mut VM) {
    super::register_core!(vm, "number?", number_p, Arity::Exactly(1));
    super::register_core!(vm, "complex?", complex_p, Arity::Exactly(1));
    super::register_core!(vm, "real?", real_p, Arity::Exactly(1));
    super::register_core!(vm, "rational?", rational_p, Arity::Exactly(1));
    super::register_core!(vm, "integer?", integer_p, Arity::Exactly(1));
    super::register_core!(vm, "nan?", nan_p, Arity::Exactly(1));
    super::register_core!(vm, "finite?", finite_p, Arity::Exactly(1));
    super::register_core!(vm, "infinite?", inf_p, Arity::Exactly(1));
    super::register_core!(vm, "exact?", exact_p, Arity::Exactly(1));
    super::register_core!(vm, "inexact?", exact_p, Arity::Exactly(1));
}

// R7RS 6.2.6 Numerical operations

fn number_p(args: Vec<Value>) -> FunctionResult<Value> {
    match unary_procedure(&args)? {
        Value::Number(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

define_predicate!(complex_p, n, n.is_complex());
define_predicate!(real_p, n, n.is_real());
define_predicate!(rational_p, n, n.is_rational());
define_predicate!(integer_p, n, n.is_integer());
define_predicate!(nan_p, n, n.is_nan());
define_predicate!(inf_p, n, n.is_infinite());
define_predicate!(finite_p, n, n.is_finite());
define_predicate!(exact_p, n, n.is_exact());
define_predicate!(inexact_p, n, n.is_inexact());

/*

pub fn fx_plus(args: Vec<Value>) -> FunctionResult<Value> {
    match binary_procedure(&args)? {
        (
            Value::Number(number::Number::Real(number::RealNumber::Fixnum(lhs))),
            Value::Number(number::Number::Real(number::RealNumber::Fixnum(rhs))),
        ) => Ok(Value::Number(number::Number::Real(
            number::RealNumber::Fixnum(lhs + rhs),
        ))),
        _ => Err(error::argument_error("Expected exactly two fixnums")),
    }
}

pub fn fx_minus(args: Vec<Value>) -> FunctionResult<Value> {
    match binary_procedure(&args)? {
        (
            Value::Number(number::Number::Real(number::RealNumber::Fixnum(lhs))),
            Value::Number(number::Number::Real(number::RealNumber::Fixnum(rhs))),
        ) => Ok(Value::Number(number::Number::Real(
            number::RealNumber::Fixnum(lhs - rhs),
        ))),
        _ => Err(error::argument_error("Expected exactly two fixnums")),
    }
}

pub fn fx_lt(args: Vec<Value>) -> FunctionResult<Value> {
    println!("args: {:?}", args);
    match binary_procedure(&args)? {
        (
            Value::Number(number::Number::Real(number::RealNumber::Fixnum(lhs))),
            Value::Number(number::Number::Real(number::RealNumber::Fixnum(rhs))),
        ) => Ok(Value::Bool(lhs < rhs)),
        _ => Err(error::argument_error("Expected exactly two fixnums")),
    }
}

pub fn fx_lt_eq(args: Vec<Value>) -> FunctionResult<Value> {
    match binary_procedure(&args)? {
        (
            Value::Number(number::Number::Real(number::RealNumber::Fixnum(lhs))),
            Value::Number(number::Number::Real(number::RealNumber::Fixnum(rhs))),
        ) => Ok(Value::Bool(lhs <= rhs)),
        _ => Err(error::argument_error("Expected exactly two fixnums")),
    }
}

pub fn fx_gt(args: Vec<Value>) -> FunctionResult<Value> {
    match binary_procedure(&args)? {
        (
            Value::Number(number::Number::Real(number::RealNumber::Fixnum(lhs))),
            Value::Number(number::Number::Real(number::RealNumber::Fixnum(rhs))),
        ) => Ok(Value::Bool(lhs > rhs)),
        _ => Err(error::argument_error("Expected exactly two fixnums")),
    }
}

pub fn fx_gt_eq(args: Vec<Value>) -> FunctionResult<Value> {
    match binary_procedure(&args)? {
        (
            Value::Number(number::Number::Real(number::RealNumber::Fixnum(lhs))),
            Value::Number(number::Number::Real(number::RealNumber::Fixnum(rhs))),
        ) => Ok(Value::Bool(lhs >= rhs)),
        _ => Err(error::argument_error("Expected exactly two fixnums")),
    }
}
*/
