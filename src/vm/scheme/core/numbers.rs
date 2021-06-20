use crate::vm::scheme::ffi::*;
use crate::vm::value::number::SchemeNumber;
use crate::vm::value::procedure::foreign;
use crate::vm::value::procedure::Arity;
use crate::vm::value::{error, number, Value};
use crate::vm::VM;
use std::ops::{Add, Div, Mul, Sub};

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
    super::register_procedure!(vm, "number?", number_p, Arity::Exactly(1));
    super::register_procedure!(vm, "complex?", complex_p, Arity::Exactly(1));
    super::register_procedure!(vm, "real?", real_p, Arity::Exactly(1));
    super::register_procedure!(vm, "rational?", rational_p, Arity::Exactly(1));
    super::register_procedure!(vm, "integer?", integer_p, Arity::Exactly(1));
    super::register_procedure!(vm, "nan?", nan_p, Arity::Exactly(1));
    super::register_procedure!(vm, "finite?", finite_p, Arity::Exactly(1));
    super::register_procedure!(vm, "infinite?", inf_p, Arity::Exactly(1));
    super::register_procedure!(vm, "exact?", exact_p, Arity::Exactly(1));
    super::register_procedure!(vm, "inexact?", inexact_p, Arity::Exactly(1));
    super::register_procedure!(vm, "+", add, Arity::Many);
    super::register_procedure!(vm, "-", sub, Arity::AtLeast(1));
    super::register_procedure!(vm, "*", mul, Arity::Many);
    super::register_procedure!(vm, "/", div, Arity::AtLeast(1));
    super::register_procedure!(vm, "<", lt, Arity::Many);
    super::register_procedure!(vm, "<=", lt_eq, Arity::Many);
    super::register_procedure!(vm, ">", gt, Arity::Many);
    super::register_procedure!(vm, ">=", gt_eq, Arity::Many);
    super::register_procedure!(vm, "=", num_eq, Arity::Many);
}

// R7RS 6.2.6 Numerical operations
fn number_p(args: Vec<Value>) -> FunctionResult<Value> {
    match unary_procedure(&args)? {
        Value::Number(_) => Ok(Value::Bool(true)),
        _ => Ok(Value::Bool(false)),
    }
}

fn as_number(v: &Value) -> FunctionResult<&number::Number> {
    match v {
        Value::Number(n) => Ok(n),
        v => return Err(error::argument_error(v.clone(), "is not a number")),
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

macro_rules! define_fold {
    ($func:ident, $identity:expr, $op:ident) => {
        pub fn $func(args: Vec<Value>) -> FunctionResult<Value> {
            let mut result = number::Number::fixnum($identity);

            for n in args {
                match n {
                    Value::Number(n) => result = result.$op(n)?,
                    v => return Err(error::argument_error(v.clone(), "is not a number")),
                }
            }

            Ok(Value::Number(result))
        }
    };
}

define_fold!(add, 0, add);
define_fold!(mul, 1, mul);

macro_rules! define_reduction {
    ($func:ident, $op:ident) => {
        pub fn $func(args: Vec<Value>) -> FunctionResult<Value> {
            if let Value::Number(mut result) = args[0].clone() {
                for n in args[1..].iter().cloned() {
                    match n {
                        Value::Number(n) => result = result.$op(n)?,
                        v => return Err(error::argument_error(v.clone(), "is not a number")),
                    }
                }

                Ok(Value::Number(result))
            } else {
                return Err(error::argument_error(args[0].clone(), "is not a number"));
            }
        }
    };
}

define_reduction!(sub, sub);
define_reduction!(div, div);

macro_rules! define_ordering {
    ($func:ident, $op:tt) => {
        fn $func(args: Vec<Value>) -> FunctionResult<Value> {
            let mut result = true;

            for n in 0..args.len() {
                if n == 0 {
                    result = true;
                } else {
                    result = as_number(&args[n - 1])? $op as_number(&args[n])?;
                }
            }

            Ok(Value::Bool(result))
        }
    };
}

define_ordering!(lt, <);
define_ordering!(lt_eq, <=);
define_ordering!(gt, >);
define_ordering!(gt_eq, >=);
define_ordering!(num_eq, ==);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::VM;

    #[test]
    fn arith_addition() {
        let result = run_as_number("(+)").unwrap();
        assert_eq!(result, number::Number::fixnum(0));

        let result = run_as_number("(+ 1)").unwrap();
        assert_eq!(result, number::Number::fixnum(1));

        let result = run_as_number("(+ 1 10)").unwrap();
        assert_eq!(result, number::Number::fixnum(11));

        let result = run_as_number("(+ 1 10 5)").unwrap();
        assert_eq!(result, number::Number::fixnum(16));
    }

    #[test]
    fn arith_subtraction() {
        let result = run_as_number("(- 2 2)").unwrap();
        assert_eq!(result, number::Number::fixnum(0));

        let result = run_as_number("(- 2 4)").unwrap();
        assert_eq!(result, number::Number::fixnum(-2));
    }

    #[test]
    fn arith_multiplication() {
        let result = run_as_number("(*)").unwrap();
        assert_eq!(result, number::Number::fixnum(1));

        let result = run_as_number("(* 2)").unwrap();
        assert_eq!(result, number::Number::fixnum(2));

        let result = run_as_number("(* 2 12)").unwrap();
        assert_eq!(result, number::Number::fixnum(24));

        let result = run_as_number("(* 2 12 10)").unwrap();
        assert_eq!(result, number::Number::fixnum(240));
    }

    #[test]
    fn arith_division() {
        let result = run_as_number("(/ 4 2)").unwrap();
        assert_eq!(result, number::Number::fixnum(2));
    }

    #[test]
    fn test_div_for_fixnum() {
        let result = run_as_number("(/ 4 2)").unwrap();
        assert_eq!(result, number::Number::fixnum(2));

        let result = run_as_number("(/ 4 3)").unwrap();
        assert_eq!(result, number::Number::fraction(4, 3));
    }

    #[test]
    fn mix_arithmetic() {
        let result = run_as_number("(/ 4 2.0)").unwrap();
        assert_eq!(result, number::Number::flonum(2.0));

        let result = run_as_number("(/ 4.0 2)").unwrap();
        assert_eq!(result, number::Number::flonum(2.0));

        let result = run_as_number("(+ 4 2.0)").unwrap();
        assert_eq!(result, number::Number::flonum(6.0));

        let result = run_as_number("(+ 4.0 2)").unwrap();
        assert_eq!(result, number::Number::flonum(6.0));

        let result = run_as_number("(- 4 2.0)").unwrap();
        assert_eq!(result, number::Number::flonum(2.0));

        let result = run_as_number("(- 4.0 2)").unwrap();
        assert_eq!(result, number::Number::flonum(2.0));
    }

    #[test]
    fn test_lt() {
        assert_matches!(run_string("(<)"), Value::Bool(true));
        assert_matches!(run_string("(< 2 3)"), Value::Bool(true));
        assert_matches!(run_string("(< 2 3 4)"), Value::Bool(true));
        assert_matches!(run_string("(< 3 2)"), Value::Bool(false));
        assert_matches!(run_string("(< 3 3)"), Value::Bool(false));
        assert_matches!(run_string("(< 3 5 2)"), Value::Bool(false));
    }

    fn run_as_number(inp: &str) -> Option<number::Number> {
        if let Value::Number(n) = run_string(inp) {
            Some(n)
        } else {
            None
        }
    }

    fn run_string(inp: &str) -> Value {
        let mut vm = VM::default();
        vm.run_string(inp, "test numbers").unwrap()
    }
}
