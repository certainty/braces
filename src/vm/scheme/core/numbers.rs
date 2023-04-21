use crate::vm::scheme::ffi::*;
use crate::vm::value::access::Access;
use crate::vm::value::number::SchemeNumber;
use crate::vm::value::procedure::foreign;
use crate::vm::value::procedure::Arity;
use crate::vm::value::{error, number, Value};
use crate::vm::Instance;
use crate::vm::VM;
use std::ops::{Add, Div, Mul, Sub};

macro_rules! define_predicate {
    ($name:ident, $number:ident, $pred:expr) => {
        fn $name(_vm: &mut Instance, args: Vec<Value>) -> FunctionResult<Access<Value>> {
            match unary_procedure(&args)? {
                Value::Number($number) => Ok(Value::Bool($pred).into()),
                _ => Ok(Value::Bool(false).into()),
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
    super::register_core!(vm, "inexact?", inexact_p, Arity::Exactly(1));
    super::register_core!(vm, "+", add, Arity::Many);
    super::register_core!(vm, "-", sub, Arity::AtLeast(1));
    super::register_core!(vm, "*", mul, Arity::Many);
    super::register_core!(vm, "/", div, Arity::AtLeast(1));
    super::register_core!(vm, "<", lt, Arity::Many);
    super::register_core!(vm, "<=", lt_eq, Arity::Many);
    super::register_core!(vm, ">", gt, Arity::Many);
    super::register_core!(vm, ">=", gt_eq, Arity::Many);
    super::register_core!(vm, "=", num_eq, Arity::Many);
}

// R7RS 6.2.6 Numerical operations
fn number_p(_vm: &mut Instance, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match unary_procedure(&args)? {
        Value::Number(_) => Ok(Value::Bool(true).into()),
        _ => Ok(Value::Bool(false).into()),
    }
}

fn as_number<'a>(_vm: &mut Instance, v: &'a Value) -> FunctionResult<&'a number::Number> {
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
        pub fn $func(_vm: &mut Instance, args: Vec<Value>) -> FunctionResult<Access<Value>> {
            let mut result = number::Number::fixnum($identity);

            match rest_procedure(&args)? {
                numbers => {
                    for n in numbers {
                        match n.to_owned() {
                            Value::Number(n) => result = result.$op(n)?,
                            v => return Err(error::argument_error(v, "is not a number")),
                        }
                    }
                }
            }

            Ok(Value::Number(result).into())
        }
    };
}

define_fold!(add, 0, add);
define_fold!(mul, 1, mul);

macro_rules! define_reduction {
    ($func:ident, $op:ident) => {
        pub fn $func(_vm: &mut Instance, args: Vec<Value>) -> FunctionResult<Access<Value>> {
            match positional_and_rest_procedure1(&args)? {
                (num @ Value::Number(_), rest) if rest.is_empty() => Ok(num.clone().into()),

                (Value::Number(init), rest) => {
                    let mut result = init.clone();

                    for n in rest {
                        match n.to_owned() {
                            Value::Number(n) => result = result.$op(n)?,
                            v => return Err(error::argument_error(v.clone(), "is not a number")),
                        }
                    }

                    Ok(Value::Number(result).into())
                }
                (other, _) => Err(error::argument_error(other.clone(), "Expected number")),
            }
        }
    };
}

define_reduction!(sub, sub);
define_reduction!(div, div);

macro_rules! define_ordering {
    ($func:ident, $op:tt) => {
        fn $func(vm: &mut Instance, args: Vec<Value>) -> FunctionResult<Access<Value>> {
            let mut result = true;

            match rest_procedure(&args)? {
                rest => {
                   for n in 0..rest.len() {
                     if n == 0 {
                       result = true;
                     } else {
                       result = as_number(vm, &rest[n - 1].get_inner_ref())? $op as_number(vm, &rest[n].get_inner_ref())?;
                    }
                  }
                }
            };


            Ok(Value::Bool(result).into())
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
    use crate::compiler::source::StringSource;
    use crate::compiler::Compiler;
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
        let mut source = StringSource::new(inp);
        let mut compiler = Compiler::new();
        let mut vm = VM::default();
        let unit = compiler.compile(&mut source).unwrap();
        vm.interpret(unit).unwrap()
    }
}
