pub mod numbers;
use super::ffi::*;
use crate::vm::value::access::{Access, Reference};
use crate::vm::value::error;
use crate::vm::value::list::List;
use crate::vm::value::procedure::foreign;
use crate::vm::value::vector::Vector;
use crate::vm::value::Value;
use crate::vm::value::{equality::SchemeEqual, procedure::Arity};
use crate::vm::VM;

macro_rules! register_core {
    ($vm:expr, $name:literal, $func:expr, $arity:expr) => {
        $vm.register_foreign(foreign::Procedure::new($name, $func, $arity))
            .unwrap()
    };
}

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

    register_core!(vm, "list", list_make, Arity::Many);
    register_core!(vm, "car", list_car, Arity::Exactly(1));
    register_core!(vm, "cons", list_cons, Arity::Exactly(2));
    register_core!(vm, "append", list_append, Arity::Exactly(2));

    register_core!(vm, "vector-ref", vector_ref, Arity::Exactly(2));
    register_core!(vm, "vector-cons", vector_cons, Arity::Exactly(2));
    register_core!(vm, "vector-append", vector_append, Arity::Exactly(2));
    register_core!(vm, "gensym", gensym, Arity::Exactly(0));

    numbers::register(vm);
}

pub fn gensym(ctx: &mut VmContext, _args: Vec<Value>) -> FunctionResult<Access<Value>> {
    Ok(ctx.gen_sym().into())
}

//  R7RS 6.1
//  (eqv? obj1 obj2
pub fn eqv(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match binary_procedure(&args)? {
        (lhs, rhs) => Ok(Value::Bool(lhs.is_eqv(rhs)).into()),
    }
}

pub fn eq(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match binary_procedure(&args)? {
        (lhs, rhs) => Ok(Value::Bool(lhs.is_eq(rhs)).into()),
    }
}

pub fn equal(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match binary_procedure(&args)? {
        (lhs, rhs) => Ok(Value::Bool(lhs.is_equal(rhs)).into()),
    }
}

// predicates
pub fn string_p(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match unary_procedure(&args)? {
        Value::String(_) => Ok(Value::Bool(true).into()),
        _ => Ok(Value::Bool(false).into()),
    }
}

pub fn symbol_p(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match unary_procedure(&args)? {
        Value::Symbol(_) => Ok(Value::Bool(true).into()),
        _ => Ok(Value::Bool(false).into()),
    }
}

pub fn char_p(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match unary_procedure(&args)? {
        Value::Char(_) => Ok(Value::Bool(true).into()),
        _ => Ok(Value::Bool(false).into()),
    }
}

pub fn list_p(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match unary_procedure(&args)? {
        Value::ProperList(_) => Ok(Value::Bool(true).into()),
        _ => Ok(Value::Bool(false).into()),
    }
}

pub fn null_p(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match unary_procedure(&args)? {
        Value::ProperList(ls) => Ok(Value::Bool(ls.is_null()).into()),
        _ => Ok(Value::Bool(false).into()),
    }
}

pub fn procedure_p(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match unary_procedure(&args)? {
        Value::Procedure(_) => Ok(Value::Bool(true).into()),
        Value::Closure(_) => Ok(Value::Bool(true).into()),
        _ => Ok(Value::Bool(false).into()),
    }
}

pub fn bool_not(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match unary_procedure(&args)? {
        Value::Bool(v) => Ok(Value::Bool(!*v).into()),
        _ => Ok(Value::Bool(false).into()),
    }
}

pub fn inspect(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    println!("Debug: {:?}", args.first().unwrap());
    Ok(Value::Unspecified.into())
}

pub fn list_make(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    let ls = args.into();
    Ok(Access::ByVal(Value::ProperList(ls)))
}

pub fn list_car(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    unary_procedure(&args).and_then(|v| match v {
        Value::ProperList(ls) => {
            if let Some(v) = ls.first() {
                Ok(Access::ByRef(v.clone()))
            } else {
                Err(error::argument_error(
                    v.clone(),
                    "can't take car of empty list",
                ))
            }
        }
        Value::ImproperList(ls, _) => {
            if let Some(v) = ls.first() {
                Ok(Access::ByRef(v.clone()))
            } else {
                Err(error::argument_error(
                    v.clone(),
                    "can't take car of empty list",
                ))
            }
        }
        _ => Err(error::argument_error(v.clone(), "Expected list")),
    })
}

pub fn list_cons(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match binary_procedure(&args)? {
        (v, Value::ProperList(elements)) => Ok(Value::ProperList(elements.cons(v.clone())).into()),
        (lhs, rhs) => Ok(Value::ImproperList(
            List::singleton(lhs.clone()),
            Reference::from(rhs.clone()),
        )
        .into()),
    }
}

pub fn list_append(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match binary_procedure(&args)? {
        (Value::ProperList(lhs), Value::ProperList(rhs)) => {
            Ok(Value::ProperList(List::append(lhs, rhs)).into())
        }
        (Value::ProperList(lhs), Value::ImproperList(rhs_head, rhs_tail)) => {
            Ok(Value::ImproperList(List::append(lhs, rhs_head), rhs_tail.clone()).into())
        }
        (lhs, Value::ProperList(_)) => Err(error::argument_error(lhs.clone(), "Expected list")),
        (lhs, Value::ImproperList(_rhs_head, _rhs_tail)) => {
            Err(error::argument_error(lhs.clone(), "Expected list"))
        }
        (Value::ProperList(_), rhs) => Err(error::argument_error(rhs.clone(), "Expected list")),
        (lhs, _) => Err(error::argument_error(lhs.clone(), "Expected list")),
    }
}

pub fn vector_ref(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match binary_procedure(&args)? {
        (Value::Vector(vector), Value::Number(index)) => {
            if let Some(idx) = index.to_usize() {
                if let Some(v) = vector.at(idx) {
                    Ok(Access::ByRef(v.clone()))
                } else {
                    Err(error::out_of_bound_error(idx, 0..vector.len()))
                }
            } else {
                Err(error::argument_error(
                    Value::Number(index.clone()),
                    "not a valid index",
                ))
            }
        }
        (other, Value::Number(_)) => Err(error::argument_error(other.clone(), "Expected vector")),
        (Value::Vector(_), other) => Err(error::argument_error(
            other.clone(),
            "Expected numeric index",
        )),
        (other, _) => Err(error::argument_error(
            other.clone(),
            "Expected the two arguments to be of type vector and number",
        )),
    }
}

pub fn vector_cons(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match binary_procedure(&args)? {
        (v, Value::Vector(vector)) => Ok(Value::Vector(vector.cons(v.clone())).into()),
        (_, other) => Err(error::argument_error(other.clone(), "Expected vector")),
    }
}

pub fn vector_append(_ctx: &mut VmContext, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match binary_procedure(&args)? {
        (Value::Vector(lhs), Value::Vector(rhs)) => {
            Ok(Value::Vector(Vector::append(lhs, rhs)).into())
        }
        (lhs, Value::Vector(_)) => Err(error::argument_error(lhs.clone(), "Expected vector")),
        (Value::Vector(_), rhs) => Err(error::argument_error(rhs.clone(), "Expected vector")),
        (lhs, _) => Err(error::argument_error(lhs.clone(), "Expected vector")),
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::source::StringSource;
    use crate::compiler::Compiler;
    use crate::vm::value::{Factory, Value};
    use crate::vm::VM;

    #[test]
    fn test_list_append() {
        let values = Factory::default();
        let mut vm = VM::default();

        let result = run_code(
            &mut vm,
            r#"
        (define x '(1 2 3))
        (define y '(1 2 3))
        (define z (append x y))
        (set! (car x) 5)
        z
        "#,
        )
        .unwrap();

        assert_eq!(
            result,
            values.proper_list(vec![
                values.real(1),
                values.real(2),
                values.real(3),
                values.real(1),
                values.real(2),
                values.real(3)
            ])
        )
    }

    fn run_code(vm: &mut VM, code: &str) -> crate::vm::Result<Value> {
        let mut source = StringSource::new(code);
        let mut compiler = Compiler::new();
        let unit = compiler.compile(&mut source)?;
        vm.interpret(unit)
    }
}
