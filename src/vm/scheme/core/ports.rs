use crate::vm::scheme::ffi::*;
use crate::vm::value::access::Access;
use crate::vm::value::port::Port;
use crate::vm::value::procedure::foreign;
use crate::vm::value::procedure::Arity;
use crate::vm::value::{error, Value};
use crate::vm::Instance;
use crate::vm::VM;
use std::io::Write;

pub fn register(vm: &mut VM) {
    super::register_core!(vm, "port?", port_p, Arity::Exactly(1));
    super::register_core!(
        vm,
        "current-input-port",
        current_input_port,
        Arity::Exactly(0)
    );
    super::register_core!(
        vm,
        "current-output-port",
        current_output_port,
        Arity::Exactly(0)
    );

    super::register_core!(
        vm,
        "current-error-port",
        current_error_port,
        Arity::Exactly(0)
    );

    super::register_core!(vm, "write-char", write_char, Arity::AtLeast(1));
    super::register_core!(vm, "write-string", write_string, Arity::AtLeast(1));
    super::register_core!(vm, "flush-output-port", flush_port, Arity::AtLeast(0));
}

fn port_p(_vm: &mut Instance, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match unary_procedure(&args)? {
        Value::Port(_) => Ok(Value::Bool(true).into()),
        _ => Ok(Value::Bool(false).into()),
    }
}

// TODO: return srfi-parameter value
fn current_input_port(_vm: &mut Instance, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    if args.len() != 0 {
        return Err(error::arity_mismatch(Arity::Exactly(0), args.len()));
    }

    Ok(Value::Port(Port::stdin()).into())
}

fn current_output_port(_vm: &mut Instance, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    if args.len() != 0 {
        return Err(error::arity_mismatch(Arity::Exactly(0), args.len()));
    }

    Ok(Value::Port(Port::stdout()).into())
}

fn current_error_port(_vm: &mut Instance, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    if args.len() != 0 {
        return Err(error::arity_mismatch(Arity::Exactly(0), args.len()));
    }

    todo!()
}

fn write_char(vm: &mut Instance, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match positional_and_rest_procedure1(&args)? {
        (Value::Char(c), r) if r.is_empty() => {
            let mut stdout = vm.io_resources.stdout().borrow_mut();
            stdout.write_all(c.encode_utf8(&mut [0; 4]).as_bytes())?;
            Ok(Value::Unspecified.into())
        }
        (Value::Char(c), r) => todo!(),
        other => {
            println!("other: {:?}", other);
            Err(error::arity_mismatch(Arity::AtLeast(1), args.len()))
        }
    }
}

fn write_string(vm: &mut Instance, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match positional_and_rest_procedure1(&args)? {
        (Value::String(s), r) if r.is_empty() => {
            let mut stdout = vm.io_resources.stdout().borrow_mut();
            stdout.write_all((&s.as_ref()).as_bytes())?;
            Ok(Value::Unspecified.into())
        }
        (Value::String(_s), _r) => todo!(),
        (other, _) => Err(error::argument_error(other.clone(), "Expected String")),
    }
}

fn flush_port(vm: &mut Instance, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    match optional_unary_procedure(&args)? {
        None => {
            vm.io_resources.stdout().borrow_mut().flush()?;
            Ok(Value::Unspecified.into())
        }
        Some(_v) => todo!(),
    }
}
