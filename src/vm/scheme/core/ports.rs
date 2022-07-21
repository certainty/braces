use crate::vm::scheme::ffi::*;
use crate::vm::value::access::Access;
use crate::vm::value::port::Port;
use crate::vm::value::procedure::foreign;
use crate::vm::value::procedure::Arity;
use crate::vm::value::{error, Value};
use crate::vm::Instance;
use crate::vm::VM;

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

    //super::register_core!(vm, "write-char", write_char, Arity::AtLeast(1))
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

    todo!()
}

fn current_output_port(_vm: &mut Instance, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    if args.len() != 0 {
        return Err(error::arity_mismatch(Arity::Exactly(0), args.len()));
    }

    todo!()
}

fn current_error_port(_vm: &mut Instance, args: Vec<Value>) -> FunctionResult<Access<Value>> {
    if args.len() != 0 {
        return Err(error::arity_mismatch(Arity::Exactly(0), args.len()));
    }

    todo!()
}
