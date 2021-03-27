use super::{Numeric, Value};
use crate::vm::error::VmError;

type Result<T> = std::result::Result<T, VmError>;

pub fn add(lhs: Value, rhs: Value) -> Result<Numeric> {
    match (lhs, rhs) {
        (Value::Number(Numeric::Fixnum(lhs)), Value::Number(Numeric::Fixnum(rhs))) => {
            Ok(Numeric::Fixnum(lhs + rhs))
        }
        _ => Err(VmError::TypeError("Invalid operands for +".into())),
    }
}
