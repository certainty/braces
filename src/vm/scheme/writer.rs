use super::value::Value;

pub struct Writer;

impl Writer {
    pub fn external_representation(&self, v: &Value) -> &str {
        match v {
            Value::Bool(true) => "#t",
            Value::Bool(false) => "#f",
            Value::Unspecified => "#<unspecified>",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_write_bool() {
        let writer = Writer;

        assert_eq!(writer.external_representation(&Value::Bool(true)), "#t");
        assert_eq!(writer.external_representation(&Value::Bool(false)), "#f");
    }
}
