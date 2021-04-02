use super::value::Value;

pub struct Writer;

impl Writer {
    pub fn external_representation(&self, v: &Value) -> String {
        match v {
            Value::Bool(true) => "#t".to_string(),
            Value::Bool(false) => "#f".to_string(),
            Value::Symbol(sym) => format!("'{}", sym),
            Value::ProperList(elts) => {
                let body: Vec<String> = elts
                    .iter()
                    .map(|e| self.external_representation(&e))
                    .collect();

                format!("'({})", body.join(" "))
            }
            Value::Unspecified => "#<unspecified>".to_string(),
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

    #[test]
    fn test_write_symbol() {
        let writer = Writer;
        assert_eq!(
            writer.external_representation(&Value::Symbol("...".to_string())),
            "'..."
        );
    }

    #[test]
    fn test_write_proper_list() {
        let writer = Writer;
        let elts = vec![Value::boolean(true), Value::boolean(false)];
        let ls = Value::proper_list(elts);

        assert_eq!(writer.external_representation(&ls), "'(#t #f)");
    }
}
