use super::Value;
use quickcheck::Arbitrary;

impl Arbitrary for Value {
    fn arbitrary(gen: &mut quickcheck::Gen) -> Self {
        match gen.choose(&[1, 2, 3, 4, 5]) {
            Some(1) => Value::Bool(bool::arbitrary(gen)),
            Some(2) => Value::character(char::arbitrary(gen)),
            Some(3) => Value::String(String::arbitrary(gen)),
            _ => Value::Bool(false),
        }
    }
}
