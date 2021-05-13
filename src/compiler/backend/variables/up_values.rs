use crate::compiler::backend::code_generator::{Error, Result};
use crate::vm::byte_code::chunk::AddressType;
use crate::vm::byte_code::chunk::ConstAddressType;

#[derive(PartialEq, Clone, Debug)]
pub struct UpValue {
    pub address: AddressType,
    pub is_local: bool,
}
impl UpValue {
    pub fn new(address: AddressType, is_local: bool) -> Self {
        Self { address, is_local }
    }
}

pub struct UpValues {
    max: usize,
    up_values: Vec<UpValue>,
}

impl UpValues {
    pub fn new(limit: usize) -> Self {
        Self {
            max: limit,
            up_values: Vec::with_capacity(limit),
        }
    }

    pub fn to_vec(&self) -> Vec<UpValue> {
        self.up_values.clone()
    }

    pub fn add(&mut self, local_addr: usize, is_local: bool) -> Result<ConstAddressType> {
        if self.up_values.len() >= self.max {
            Err(Error::TooManyUpValues)
        } else {
            let value = UpValue::new(local_addr, is_local);
            if let Some(addr) = self.up_values.iter().position(|v| v == &value) {
                Ok(addr as ConstAddressType)
            } else {
                self.up_values.push(value);
                Ok(self.last_address())
            }
        }
    }

    pub fn len(&self) -> usize {
        self.up_values.len()
    }

    pub fn last_address(&self) -> ConstAddressType {
        (self.up_values.len() - 1) as ConstAddressType
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_up_value_add_deduplicates() {
        let mut up_values = UpValues::new(10);

        let first_addr = up_values.add(10, true).unwrap();
        up_values.add(20, true).unwrap();
        //again
        let second_addr = up_values.add(10, true).unwrap();
        // again but not local
        let third_addr = up_values.add(10, false).unwrap();

        assert_eq!(first_addr, second_addr);
        assert_ne!(first_addr, third_addr);

        assert_eq!(
            up_values.up_values,
            vec![
                UpValue::new(10, true),
                UpValue::new(20, true),
                UpValue::new(10, false)
            ]
        )
    }
}
