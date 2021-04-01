use super::value;

pub trait ExternalRepresentation {
    fn external_repr(&self) -> &str;
}

pub struct Writer {}

impl Writer {
    pub fn write<T: ExternalRepresentation>(&self, v: &T) -> String {
        String::from(v.external_repr())
    }
}
