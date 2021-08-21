use super::{HasOrigin, Origin};
use std::io::Cursor;

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct StringSource(Cursor<String>);

impl StringSource {
    pub fn new<C: Into<String>>(content: C) -> Self {
        Self(Cursor::new(content.into()))
    }
}

impl HasOrigin for StringSource {
    fn origin(&self) -> Origin {
        Origin::Synthetic
    }
}

impl std::io::Read for StringSource {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.0.read(buf)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Read;

    #[test]
    fn buffer_source_works() {
        let mut s = StringSource::new("test content");
        let mut out = String::new();

        s.read_to_string(&mut out).unwrap();
        assert_eq!("test content", &out)
    }
}
