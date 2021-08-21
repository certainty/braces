use super::{HasOrigin, Origin};
use std::io::Cursor;

#[derive(Clone, Debug)]
pub struct BufferSource {
    content: Cursor<String>,
    name: String,
}

impl BufferSource {
    pub fn new<L: Into<String>, C: Into<String>>(content: C, label: L) -> Self {
        Self {
            content: Cursor::new(content.into()),
            name: label.into(),
        }
    }
}

impl HasOrigin for BufferSource {
    fn origin(&self) -> Origin {
        Origin::Buffer(self.name.clone())
    }
}

impl std::io::Read for BufferSource {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.content.read(buf)
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::source::BufferSource;
    use std::io::Read;

    #[test]
    fn buffer_source_works() {
        let mut s = BufferSource::new("test content", "foo");
        let mut out = String::new();

        s.read_to_string(&mut out).unwrap();
        assert_eq!("test content", &out)
    }
}
