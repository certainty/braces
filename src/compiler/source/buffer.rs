use super::{HasOrigin, Origin};
use std::io::Write;

#[derive(Clone, Debug)]
pub struct BufferSource {
    content: String,
    name: String,
}

impl BufferSource {
    pub fn new<L: Into<String>, C: Into<String>>(content: C, label: L) -> Self {
        Self {
            content: content.into(),
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
        self.content.as_bytes().to_vec().write(buf)
    }
}
