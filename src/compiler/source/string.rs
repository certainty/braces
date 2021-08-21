use super::{HasOrigin, Origin};
use std::io::Write;

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct StringSource(String);

impl StringSource {
    pub fn new<C: Into<String>>(content: C) -> Self {
        Self(content.into())
    }
}

impl HasOrigin for StringSource {
    fn origin(&self) -> Origin {
        Origin::Synthetic
    }
}

impl std::io::Read for StringSource {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.0.as_bytes().to_vec().write(buf)
    }
}
