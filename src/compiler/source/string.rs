use super::{HasOrigin, Origin};

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
        Origin::String
    }
}

impl std::io::Read for StringSource {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.content.write(&mut buf)
    }
}
