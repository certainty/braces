use super::{HasOrigin, Origin};

#[repr(transparent)]
pub struct FileSource(std::path::PathBuf);

impl FileSource {
    pub fn new(p: std::path::PathBuf) -> Self {
        Self(p)
    }
}

impl HasOrigin for FileSource {
    fn origin(&self) -> Origin {
        Origin::File(self.0.clone())
    }
}

impl std::io::Read for FileSource {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let mut file = std::fs::File::open(self.0.clone())?;
        file.read(buf)
    }
}
