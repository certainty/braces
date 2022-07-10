use super::{HasOrigin, Origin};

pub struct FileSource {
    file: std::fs::File,
    path: std::path::PathBuf,
}

impl FileSource {
    pub fn new(p: std::path::PathBuf) -> Self {
        Self {
            file: std::fs::File::open(p.clone()).unwrap(),
            path: p,
        }
    }
}

impl HasOrigin for FileSource {
    fn origin(&self) -> Origin {
        Origin::File(self.path.clone())
    }
}

impl std::io::Read for FileSource {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        self.file.read(buf)
    }
}
