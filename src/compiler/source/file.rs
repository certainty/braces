use super::{HasOrigin, Origin};

// TODO: rethink this source
// Should it slurp the contents right away or really keep the internal file?
pub struct FileSource {
    pub path: std::path::PathBuf,
    file: std::fs::File,
}

impl FileSource {
    pub fn new(p: std::path::PathBuf) -> Self {
        Self {
            path: p.clone(),
            file: std::fs::File::open(p).unwrap(),
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
