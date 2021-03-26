use std::convert::From;
use std::io;

pub trait Source {
    fn read_to_string(&mut self, buf: &mut String) -> io::Result<()>;
}

impl From<String> for StringSource {
    fn from(str: String) -> StringSource {
        StringSource { internal: str }
    }
}

impl From<&str> for StringSource {
    fn from(s: &str) -> StringSource {
        StringSource::from(String::from(s))
    }
}

pub struct StringSource {
    internal: String,
}

impl Source for StringSource {
    fn read_to_string(&mut self, buf: &mut String) -> io::Result<()> {
        *buf = self.internal.clone();
        Ok(())
    }
}

pub struct ReaderSource<T: std::io::Read> {
    reader: T,
}

impl<T: std::io::Read> Source for ReaderSource<T> {
    fn read_to_string(&mut self, mut buf: &mut String) -> io::Result<()> {
        self.reader.read_to_string(&mut buf)?;
        Ok(())
    }
}
