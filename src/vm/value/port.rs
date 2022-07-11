use std::io::{BufWriter, Write};

pub type Result<T> = std::result::Result<T, super::error::RuntimeError>;

#[derive(Debug)]
pub enum Content<T> {
    Eof,
    Data(T),
}

pub trait SchemePort {
    fn is_binary_port() -> bool;
    fn is_textual_port() -> bool;
    fn is_input_port() -> bool;
}

pub trait BinaryOutputPort {
    fn write_u8(&mut self, byte: u8) -> Result<usize>;
    fn flush(&mut self) -> Result<()>;
    fn close(&self) -> Result<()>;
}

pub trait BinaryInputPort {
    fn read_u8(&mut self) -> Result<Content<u8>>;
    fn peek(&self) -> Result<Content<u8>>;
    fn is_ready(&self) -> bool;
    fn close(&self) -> Result<()>;
}

pub trait TextualOutputPort {
    fn write_char(&mut self, c: char) -> Result<usize>;
    fn write(&mut self, buf: &str) -> Result<usize>;
    fn flush(&mut self) -> Result<()>;
    fn close(&self) -> Result<()>;
}

pub trait TextualInputPort {
    fn read_line(&mut self) -> Result<Content<String>>;
    fn read_char(&mut self) -> Result<Content<char>>;
    fn peek_char(&mut self) -> Result<Content<char>>;
    fn is_ready(&self) -> bool;
    fn close(&self) -> Result<()>;
}
