pub trait Source {
    fn for_message(&self) -> String;
    fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<()>;
}
