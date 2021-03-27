// the scheme printer

pub trait Print {
    fn print(&self) -> Option<String>;
}

pub fn print<T: Print>(val: &T) -> String {
    val.print().unwrap_or(String::from("#<unprintable>"))
}
