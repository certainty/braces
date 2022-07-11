use std::io::{BufWriter, Write};

#[derive(Debug)]
pub enum Port {
    InputPort(),
    OutputPort(BufWriter<Write>),
}
