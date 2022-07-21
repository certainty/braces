use rustc_hash::FxHashMap;

use super::{equality::SchemeEqual, error};
use std::{
    cell::RefCell,
    io::Read,
    io::{Stderr, Stdin, Stdout, Write},
    rc::Rc,
};

pub type Result<T> = std::result::Result<T, error::RuntimeError>;

#[derive(Clone, Debug, PartialEq)]
pub enum PortType {
    Binary,
    Textual,
}

pub trait ReadWrite: Read + Write {}

#[derive(Debug)]
pub struct OutputPort<T: Write> {
    underlying: Rc<RefCell<T>>,
}

#[derive(Debug)]
pub struct InputPort<T: Read> {
    underlying: Rc<RefCell<T>>,
}

// holds references to input and output facilitys
#[repr(transparent)]
pub struct IORegistry(FxHashMap<IOKey, IOEntry>);

impl IORegistry {
    pub fn new() -> Self {
        let mut map = FxHashMap::default();

        map.insert(IOKey::Stdin, IOEntry::stdin());
        map.insert(IOKey::Stdout, IOEntry::stdout());
        map.insert(IOKey::Stderr, IOEntry::stderr());

        Self(map)
    }

    pub fn stdout(&mut self) -> &mut Rc<RefCell<Stdout>> {
        match self.0.get_mut(&IOKey::Stdout).unwrap() {
            IOEntry::Stdout(inner) => inner,
            _ => unreachable!(),
        }
    }
}

pub enum IOEntry {
    Stdout(Rc<RefCell<Stdout>>),
    Stdin(Rc<RefCell<Stdin>>),
    Stderr(Rc<RefCell<Stderr>>),
}

impl IOEntry {
    pub fn stdin() -> Self {
        Self::Stdin(Rc::new(RefCell::new(std::io::stdin())))
    }

    pub fn stdout() -> Self {
        Self::Stdout(Rc::new(RefCell::new(std::io::stdout())))
    }

    pub fn stderr() -> Self {
        Self::Stderr(Rc::new(RefCell::new(std::io::stderr())))
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum IOKey {
    Stdin,
    Stdout,
    Stderr,
    Uri(String),
}

#[derive(Clone, PartialEq)]
#[repr(transparent)]
pub struct Port(IOKey);

impl Port {
    pub fn write_char(&mut self, c: &char) -> Result<usize> {
        todo!()
    }

    pub fn stdin() -> Self {
        Self(IOKey::Stdin)
    }

    pub fn stdout() -> Self {
        Self(IOKey::Stdout)
    }
}

impl std::fmt::Debug for Port {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let struct_name = match &self.0 {
            IOKey::Stdin => "stdin".to_string(),
            IOKey::Stdout => "stdout".to_string(),
            IOKey::Stderr => "stderr".to_string(),
            IOKey::Uri(uri) => format!("{}", uri),
        };

        f.debug_struct(&struct_name).finish()
    }
}

impl SchemeEqual<Port> for Port {
    fn is_eq(&self, other: &Port) -> bool {
        self == other
    }

    fn is_eqv(&self, other: &Port) -> bool {
        self == other
    }

    fn is_equal(&self, other: &Port) -> bool {
        self == other
    }
}
