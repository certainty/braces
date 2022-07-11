use super::{equality::SchemeEqual, error};
use std::{
    io::{Read, Write},
    rc::Rc,
};

pub type Result<T> = std::result::Result<T, error::RuntimeError>;

#[derive(Debug)]
pub enum Content<T> {
    Eof,
    Data(T),
}

#[derive(Clone, Debug, PartialEq)]
pub enum PortType {
    Binary,
    Textual,
}

pub trait ReadWrite: Read + Write {}

#[derive(Clone)]
pub enum Port {
    InputPort(Rc<dyn Read + 'static>, PortType),
    OutputPort(Rc<dyn Write + 'static>, PortType),
    InputOutputPort(Rc<dyn ReadWrite + 'static>, PortType),
}

impl PartialEq for Port {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::InputPort(l0, l1), Self::InputPort(r0, r1)) => Rc::ptr_eq(l0, r0) && l1 == r1,
            (Self::OutputPort(l0, l1), Self::OutputPort(r0, r1)) => Rc::ptr_eq(l0, r0) && l1 == r1,
            (Self::InputOutputPort(l0, l1), Self::InputOutputPort(r0, r1)) => {
                Rc::ptr_eq(l0, r0) && l1 == r1
            }
            _ => false,
        }
    }
}

impl std::fmt::Debug for Port {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let struct_name = match self {
            Self::InputPort(_, _) => "InputPort",
            Self::OutputPort(_, _) => "OutputPort",
            _ => "InputOutputPort",
        };

        f.debug_struct(struct_name)
            .field("type", &self.port_type())
            .finish()
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

impl Port {
    pub fn stdout() -> Self {
        Self::OutputPort(Rc::new(std::io::stdout()), PortType::Textual)
    }

    pub fn stderr() -> Self {
        Self::OutputPort(Rc::new(std::io::stderr()), PortType::Textual)
    }

    pub fn stdin() -> Self {
        Self::InputPort(Rc::new(std::io::stdin()), PortType::Textual)
    }

    pub fn is_output_port(&self) -> bool {
        match self {
            Self::InputPort(_, _) => false,
            _ => true,
        }
    }

    pub fn is_input_port(&self) -> bool {
        match self {
            Self::InputPort(_, _) => true,
            Self::InputOutputPort(_, _) => true,
            _ => false,
        }
    }

    pub fn is_textual(&self) -> bool {
        match self.port_type() {
            PortType::Binary => false,
            _ => true,
        }
    }

    pub fn is_binary(&self) -> bool {
        !self.is_textual()
    }

    fn port_type(&self) -> &PortType {
        match &self {
            Self::InputPort(_, tpe) => tpe,
            Self::OutputPort(_, tpe) => tpe,
            Self::InputOutputPort(_, tpe) => tpe,
        }
    }
}
