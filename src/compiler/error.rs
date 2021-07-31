use super::frontend::expander;
use super::frontend::parser;
use super::frontend::reader;
use super::source::SourceType;
use super::source_location::SourceLocation;

pub trait UserMessage {
    fn print_user_friendly_message(&self);
}

impl UserMessage for reader::Error {
    fn print_user_friendly_message(&self) {
        match self {
            reader::Error::IncompleteInput => eprintln!("ReadError: unexpected end of input"),
            reader::Error::IoError(e) => eprintln!("ReadError: error during read caused by: {}", e),
            reader::Error::ReadError(details) => {
                eprintln!("ReadError: error while parsing the next datum");
                if let Some(detail) = details.first() {
                    eprintln!(
                        "--> {} in context: {}",
                        source_location_string(&detail.location),
                        detail.context
                    );
                    eprintln!("   |");
                    eprintln!("   |");
                    eprintln!("{}  |         {}", detail.location.line, detail.span);
                    eprintln!("   |\n");
                }
            }
        }
    }
}

impl UserMessage for parser::Error {
    fn print_user_friendly_message(&self) {
        match self {
            parser::Error::ReadError(e) => e.print_user_friendly_message(),
            parser::Error::ParseError(message, location) => {
                eprintln!("ParseError: {}", message);
                eprintln!("--> {} ", source_location_string(location))
            }
            parser::Error::DomainError(message, location) => {
                eprintln!("DomainError: {}", message);
                eprintln!("--> {} ", source_location_string(location))
            }
            parser::Error::ExpansionError(message, _datum) => {
                eprintln!("ExpansionError: {}", message);
            }

            parser::Error::ExpanderError(e) => e.print_user_friendly_message(),
            parser::Error::Bug(e) => {
                eprintln!("ParserBug: {}", e)
            }
        }
    }
}

impl UserMessage for expander::Error {
    fn print_user_friendly_message(&self) {
        match self {
            _ => eprintln!("Failed to expand  [TODO: better error message]"),
        }
    }
}

impl UserMessage for super::Error {
    fn print_user_friendly_message(&self) {
        match self {
            super::Error::ParseError(e) => e.print_user_friendly_message(),
            super::Error::GenerationError(e) => eprintln!("Failed to generate code: {:?}", e),
        }
    }
}

fn source_location_string(loc: &SourceLocation) -> String {
    match &loc.source_type {
        SourceType::Synthetic => format!("{}:{}", loc.line, loc.column),
        SourceType::Buffer(name) => format!("[buffer] {}:{}:{}", name, loc.line, loc.column),
        SourceType::File(path) => format!(
            "[file] {}:{}:{}",
            path.as_path().display(),
            loc.line,
            loc.column
        ),
    }
}
