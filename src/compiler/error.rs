use super::frontend::parser::expression;
use super::frontend::parser::sexp::error::ReadError;
use super::source::SourceType;
use super::source_location::SourceLocation;

pub trait UserMessage {
    fn print_user_friendly_message(&self);
}

impl UserMessage for ReadError {
    fn print_user_friendly_message(&self) {
        match self {
            ReadError::IncompleteInput => eprintln!("ReadError: unexpected end of input"),
            ReadError::IoError(e) => eprintln!("ReadError: error during read caused by: {}", e),
            ReadError::ReadError(details) => {
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

impl UserMessage for expression::error::Error {
    fn print_user_friendly_message(&self) {
        match self {
            expression::error::Error::ReadError(e) => e.print_user_friendly_message(),
            expression::error::Error::ParseError(message, location) => {
                eprintln!("ParseError: {}", message);
                eprintln!("--> {} ", source_location_string(location))
            }
            expression::error::Error::DomainError(message, location) => {
                eprintln!("DomainError: {}", message);
                eprintln!("--> {} ", source_location_string(location))
            }
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
        SourceType::Buffer(name) => format!("[buffer] {}:{}:{}", name, loc.line, loc.column),
        SourceType::File(path) => format!(
            "[file] {}:{}:{}",
            path.as_path().display(),
            loc.line,
            loc.column
        ),
    }
}
