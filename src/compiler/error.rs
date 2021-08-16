use super::source::{Registry, SourceId, SourceType};
use crate::compiler::frontend;
use crate::compiler::frontend::error::Detail;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

#[derive(Debug)]
pub enum Error {
    FrontendError(frontend::error::Error),
}

impl Error {
    pub fn print(registry: &Registry, e: Error) {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        term::emit(
            &mut writer.lock(),
            &config,
            &registry.sources(),
            &Self::diagnostic(e),
        )
        .unwrap();
    }

    pub fn diagnostic(e: Error) -> Diagnostic<SourceId> {
        match e {
            Error::FrontendError(fe) => Self::frontend_diagnostic(fe),
        }
    }

    fn frontend_diagnostic(e: frontend::error::Error) -> Diagnostic<SourceId> {
        use frontend::error::Error::*;

        match e {
            IoError(message, source_id, underlying) => Diagnostic::error()
                .with_code("E001")
                .with_message("failure while reading input")
                .with_notes(vec![message]),
            IncompleteInput(message, source) => Diagnostic::error()
                .with_code("E010")
                .with_message("unexpected end of input")
                .with_notes(vec![message]),
            ReadError(message, detail, more_details) => {
                let mut labels =
                    vec![Label::primary(detail.location.id, detail.location.span)
                        .with_message(message)];
                labels.extend(more_details.iter().map(Self::to_label));

                Diagnostic::error()
                    .with_code("E011")
                    .with_message("failed to read input")
                    .with_labels(labels)
            }
            ParseError(message, detail, more_details) => {
                let mut labels =
                    vec![Label::primary(detail.location.id, detail.location.span)
                        .with_message(message)];
                labels.extend(more_details.iter().map(Self::to_label));

                Diagnostic::error()
                    .with_code("E012")
                    .with_message("failed to parse input")
                    .with_labels(labels)
            }
            Bug(message) => Diagnostic::bug().with_message(message),
        }
    }

    fn to_label(detail: &Detail) -> Label<SourceId> {
        Label::secondary(detail.location.id, detail.location.span).with_message(detail.content)
    }
}
