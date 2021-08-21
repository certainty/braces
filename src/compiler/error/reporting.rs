use super::Error;
use crate::compiler::backend::error::Error as BackendError;
use crate::compiler::frontend::error::Detail as FrontendErrorDetail;
use crate::compiler::frontend::error::Error as FrontendError;
use crate::compiler::source::registry;
use crate::compiler::source::{Registry, SourceId};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

pub struct ErrorReporter<'a> {
    source_registry: &'a Registry,
}

impl<'a> ErrorReporter<'a> {
    pub fn new(source_registry: &'a Registry) -> Self {
        Self { source_registry }
    }
}

impl<'a> ErrorReporter<'a> {
    pub fn report_error(&self, e: &Error) {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        term::emit(
            &mut writer.lock(),
            &config,
            &self.source_registry,
            &self.diagnostic(e),
        )
        .unwrap();
    }

    pub fn diagnostic(&self, e: &Error) -> Diagnostic<SourceId> {
        match e {
            Error::IoError(e) => Diagnostic::error()
                .with_code("E000")
                .with_message(format!("{}", e)),
            Error::SourceCodeRegistryError(e) => self.registry_error_diagnostic(e),
            Error::FrontendError(fe) => self.frontend_diagnostic(fe),
            Error::BackendError(be) => self.backend_diagnostic(be),
        }
    }

    fn backend_diagnostic(&self, _e: &BackendError) -> Diagnostic<SourceId> {
        todo!()
    }

    fn registry_error_diagnostic(&self, _e: &registry::Error) -> Diagnostic<SourceId> {
        todo!()
    }

    fn frontend_diagnostic(&self, e: &FrontendError) -> Diagnostic<SourceId> {
        use FrontendError::*;
        match e {
            IoError(e) => Diagnostic::error()
                .with_code("E000")
                .with_message(format!("{}", e)),
            SourceError(message, _source_id, _underlying) => Diagnostic::error()
                .with_code("E001")
                .with_message("failure while reading input")
                .with_notes(vec![message.clone()]),
            IncompleteInput(message, _source) => Diagnostic::error()
                .with_code("E010")
                .with_message("unexpected end of input")
                .with_notes(vec![message.clone()]),
            ReadError(message, detail, more_details) => {
                let mut labels =
                    vec![
                        Label::primary(detail.location.id, detail.location.span.clone())
                            .with_message(message),
                    ];
                labels.extend(more_details.iter().map(Self::to_label));

                Diagnostic::error()
                    .with_code("E011")
                    .with_message("failed to read input")
                    .with_labels(labels)
            }
            ParseError(message, detail, more_details) => {
                let mut labels =
                    vec![
                        Label::primary(detail.location.id, detail.location.span.clone())
                            .with_message(message),
                    ];
                labels.extend(more_details.iter().map(Self::to_label));

                Diagnostic::error()
                    .with_code("E012")
                    .with_message("failed to parse input")
                    .with_labels(labels)
            }
            ExpansionError(message, detail, more_details) => {
                let mut labels =
                    vec![
                        Label::primary(detail.location.id, detail.location.span.clone())
                            .with_message(message),
                    ];
                labels.extend(more_details.iter().map(Self::to_label));

                Diagnostic::error()
                    .with_code("E013")
                    .with_message("failed to expand input")
                    .with_labels(labels)
            }
            Bug(message) => Diagnostic::bug().with_message(message),
        }
    }

    fn to_label(detail: &FrontendErrorDetail) -> Label<SourceId> {
        Label::secondary(detail.location.id, detail.location.span.clone())
            .with_message(detail.content.clone())
    }
}
