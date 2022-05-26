use crate::compiler::{
    frontend::reader::datum::Datum,
    source::{HasSourceLocation, Location},
};

use super::{
    identifier::Identifier, sequence::BeginExpression, CoreParser, Expression, ParseResult, Result,
};

#[derive(PartialEq, Clone, Debug)]
pub enum ImportSpec {
    LibraryName(Identifier),
    Only(Vec<(ImportSpec, Identifier)>),
    Except(Vec<(ImportSpec, Identifier)>),
    Prefix(Box<ImportSpec>, Identifier),
    Rename(Box<ImportSpec>, Vec<(Identifier, Identifier)>),
}

#[derive(PartialEq, Clone, Debug)]
pub enum ExportSpec {
    Rename(Identifier, Identifier),
    Id(Identifier),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Include {
    CaseInsensitive(String),
    CaseSenstive(String),
}

#[derive(PartialEq, Clone, Debug)]
pub struct CondExpand {}

#[derive(PartialEq, Clone, Debug)]
pub struct Declaration {
    pub begin: BeginExpression,
    pub import: Vec<ImportSpec>,
    pub export: Vec<ExportSpec>,
    pub include: Vec<Include>,
    pub cond_expand: Vec<CondExpand>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct LibraryExpression {
    pub name: Identifier,
    pub declaration: Declaration,
    location: Location,
}

impl HasSourceLocation for LibraryExpression {
    fn source_location(&self) -> &Location {
        &self.location
    }
}
