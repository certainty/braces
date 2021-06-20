use thiserror::Error;
pub mod environment;

#[derive(Error, Debug, Clone)]
pub enum Error {}
