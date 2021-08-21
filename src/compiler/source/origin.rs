impl std::fmt::Display for Origin {
    fn fmt(
        &self,
        _formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub enum Origin {
    Synthetic,
    Buffer(std::string::String),
    File(std::path::PathBuf),
}

pub trait HasOrigin {
    fn origin(&self) -> Origin;
}
