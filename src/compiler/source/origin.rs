impl std::fmt::Display for Origin {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::Synthetic => formatter.write_str("synthetic"),
            Self::Buffer(name) => formatter.write_fmt(format_args!("buffer({})", name)),
            Self::File(path) => {
                formatter.write_fmt(format_args!("file({})", path.to_str().unwrap_or("")))
            }
        }
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
