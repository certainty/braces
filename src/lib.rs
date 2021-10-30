extern crate im_rc;
extern crate thiserror;

extern crate clap;
extern crate lazy_static;
extern crate nom;

#[macro_use]
extern crate anyhow;

pub mod cmd;
pub mod compiler;
pub mod repl;
pub mod vm;

#[cfg(test)]
#[macro_use]
extern crate matches;

#[cfg(test)]
extern crate quickcheck;

#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

pub const BRACES_VERSION: &str = "0.0.1";

use directories::BaseDirs;

pub fn braces_config_directory() -> std::path::PathBuf {
    if let Some(base_dirs) = BaseDirs::new() {
        base_dirs.config_dir().join("braces")
    } else {
        panic!("Can't find base directories")
    }
}
