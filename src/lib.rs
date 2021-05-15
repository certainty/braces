extern crate im;
extern crate thiserror;

extern crate nom;
extern crate nom_locate;
#[macro_use]
extern crate lazy_static;

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

use directories::BaseDirs;

pub fn braces_config_directory() -> std::path::PathBuf {
    if let Some(base_dirs) = BaseDirs::new() {
        base_dirs.config_dir().join("braces")
    } else {
        panic!("Can't find base directories")
    }
}
