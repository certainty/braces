extern crate im;
extern crate pest;
extern crate thiserror;
#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate lazy_static;

extern crate nom;

#[macro_use]
extern crate nom_locate;

pub mod compiler;
pub mod vm;

#[cfg(test)]
#[macro_use]
extern crate matches;

#[cfg(test)]
extern crate quickcheck;

#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;
