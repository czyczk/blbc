/// This is an ink! compatible lite version of casbin-rs.
/// See https://github.com/casbin/casbin-rs/ for the original implementation.
///
use self::error::Error;

pub mod config;
pub mod error;
pub mod io;
pub mod model;
pub mod util;

pub type Result<T> = core::result::Result<T, Error>;
