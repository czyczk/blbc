/// This is an ink! compatible lite version of casbin-rs.
/// See https://github.com/casbin/casbin-rs/ for the original implementation.
///
use self::error::Error;

mod adapter;
mod config;
mod core_api;
pub mod effector;
pub mod enforcer;
pub mod error;
mod io;
mod macros;
pub mod model;
mod util;

pub type Result<T> = core::result::Result<T, Error>;
