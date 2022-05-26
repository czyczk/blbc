extern crate alloc;

use super::{
    error::{Error, StdIoError},
    Result,
};
use ink_prelude::vec;
use ink_prelude::{string::String, vec::Vec};

#[derive(Default)]
pub struct VecReader {
    bytes: Vec<u8>,
    cursor: usize,
}

impl<S: AsRef<str>> From<S> for VecReader {
    fn from(str: S) -> Self {
        Self {
            bytes: str.as_ref().as_bytes().into(),
            cursor: Default::default(),
        }
    }
}

impl VecReader {
    pub fn from_string<S: AsRef<str>>(str: S) -> Self {
        str.into()
    }

    pub fn read_line(&mut self, buf: &mut String) -> Result<usize> {
        if self.cursor == self.bytes.len() {
            return Ok(0);
        }

        let mut v = vec![];
        let mut count = 0usize;
        while self.cursor < self.bytes.len() {
            let cur_byte = self.bytes[self.cursor];
            v.push(cur_byte);
            count += 1;
            self.cursor += 1;
            if cur_byte == b'\n' {
                break;
            }
        }

        *buf = String::from_utf8(v).map_err(|_| {
            Error::from(StdIoError::IoError(
                "stream did not contain valid UTF-8".into(),
            ))
        })?;

        Ok(count)
    }
}
