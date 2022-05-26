extern crate alloc;

use super::{
    error::{Error, StdIoError},
    io::VecReader,
    Result,
};
use ink_prelude::format;
use ink_prelude::vec::Vec;
use ink_prelude::{collections::BTreeMap, string::String};

const DEFAULT_SECTION: &str = "default";
const DEFAULT_COMMENT: &str = "#";
const DEFAULT_COMMENT_SEM: &str = ";";
const DEFAULT_MULTI_LINE_SEPARATOR: &str = "\\";

pub(crate) struct Config {
    data: BTreeMap<String, BTreeMap<String, String>>,
}

impl Config {
    pub(crate) fn from_str<S: AsRef<str>>(s: S) -> Result<Self> {
        let mut c = Config {
            data: BTreeMap::new(),
        };

        c.parse_buffer(&mut VecReader::from_string(s))?;
        Ok(c)
    }

    fn parse_buffer(&mut self, reader: &mut VecReader) -> Result<()> {
        let mut section = String::new();

        loop {
            let mut line = String::new();
            let bytes = reader.read_line(&mut line)?;
            if bytes == 0 {
                // EOF reached
                break Ok(());
            }
            line = line.trim().into();
            if line.is_empty()
                || line.starts_with(DEFAULT_COMMENT)
                || line.starts_with(DEFAULT_COMMENT_SEM)
            {
                continue;
            } else if line.starts_with('[') && line.ends_with(']') {
                section = line[1..line.len() - 1].into();
            } else {
                let mut next_section = String::new();
                while line.ends_with(DEFAULT_MULTI_LINE_SEPARATOR) {
                    line = line[..line.len() - 1].trim_end().into();

                    let mut inner_line = String::new();
                    let inner_bytes = reader.read_line(&mut inner_line)?;
                    if inner_bytes == 0 {
                        break;
                    }

                    let inner_line: String = inner_line.trim().into();
                    if inner_line.is_empty()
                        || inner_line.starts_with(DEFAULT_COMMENT)
                        || inner_line.starts_with(DEFAULT_COMMENT_SEM)
                    {
                        continue;
                    }

                    if inner_line.starts_with('[') && inner_line.ends_with(']') {
                        next_section = inner_line[1..inner_line.len() - 1].into();
                    } else {
                        line.push_str(&inner_line);
                    }
                }

                let option_val: Vec<&str> = line
                    .trim_end_matches(|c| {
                        char::is_whitespace(c) || String::from(c) == DEFAULT_MULTI_LINE_SEPARATOR
                    })
                    .splitn(2, '=')
                    .map(|e| e.trim())
                    .collect();

                if option_val.len() != 2 {
                    return Err(Error::from(StdIoError::from(format!(
                        "parse content error, line={}",
                        line
                    ))));
                }

                self.add_config(section.clone(), option_val[0].into(), option_val[1].into());

                if !next_section.is_empty() {
                    section = next_section;
                }
            }
        }
    }

    pub(crate) fn add_config(&mut self, mut section: String, option: String, value: String) {
        if section.is_empty() {
            section = DEFAULT_SECTION.into();
        }
        let section_value = self.data.entry(section).or_insert_with(BTreeMap::new);

        // if key not exists then insert, else update
        let key_value = section_value.get_mut(&option);
        match key_value {
            Some(old_value) => {
                *old_value = value;
            }
            None => {
                section_value.insert(option, value);
            }
        }
    }

    pub fn get(&self, key: &str) -> Option<&str> {
        let keys: Vec<String> = key.to_lowercase().split("::").map(String::from).collect();
        if keys.len() >= 2 {
            let section = &keys[0];
            let option = &keys[1];
            self.data
                .get(section)
                .and_then(|m| m.get(option).map(|v| v.as_str()))
        } else {
            let section = DEFAULT_SECTION;
            let option = &keys[0];
            self.data
                .get(section)
                .and_then(|m| m.get(option).map(|v| v.as_str()))
        }
    }

    #[allow(dead_code)]
    pub(crate) fn set(&mut self, key: &str, value: &str) {
        assert!(!key.is_empty(), "key can't be empty");
        let keys: Vec<String> = key.to_lowercase().split("::").map(String::from).collect();
        if keys.len() >= 2 {
            let section = &keys[0];
            let option = &keys[1];
            self.add_config(section.into(), option.into(), value.into());
        } else {
            let section = DEFAULT_SECTION;
            let option = &keys[0];
            self.add_config(section.into(), option.into(), value.into());
        }
    }

    #[allow(dead_code)]
    pub(crate) fn get_bool(&self, key: &str) -> Option<bool> {
        self.get(key).and_then(|v| v.parse::<bool>().ok())
    }

    #[allow(dead_code)]
    pub(crate) fn get_string(&self, key: &str) -> Option<String> {
        self.get_str(key).map(|v| v.into())
    }

    pub(crate) fn get_str(&self, key: &str) -> Option<&str> {
        self.get(key)
    }

    #[allow(dead_code)]
    pub(crate) fn get_int(&self, key: &str) -> Option<i64> {
        self.get(key).and_then(|v| v.parse::<i64>().ok())
    }
}
