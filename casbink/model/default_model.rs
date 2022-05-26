extern crate alloc;

use crate::casbink::error::ModelError;
use crate::casbink::util::remove_comment;
use crate::casbink::Result;
use crate::casbink::{config::Config, util::escape_assertion};
use ink_prelude::collections::BTreeMap;
use ink_prelude::format;
use ink_prelude::string::String;
use ritelinked::LinkedHashMap;

use super::{
    assertion::{Assertion, AssertionMap},
    Model,
};

#[derive(Debug, Clone, Default)]
pub struct DefaultModel {
    pub(crate) model: BTreeMap<String, AssertionMap>,
}

impl DefaultModel {
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(s: &str) -> Result<DefaultModel> {
        let cfg = Config::from_str(s)?;

        let mut model = DefaultModel::default();
        model.load_section(&cfg, "r")?;

        Ok(model)
    }

    fn load_section(&mut self, cfg: &Config, sec: &str) -> Result<()> {
        let mut i = 1;

        loop {
            if !self.load_assertion(cfg, sec, &format!("{}{}", sec, self.get_key_suffix(i)))? {
                break Ok(());
            } else {
                i += 1;
            }
        }
    }

    fn load_assertion(&mut self, cfg: &Config, sec: &str, key: &str) -> Result<bool> {
        let sec_name = match sec {
            "r" => "request_definition",
            "p" => "policy_definition",
            "g" => "role_definition",
            "e" => "policy_effect",
            "m" => "matchers",
            _ => {
                return Err(ModelError::Other(format!("Unknown section: `{}`", sec)).into());
            }
        };

        if let Some(val) = cfg.get_str(&format!("{}::{}", sec_name, key)) {
            Ok(self.add_def(sec, key, val))
        } else {
            Ok(false)
        }
    }

    fn get_key_suffix(&self, i: u64) -> String {
        if i == 1 {
            "".into()
        } else {
            format!("{}", i)
        }
    }
}

impl Model for DefaultModel {
    fn add_def(&mut self, sec: &str, key: &str, value: &str) -> bool {
        let mut ast = Assertion {
            key: key.into(),
            value: remove_comment(value),
            ..Default::default()
        };

        if ast.value.is_empty() {
            return false;
        }

        if sec == "r" || sec == "p" {
            ast.tokens = ast
                .value
                .split(',')
                .map(|x| format!("{}_{}", key, x.trim()))
                .collect();
        } else {
            ast.value = escape_assertion(&ast.value);
        }

        if let Some(new_model) = self.model.get_mut(sec) {
            new_model.insert(key.into(), ast);
        } else {
            let mut new_ast_map = LinkedHashMap::new();
            new_ast_map.insert(key.into(), ast);
            self.model.insert(sec.into(), new_ast_map);
        }

        true
    }
}
