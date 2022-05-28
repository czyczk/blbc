extern crate alloc;

use ink_prelude::boxed::Box;
use ink_prelude::format;
use ink_prelude::string::String;
use ink_prelude::vec::Vec;
use rhaink::engine::Engine;
use rhaink::{Dynamic, Scope};

use super::adapter::Adapter;
use super::core_api::CoreApi;
use super::effector::{EffectKind, Effector};
use super::error::{ModelError, PolicyError, RequestError, RhaiEvalAltResultError};
use super::model::Model;
use super::util::escape_eval;
use crate::casbink::Result;
use crate::get_or_err;

// Enforcer is the main interface for authorization enforcement and policy management.
pub struct Enforcer {
    model: Box<dyn Model>,
    adapter: Box<dyn Adapter>,
    eft: Box<dyn Effector>,
    enabled: bool,
    auto_save: bool,
    auto_build_role_links: bool,
    engine: Engine,
}

impl Enforcer {
    pub(crate) fn private_enforce(&self, rvals: &[Dynamic]) -> Result<(bool, Option<Vec<usize>>)> {
        if !self.enabled {
            return Ok((true, None));
        }

        let mut scope: Scope = Scope::new();

        let r_ast = get_or_err!(self, "r", ModelError::R, "request");
        let p_ast = get_or_err!(self, "p", ModelError::P, "policy");
        let m_ast = get_or_err!(self, "m", ModelError::M, "matcher");
        let e_ast = get_or_err!(self, "e", ModelError::E, "effector");

        if r_ast.tokens.len() != rvals.len() {
            return Err(
                RequestError::UnmatchRequestDefinition(r_ast.tokens.len(), rvals.len()).into(),
            );
        }

        for (rtoken, rval) in r_ast.tokens.iter().zip(rvals.iter()) {
            scope.push_constant_dynamic(rtoken, rval.to_owned());
        }

        let policies = p_ast.get_policy();
        let (policy_len, scope_len) = (policies.len(), scope.len());

        let mut eft_stream = self.eft.new_stream(&e_ast.value, policy_len.max(1));
        let m_ast_compiled = self
            .engine
            .compile_expression(&escape_eval(&m_ast.value))
            .map_err(|e| RhaiEvalAltResultError::EvalAltResult(format!("{}", e)))?;

        if policy_len == 0 {
            for token in p_ast.tokens.iter() {
                scope.push_constant(token, String::new());
            }

            let eval_result = self
                .engine
                .eval_ast_with_scope::<bool>(&mut scope, &m_ast_compiled)
                .map_err(|e| RhaiEvalAltResultError::EvalAltResult(format!("{}", e)))?;
            let eft = if eval_result {
                EffectKind::Allow
            } else {
                EffectKind::Indeterminate
            };

            eft_stream.push_effect(eft);

            return Ok((eft_stream.next(), None));
        }

        for pvals in policies {
            scope.rewind(scope_len);

            if p_ast.tokens.len() != pvals.len() {
                return Err(
                    PolicyError::UnmatchPolicyDefinition(p_ast.tokens.len(), pvals.len()).into(),
                );
            }
            for (ptoken, pval) in p_ast.tokens.iter().zip(pvals.iter()) {
                scope.push_constant(ptoken, pval.to_owned());
            }

            let eval_result = self
                .engine
                .eval_ast_with_scope::<bool>(&mut scope, &m_ast_compiled)
                .map_err(|e| RhaiEvalAltResultError::EvalAltResult(format!("{}", e)))?;
            let eft = match p_ast.tokens.iter().position(|x| x == "p_eft") {
                Some(j) if eval_result => {
                    let p_eft = &pvals[j];
                    if p_eft == "deny" {
                        EffectKind::Deny
                    } else if p_eft == "allow" {
                        EffectKind::Allow
                    } else {
                        EffectKind::Indeterminate
                    }
                }
                None if eval_result => EffectKind::Allow,
                _ => EffectKind::Indeterminate,
            };

            if eft_stream.push_effect(eft) {
                break;
            }
        }

        Ok((eft_stream.next(), {
            {
                eft_stream.explain()
            }
        }))
    }
}

impl CoreApi for Enforcer {
    #[inline]
    fn get_model(&self) -> &dyn Model {
        &*self.model
    }
}
