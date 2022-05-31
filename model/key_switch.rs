extern crate alloc;
use super::{datetime::ScaleDateTimeLocal, ManualJsonfiable};
use crate::alloc::string::ToString;
use alloc::collections::BTreeMap;
use ink_env::AccountId;
use ink_prelude::format;
use ink_prelude::string::String;
use ink_storage::traits::{PackedLayout, SpreadLayout};
use scale::{Decode, Encode};

#[derive(Decode, Encode)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// KeySwitchTrigger 表示要传给链码的密文资源访问请求
pub struct KeySwitchTrigger {
    /// 资源 ID
    pub resource_id: String,
    /// 授权会话 ID。为零值时可忽略。
    pub auth_session_id: String,
    /// 访问申请者用于密钥置换的公钥（[64]byte 的 Base64 编码）
    pub key_switch_pk: String,
}

#[derive(Decode, Encode)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// KeySwitchResult 表示要传给链码的密钥置换结果
pub struct KeySwitchResult {
    /// 密钥置换会话 ID
    pub key_switch_session_id: String,
    /// 个人份额（[64]byte 的 Base64 编码）
    pub share: String,
    /// 零知识证明（[96]byte 的 Base64 编码），用于验证份额
    pub zk_proof: String,
    /// 份额生成者的密钥置换公钥（[64]byte 的 Base64 编码），用于验证份额
    pub key_switch_pk: String,
}

#[derive(Decode, Encode, PackedLayout, SpreadLayout)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// KeySwitchResultQuery 表示密钥置换的查询请求
pub struct KeySwitchResultQuery {
    /// 密钥置换会话 ID
    pub key_switch_session_id: String,
    /// 密钥置换结果的创建者公钥
    pub result_creator: AccountId,
}

#[derive(Debug, Clone, Eq, PartialEq, Decode, Encode, PackedLayout, SpreadLayout, Hash)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// KeySwitchTriggerStored 表示从链码得到的密文资源访问请求
pub struct KeySwitchTriggerStored {
    /// 密钥置换会话 ID
    pub key_switch_session_id: String,
    /// 资源 ID
    pub resource_id: String,
    /// 授权会话 ID。为零值时可忽略。
    pub auth_session_id: String,
    /// 访问申请者公钥
    pub creator: AccountId,
    /// 访问申请者用于密钥置换的公钥（[64]byte 的 Base64 编码）
    pub key_switch_pk: String,
    /// 时间戳
    pub timestamp: ScaleDateTimeLocal,
    /// 访问申请是否通过验证
    pub validation_result: bool,
}

impl ManualJsonfiable for KeySwitchTriggerStored {
    fn to_json_string(&self) -> String {
        let mut result: String = "{".into();
        result.push_str(&format!(
            "\"keySwitchSessionId\":\"{}\",",
            self.key_switch_session_id
        ));
        result.push_str(&format!("\"resourceId\":\"{}\",", self.resource_id));
        result.push_str(&format!("\"authSessionId\":\"{}\",", self.auth_session_id));
        let creator_bytes = self.creator.encode();
        // TODO: encode as Base64. Actually it should be encoded as Ss58 but I don't know how for
        // now.
        let creator_as_base64: String = base64::encode(creator_bytes);
        result.push_str(&format!("\"creator\":\"{}\",", creator_as_base64));
        result.push_str(&format!("\"keySwitchPk\":\"{}\",", self.key_switch_pk));
        result.push_str(&format!(
            "\"timestamp\":\"{}\",",
            self.timestamp.rfc3339_str,
        ));
        result.push_str(&format!("\"validationResult\":{}", self.validation_result));
        result.push_str("}");

        result
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Decode, Encode, PackedLayout, SpreadLayout)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// KeySwitchResultStored 表示从链码得到的密钥置换结果
pub struct KeySwitchResultStored {
    /// 密钥置换会话 ID
    pub key_switch_session_id: String,
    /// 个人份额（[64]byte 的 Base64 编码）
    pub share: String,
    /// 零知识证明（[96]byte 的 Base64 编码），用于验证份额
    pub zk_proof: String,
    /// 份额生成者的密钥置换公钥（[64]byte 的 Base64 编码），用于验证份额
    pub key_switch_pk: String,
    /// 密钥置换响应者的公钥
    pub creator: AccountId,
    /// 时间戳
    pub timestamp: ScaleDateTimeLocal,
}

#[derive(Debug, Clone, Eq, PartialEq, Decode, Encode, PackedLayout, SpreadLayout, Hash)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// DepartmentIdentityStored 表示由链码返回的部门身份信息
pub struct DepartmentIdentityStored {
    /// 部门类型
    pub dept_type: String,
    /// 部门级别
    pub dept_level: u8,
    /// 部门名称
    pub dept_name: String,
    /// 上级部门名称
    pub super_dept_name: String,
}

impl Into<BTreeMap<String, String>> for DepartmentIdentityStored {
    fn into(self) -> BTreeMap<String, String> {
        return BTreeMap::from([
            ("dept_type".into(), self.dept_type),
            ("dept_level".into(), self.dept_level.to_string()),
            ("dept_name".into(), self.dept_name),
            ("super_dept_name".into(), self.super_dept_name),
        ]);
    }
}
