// 使用 `alloc` 是因为似乎不能在嵌套类中使用 `HashMap`（`Encode` trait 不满足）。作为权变，使用 `alloc::collections::BTreeMap`。
extern crate alloc;
use alloc::collections::BTreeMap;
use ink_prelude::string::String;
use ink_env::AccountId;
use super::datetime::ScaleDateTimeLocal;
use ink_storage::traits::{PackedLayout, SpreadLayout};
use scale::{Decode, Encode};


#[derive(Debug, Clone, Eq, PartialEq, Decode, Encode, PackedLayout, SpreadLayout)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// AuthRequest 表示传给链码的访问权申请请求
pub struct AuthRequest {
    /// 资源 ID
    pub resource_id: String,
    /// 扩展字段
    pub extensions: BTreeMap<String, String>,
}
#[derive(Debug, Clone, Eq, PartialEq, Decode, Encode, PackedLayout, SpreadLayout)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// AuthResponse 表示传给链码的访问申请批复
pub struct AuthResponse {
    /// 访问权申请会话 ID
    pub auth_session_id: String,
    /// 访问权批复结果
    pub result: bool,
    /// 扩展字段
    pub extensions: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Eq, PartialEq, Decode, Encode, PackedLayout, SpreadLayout)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// AuthRequestStored 表示从链码得到的访问权申请请求
pub struct AuthRequestStored {
    /// 授权会话 ID
    pub auth_session_id: String,
    /// 资源 ID
    pub resource_id: String,
    /// 扩展字段
    pub extensions: BTreeMap<String, String>,
    /// 访问权申请者公钥（Base64 编码）
    pub creator: AccountId,
    /// 时间戳
    pub timestamp: ScaleDateTimeLocal,
}
#[derive(Debug, Clone, Eq, PartialEq, Decode, Encode, PackedLayout, SpreadLayout)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// AuthResponseStored 表示从链码得到的访问申请批复
pub struct AuthResponseStored {
    /// 访问权申请会话 ID
    pub auth_session_id: String,
    /// 访问权批复结果
    pub result: bool,
    /// 扩展字段
    pub extensions: BTreeMap<String, String>,
    /// 访问权批复者公钥（Base64 编码）
    pub creator: AccountId,
    /// 时间戳
    pub timestamp: ScaleDateTimeLocal,
}
