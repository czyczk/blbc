// 使用 `alloc` 是因为似乎不能在嵌套类中使用 `HashMap`（`Encode` trait 不满足）。作为权变，使用 `alloc::collections::BTreeMap`。
extern crate alloc;
use alloc::collections::BTreeMap;
use ink_env::AccountId;
use ink_prelude::string::String;
use ink_storage::traits::{PackedLayout, SpreadLayout};
use scale::{Decode, Encode};

use super::datetime::ScaleDateTimeLocal;

#[derive(Debug, Clone, Eq, PartialEq, Decode, Encode, PackedLayout, SpreadLayout)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// ResourceType 用于标志一个资源的加密类别
pub enum ResourceType {
    /// Plain 表示资源加密类别为“明文”。
    Plain,
    /// Encrypted 表示资源加密类别为“加密”。需要经由密钥置换流程来解密。
    Encrypted,
    /// Offchain 表示资源加密类别为“链下”。数据本身需要经由密钥置换流程来解密。
    Offchain,
}

#[derive(Debug, Clone, Eq, PartialEq, Decode, Encode, PackedLayout, SpreadLayout)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// ResMetadata 包含要传入链码的资源的元数据
pub struct ResMetadata {
    /// 资源加密类别
    pub resource_type: ResourceType,
    /// 资源 ID
    pub resource_id: String,
    /// 资源的明文的哈希值（[u8; 32] 的 Base64 编码）
    pub hash: String,
    /// 资源的明文的内容部分的大小
    pub size: u64,
    /// 扩展字段（包含可公开的属性）
    pub extensions: BTreeMap<String, String>,
}

#[derive(Debug, Clone, Eq, PartialEq, Decode, Encode, PackedLayout, SpreadLayout)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// ResMetadataStored 包含从链码中读出的资源的元数据
pub struct ResMetadataStored {
    /// 资源加密类别
    pub resource_type: ResourceType,
    /// 资源 ID
    pub resource_id: String,
    /// 资源的明文的哈希值（[u8; 32] 的 Base64 编码）
    pub hash: String,
    /// 资源的明文的大小
    pub size: u64,
    /// 扩展字段
    pub extensions: BTreeMap<String, String>,
    /// 资源创建者地址
    pub creator: AccountId,
    /// 时间戳
    pub timestamp: ScaleDateTimeLocal,
    /// 所包含的区块
    pub block_number: u32,
    /// 上传的密文的哈希值，由链码计算。明文时应与 `hash` 有相同值。
    pub hash_stored: String,
    /// 上传的密文的大小，由链码确定。明文时应与 `size` 有相同值。
    pub size_stored: u64,
}

#[derive(Decode, Encode)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// PlainData 用于表示要传入链码的明文资源
pub struct PlainData {
    /// 资源的元数据
    pub metadata: ResMetadata,
    /// 资源的数据本体（Base64 编码）
    pub data: String,
}
#[derive(Decode, Encode)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// EncryptedData 用于表示要传入链码的密文资源
pub struct EncryptedData {
    /// 资源的元数据
    pub metadata: ResMetadata,
    /// 资源的数据本体（密文）（Base64 编码）
    pub data: String,
    /// 对称密钥（密文）（Base64 编码）
    pub key: String,
    /// 策略
    pub policy: String,
}
#[derive(Decode, Encode)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// OffchainData 用于表示要传入链码的链下资源
pub struct OffchainData {
    /// 资源的元数据
    pub metadata: ResMetadata,
    /// 资源在 IPFS 网络上的内容 ID
    pub cid: String,
    /// 对称密钥（密文）（Base64 编码）
    pub key: String,
    /// 策略
    pub policy: String,
}
