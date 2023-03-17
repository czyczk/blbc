use ink_prelude::string::String;
use ink_storage::traits::{PackedLayout, SpreadLayout};
use scale::{Decode, Encode};

#[derive(Debug, Clone, Eq, PartialEq, Decode, Encode, PackedLayout, SpreadLayout)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// DocumentType 表示数字文档的文档类型
pub enum DocumentType {
    // DesignDocument 表示设计文档
    DesignDocument,
    // ProductionDocument 表示生产文档
    ProductionDocument,
    // TransferDocument 表示转移文档
    TransferDocument,
    // UsageDocument 表示使用文档
    UsageDocument,
    // RepairDocument 表示维修文档
    RepairDocument,
    // // OtherDocument 表示尚未存入文档类型
    // OtherDocument,
}

impl Into<String> for DocumentType {
    fn into(self) -> String {
        return match self {
            DocumentType::DesignDocument => { "DesignDocument".into() }
            DocumentType::ProductionDocument => { "ProductionDocument".into() }
            DocumentType::TransferDocument => { "TransferDocument".into() }
            DocumentType::UsageDocument => { "UsageDocument".into() }
            DocumentType::RepairDocument => { "RepairDocument".into() }
        };
    }
}
