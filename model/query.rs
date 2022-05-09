use ink_prelude::vec::Vec;
use ink_prelude::string::String;
use scale::{Decode, Encode};
use ink_storage::traits::{PackedLayout, SpreadLayout};

#[derive(Debug, Clone, Eq, PartialEq, Decode, Encode, PackedLayout, SpreadLayout)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// IDsWithPagination 结构体用于封装 ID 列表和书签，其中 ID 列表可用于资源 ID、授权会话 ID 等。
pub struct IDsWithPagination {
    // ID 列表
    pub ids: Vec<String>,
    // 标识分页终点的书签
    pub bookmark: String,
}