use ink_prelude::vec::Vec;
use ink_prelude::string::String;
use scale::{Decode, Encode};
use ink_storage::traits::{PackedLayout, SpreadLayout};
use crate::model::datetime::ScaleDateTimeLocal;
use crate::model::document::DocumentType;

#[derive(Debug, Clone, Eq, PartialEq, Decode, Encode, PackedLayout, SpreadLayout)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
/// IDsWithPagination 结构体用于封装 ID 列表和书签，其中 ID 列表可用于资源 ID、授权会话 ID 等。
pub struct IDsWithPagination {
    // ID 列表
    pub ids: Vec<String>,
    // 标识分页终点的书签
    pub bookmark: String,
}


#[derive(Debug, Clone, Eq, PartialEq, Decode, Encode, PackedLayout, SpreadLayout)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
// CommonQueryConditions 表示适用所有通用模型的查询条件。不单独使用，用于组合于其他查询条件。
pub struct CommonQueryConditions {
    pub is_desc: bool,
    pub resource_id: Option<String>,
    pub is_name_exact: Option<bool>,
    // 名称是否为精确名称。`None` 表示条件不启用。`true` 为精确名称，`false` 为部分名称（名称关键字）。该字段若不为 `None` 则 `Name` 字段不可为 `None`。
    pub name: Option<String>,
    pub is_time_exact: Option<bool>,
    // 时间是否为精确时间。`None` 表示条件不启用。`true` 为精确时间，`false` 为时间范围。该字段若不为 `None` 则相应的时间字段（精确时间字段或范围时间字段）不可为 `None`。
    pub time: Option<ScaleDateTimeLocal>,
    pub time_after_inclusive: Option<ScaleDateTimeLocal>,
    // 时间条件启用时，`TimeAfterInclusive` 和 `TimeBeforeExclusive` 不可全为 `None`。
    pub time_before_exclusive: Option<ScaleDateTimeLocal>,
    // 时间条件启用时，`TimeAfterInclusive` 和 `TimeBeforeExclusive` 不可全为 `None`。
    pub last_resource_id: Option<String>,    // 上一次查询最后的资源 ID
}

#[derive(Debug, Clone, Eq, PartialEq, Decode, Encode, PackedLayout, SpreadLayout)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
// DocumentQueryConditions 表示适用数字文档的查询条件。
pub struct DocumentQueryConditions {
    pub common_query_conditions: CommonQueryConditions,
    pub document_type: Option<DocumentType>,
    pub preceding_document_id: Option<String>,
    pub head_document_id: Option<String>,
    pub entity_asset_id: Option<String>,
}

#[derive(Debug, Clone, Eq, PartialEq, Decode, Encode, PackedLayout, SpreadLayout)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
// EntityAssetQueryConditions 表示适用实体资产的查询条件。
pub struct EntityAssetQueryConditions {
    pub common_query_conditions: CommonQueryConditions,
    pub design_document_id: Option<String>,
}

#[derive(Debug, Clone, Eq, PartialEq, Decode, Encode, PackedLayout, SpreadLayout)]
#[cfg_attr(feature = "std", derive(scale_info::TypeInfo))]
pub enum QueryConditions {
    DocumentQueryConditions(DocumentQueryConditions),
    EntityAssetQueryConditions(EntityAssetQueryConditions),
}