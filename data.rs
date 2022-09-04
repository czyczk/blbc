use core::iter::FromIterator;
use ink::codegen::{EmitEvent, Env};
use ink_env::hash::Sha2x256;
use ink_env::AccountId;
use ink_lang as ink;
use ink_prelude::format;
use ink_prelude::string::String;
use ink_prelude::vec::Vec;

use crate::model::query::{DocumentQueryConditions, EntityAssetQueryConditions, QueryConditions};
use crate::{
    blbc::{Blbc, ResourceCreated, EVENT_ID_FOR_RETURNED_VALUE},
    model::{
        data::{EncryptedData, OffchainData, PlainData, ResMetadataStored},
        datetime::ScaleDateTimeLocal,
        query::IDsWithPagination,
    },
};

pub fn create_plain_data(
    ctx: &mut Blbc,
    plain_data: PlainData,
    event_id: Option<String>,
) -> Result<(), String> {
    ink_env::debug_println!("---");
    ink_env::debug_println!("create_plain_data()");

    // 检查资源 ID 是否被占用
    let resource_id = &plain_data.metadata.resource_id;
    ink_env::debug_println!("收到资源 ID: {}", &resource_id);
    if ctx.resource_ids.contains(resource_id) {
        return Err(format!("资源 ID '{}' 已被占用", resource_id));
    }
    // if ctx.res_metadata_map.get(resource_id).is_some() {
    //     return Err(format!("资源 ID '{}' 已被占用", resource_id));
    // }

    // 将数据本体从 Base64 解码
    ink_env::debug_println!("正在从 Base64 解码");
    let data_bytes = match base64::decode(plain_data.data) {
        Ok(b) => Vec::from_iter(b),
        Err(err) => return Err(format!("无法解析数据本体: {}", err)),
    };

    // 计算哈希和大小并检查是否与用户提供的值相同
    ink_env::debug_println!("正在检查哈希与大小");
    let size_stored = data_bytes.len() as u64;
    if size_stored != plain_data.metadata.size {
        return Err(format!(
            "大小不匹配，应有大小为 {}，实际大小为 {}",
            plain_data.metadata.size, size_stored
        ));
    }

    let hash_stored = ctx.env().hash_bytes::<Sha2x256>(&data_bytes);
    let hash_stored_base64 = base64::encode(hash_stored);
    if hash_stored_base64 != plain_data.metadata.hash {
        return Err("哈希不匹配".into());
    }

    // 获取创建者与时间戳
    let creator = ctx.env().caller();
    let timestamp = ctx.env().block_timestamp();
    let timestamp_as_datetime: ScaleDateTimeLocal = timestamp.into();
    ink_env::debug_println!("Datetime: {}", &timestamp_as_datetime);

    // 准备存储元数据
    ink_env::debug_println!("正在组装元数据结构体");
    let metadata_stored = ResMetadataStored {
        resource_type: plain_data.metadata.resource_type,
        resource_id: resource_id.clone(),
        hash: plain_data.metadata.hash,
        size: plain_data.metadata.size,
        extensions: plain_data.metadata.extensions,
        creator,
        timestamp: timestamp_as_datetime,
        block_number: ctx.env().block_number(),
        hash_stored: hash_stored_base64,
        size_stored,
    };

    // 存储数据
    ink_env::debug_println!("正在存储数据");
    ctx.res_map.insert(resource_id.clone(), &data_bytes);
    ctx.res_metadata_map
        .insert(resource_id.clone(), &metadata_stored);
    ctx.resource_ids.push(resource_id.clone());

    // 通过事件返回值
    ctx.env().emit_event(ResourceCreated {
        event_id: EVENT_ID_FOR_RETURNED_VALUE.into(),
        resource_id: resource_id.clone(),
    });

    if let Some(event_id) = event_id {
        ctx.env().emit_event(ResourceCreated {
            event_id,
            resource_id: resource_id.clone(),
        });
    }

    return Ok(());
}

pub fn create_encrypted_data(
    ctx: &mut Blbc,
    encrypted_data: EncryptedData,
    event_id: Option<String>,
) -> Result<(), String> {
    // 检查资源 ID 是否被占用
    let resource_id = &encrypted_data.metadata.resource_id;
    ink_env::debug_println!("收到资源 ID: {}", &resource_id);
    if ctx.resource_ids.contains(resource_id) {
        return Err(format!("资源 ID '{}' 已被占用", resource_id));
    }

    // 将数据本体(密文)从 Base64 解码
    ink_env::debug_println!("正在从 Base64 解码");
    let data_bytes = match base64::decode(encrypted_data.data) {
        Ok(b) => Vec::from_iter(b),
        Err(err) => return Err(format!("无法解析数据本体: {}", err)),
    };

    // 计算解码后的数据本体的哈希和大小
    let size_stored = data_bytes.len() as u64;
    let hash_stored = ctx.env().hash_bytes::<Sha2x256>(&data_bytes);
    let hash_stored_base64 = base64::encode(hash_stored);

    // 获取创建者与时间戳
    let creator = ctx.env().caller();
    let timestamp = ctx.env().block_timestamp();
    let timestamp_as_datetime: ScaleDateTimeLocal = timestamp.into();
    ink_env::debug_println!("Datetime: {}", &timestamp_as_datetime);

    // 准备存储元数据
    ink_env::debug_println!("正在组装元数据结构体");
    let metadata_stored = ResMetadataStored {
        resource_type: encrypted_data.metadata.resource_type,
        resource_id: resource_id.clone(),
        hash: encrypted_data.metadata.hash,
        size: encrypted_data.metadata.size,
        extensions: encrypted_data.metadata.extensions,
        creator,
        timestamp: timestamp_as_datetime,
        block_number: ctx.env().block_number(),
        hash_stored: hash_stored_base64,
        size_stored,
    };

    // 将 key 从 Base64 解码
    ink_env::debug_println!("正在从 Base64 解码");
    let key_decoded = match base64::decode(encrypted_data.key) {
        Ok(b) => Vec::from_iter(b),
        Err(err) => return Err(format!("无法解析 key: {}", err)),
    };

    // 存储数据
    ink_env::debug_println!("正在存储数据");
    ctx.res_map.insert(resource_id.clone(), &data_bytes);
    ctx.res_key_map.insert(resource_id.clone(), &key_decoded);
    ctx.res_policy_map
        .insert(resource_id.clone(), &encrypted_data.policy);
    ctx.res_metadata_map
        .insert(resource_id.clone(), &metadata_stored);
    ctx.resource_ids.push(resource_id.clone());

    // 通过事件返回值
    ctx.env().emit_event(ResourceCreated {
        event_id: EVENT_ID_FOR_RETURNED_VALUE.into(),
        resource_id: resource_id.clone(),
    });

    if let Some(event_id) = event_id {
        ctx.env().emit_event(ResourceCreated {
            event_id,
            resource_id: resource_id.clone(),
        });
    }

    return Ok(());
}

pub fn create_offchain_data(
    ctx: &mut Blbc,
    offchain_data: OffchainData,
    event_id: Option<String>,
) -> Result<(), String> {
    // 检查资源 ID 是否被占用
    let resource_id = &offchain_data.metadata.resource_id;
    ink_env::debug_println!("收到资源 ID: {}", &resource_id);
    if ctx.resource_ids.contains(resource_id) {
        return Err(format!("资源 ID '{}' 已被占用", resource_id));
    }

    // CID 不可为空
    if offchain_data.cid.is_empty() {
        return Err(format!("资源 cid 不可为空"));
    }

    // 计算存储的哈希与大小
    let size_stored = offchain_data.cid.len() as u64;
    let cid = offchain_data.cid.clone();
    let cid_bytes = cid.into_bytes();
    let hash_stored = ctx.env().hash_bytes::<Sha2x256>(&cid_bytes);
    let hash_stored_base64 = base64::encode(hash_stored);

    // 获取创建者与时间戳
    let creator = ctx.env().caller();
    let timestamp = ctx.env().block_timestamp();
    let timestamp_as_datetime: ScaleDateTimeLocal = timestamp.into();
    ink_env::debug_println!("Datetime: {}", &timestamp_as_datetime);

    // 准备存储元数据
    ink_env::debug_println!("正在组装元数据结构体");
    let metadata_stored = ResMetadataStored {
        resource_type: offchain_data.metadata.resource_type,
        resource_id: resource_id.clone(),
        hash: offchain_data.metadata.hash,
        size: offchain_data.metadata.size,
        extensions: offchain_data.metadata.extensions,
        creator,
        timestamp: timestamp_as_datetime,
        block_number: ctx.env().block_number(),
        hash_stored: hash_stored_base64,
        size_stored,
    };

    // 将 key 从 Base64 解码
    ink_env::debug_println!("正在从 Base64 解码");
    let key_decoded = match base64::decode(&offchain_data.key) {
        Ok(b) => Vec::from_iter(b),
        Err(err) => return Err(format!("无法解析 key: {}", err)),
    };

    // 存储数据
    ink_env::debug_println!("正在存储数据");
    ctx.res_map
        .insert(resource_id.clone(), &offchain_data.cid.clone().into_bytes());
    ctx.res_key_map.insert(resource_id.clone(), &key_decoded);
    ctx.res_policy_map
        .insert(resource_id.clone(), &offchain_data.policy);
    ctx.res_metadata_map
        .insert(resource_id.clone(), &metadata_stored);
    ctx.resource_ids.push(resource_id.clone());

    // 通过事件返回值
    ctx.env().emit_event(ResourceCreated {
        event_id: EVENT_ID_FOR_RETURNED_VALUE.into(),
        resource_id: resource_id.clone(),
    });

    if let Some(event_id) = event_id {
        ctx.env().emit_event(ResourceCreated {
            event_id,
            resource_id: resource_id.clone(),
        });
    }

    return Ok(());
}

pub fn list_resource_ids_by_creator(
    ctx: &mut Blbc,
    data_type: String,
    is_desc: bool,
    page_size: u64,
    bookmark: Option<String>,
) -> Result<IDsWithPagination, String> {
    if page_size < 1 {
        return Err("page_size 应为正整数".into());
    }

    // 获取当前调用者
    let creator = ctx.env().caller();

    // 获取全部 resource_id 并排序
    let mut resource_ids = ctx.resource_ids.clone();
    if is_desc {
        resource_ids.sort_unstable_by(|a, b| b.cmp(a));
    } else {
        resource_ids.sort_unstable();
    }

    // 遍历并收集结果
    let mut eligible_ids: Vec<String> = Vec::new();
    let mut eligible_ids_num = 0;
    let mut bookmark_is_found = false;
    let bookmark_string: String;
    // 书签为空表示从头查起
    match bookmark {
        None => {
            bookmark_string = "".into();
            bookmark_is_found = true;
        }
        Some(s) => {
            bookmark_string = s.clone();
            if s.chars().count() == 0 {
                bookmark_is_found = true;
            }
        }
    }
    for resource_id in resource_ids {
        if bookmark_is_found {
            let metadata = match ctx.get_metadata(resource_id.clone()) {
                Ok(b) => b,
                Err(msg) => return Err(msg),
            };
            let is_eligible_resource_id = match is_eligible(metadata, creator, data_type.clone()) {
                Ok(b) => b,
                Err(msg) => return Err(msg),
            };
            if is_eligible_resource_id {
                eligible_ids.push(resource_id.clone());
                eligible_ids_num += 1;
            }
        }
        if eligible_ids_num >= page_size {
            break;
        }
        if resource_id.eq(&bookmark_string) {
            bookmark_is_found = true;
        }
    }

    // 准备返回值
    let last_eligible_id = match eligible_ids.last() {
        None => bookmark_string.clone(),
        Some(v) => (*v).clone(),
    };
    let pagination_result = IDsWithPagination {
        ids: eligible_ids,
        bookmark: last_eligible_id,
    };
    // 测试时打开注释用来输出信息到命令行
    //panic!("{:#?}", pagination_result);
    return Ok(pagination_result);
}

pub fn list_resource_ids_by_conditions(
    ctx: &mut Blbc,
    query_conditions: QueryConditions,
    page_size: u64,
) -> Result<IDsWithPagination, String> {
    ink_env::debug_println!(
        "list_resource_ids_by_conditions 查询参数：{:?}",
        query_conditions
    );
    // 检查与准备变量
    if page_size < 1 {
        return Err("page_size 应为正整数".into());
    }
    let resource_ids = ctx.resource_ids.clone();
    let mut eligible_ids: Vec<String> = Vec::new();
    let bookmark_string: String;

    // 分别处理 QueryConditions::DocumentQueryConditions 与 QueryConditions::EntityAssetQueryConditions
    match query_conditions {
        QueryConditions::DocumentQueryConditions(document_query_conditions) => {

            // 获取全部文档 resource_id 并排序
            let mut document_resource_ids = match get_resource_ids_by_category(ctx, resource_ids.clone(), "Document".into()) {
                Ok(t) => {t}
                Err(msg) => return Err(msg),
            };
            if document_query_conditions.common_query_conditions.is_desc {
                document_resource_ids.sort_unstable_by(|a, b| b.cmp(a));
            } else {
                document_resource_ids.sort_unstable();
            }

            // 书签为空表示从头查起
            let mut bookmark_is_found = false;
            match &document_query_conditions
                .common_query_conditions
                .last_resource_id
            {
                Some(last_resource_id) => {
                    bookmark_string = last_resource_id.clone();
                    if bookmark_string.is_empty() {
                        bookmark_is_found = true;
                    }
                }
                None => {
                    bookmark_string = "".into();
                    bookmark_is_found = true;
                }
            }

            // 遍历并收集结果
            let mut eligible_ids_num = 0;
            for resource_id in document_resource_ids {
                if bookmark_is_found {
                    let metadata = match ctx.get_metadata(resource_id.clone()) {
                        Ok(b) => b,
                        Err(msg) => return Err(msg),
                    };
                    match meet_document_query_conditions(
                        metadata,
                        document_query_conditions.clone(),
                    ) {
                        Ok(true) => {
                            eligible_ids.push(resource_id.clone());
                            eligible_ids_num += 1;
                        }
                        Ok(false) => {}
                        Err(msg) => return Err(msg),
                    }
                }
                if eligible_ids_num >= page_size {
                    break;
                }
                if resource_id.eq(&bookmark_string) {
                    bookmark_is_found = true;
                }
            }
        }
        QueryConditions::EntityAssetQueryConditions(entity_asset_query_conditions) => {

            // 获取全部资产 resource_id 并排序
            let mut entity_asset_resource_ids = match get_resource_ids_by_category(ctx, resource_ids.clone(), "EntityAsset".into()) {
                Ok(t) => {t}
                Err(msg) => return Err(msg),
            };
            if entity_asset_query_conditions
                .common_query_conditions
                .is_desc
            {
                entity_asset_resource_ids.sort_unstable_by(|a, b| b.cmp(a));
            } else {
                entity_asset_resource_ids.sort_unstable();
            }

            // 书签为空表示从头查起
            let mut bookmark_is_found = false;
            match &entity_asset_query_conditions
                .common_query_conditions
                .last_resource_id
            {
                Some(last_resource_id) => {
                    bookmark_string = last_resource_id.clone();
                    if bookmark_string.is_empty() {
                        bookmark_is_found = true;
                    }
                }
                None => {
                    bookmark_string = "".into();
                    bookmark_is_found = true;
                }
            }
            // 遍历并收集结果
            let mut eligible_ids_num = 0;
            for resource_id in entity_asset_resource_ids {
                if bookmark_is_found {
                    let metadata = match ctx.get_metadata(resource_id.clone()) {
                        Ok(b) => b,
                        Err(msg) => return Err(msg),
                    };
                    match meet_entity_asset_query_conditions(
                        metadata,
                        entity_asset_query_conditions.clone(),
                    ) {
                        Ok(true) => {
                            eligible_ids.push(resource_id.clone());
                            eligible_ids_num += 1;
                        }
                        Ok(false) => {}
                        Err(msg) => return Err(msg),
                    }
                }
                if eligible_ids_num >= page_size {
                    break;
                }
                if resource_id.eq(&bookmark_string) {
                    bookmark_is_found = true;
                }
            }
        }
    };
    // 准备返回值
    let last_eligible_id = match eligible_ids.last() {
        None => bookmark_string.clone(),
        Some(v) => (*v).clone(),
    };
    let pagination_result = IDsWithPagination {
        ids: eligible_ids,
        bookmark: last_eligible_id,
    };
    // 测试时打开注释用来输出信息到命令行
    //panic!("{:#?}", pagination_result);
    return Ok(pagination_result);
}

fn meet_document_query_conditions(
    metadata: ResMetadataStored,
    document_query_conditions: DocumentQueryConditions,
) -> Result<bool, String> {
    // 先匹配 common_query_conditions 之外的一系列条件
    match document_query_conditions.document_type {
        None => {}
        Some(document_type) => {
            let document_type_to_be_checked = match metadata.extensions.get("documentType") {
                None => {
                    return Err("metadata 中找不到 documentType 属性".into());
                }
                Some(s) => s,
            };
            let document_type_to_be_checked_string = (*document_type_to_be_checked).clone();
            let document_type_string: String = document_type.into();
            if document_type_to_be_checked_string.ne(&document_type_string) {
                return Ok(false);
            }
        }
    };
    match document_query_conditions.preceding_document_id {
        None => {}
        Some(preceding_document_id) => {
            let preceding_document_id_to_be_checked =
                match metadata.extensions.get("precedingDocumentId") {
                    None => {
                        return Err("metadata 中找不到 precedingDocumentId 属性".into());
                    }
                    Some(s) => s,
                };
            if (*preceding_document_id_to_be_checked).ne(&preceding_document_id) {
                return Ok(false);
            }
        }
    };
    match document_query_conditions.head_document_id {
        None => {}
        Some(head_document_id) => {
            let head_document_id_to_be_checked = match metadata.extensions.get("headDocumentId") {
                None => {
                    return Err("metadata 中找不到 headDocumentId 属性".into());
                }
                Some(s) => s,
            };
            if (*head_document_id_to_be_checked).ne(&head_document_id) {
                return Ok(false);
            }
        }
    };
    match document_query_conditions.entity_asset_id {
        None => {}
        Some(entity_asset_id) => {
            let entity_asset_id_to_be_checked = match metadata.extensions.get("entityAssetId") {
                None => {
                    return Err("metadata 中找不到 entityAssetId 属性".into());
                }
                Some(s) => s,
            };
            if (*entity_asset_id_to_be_checked).ne(&entity_asset_id) {
                return Ok(false);
            }
        }
    };
    // 匹配 common_query_conditions 中的一系列条件
    let common_query_conditions = document_query_conditions.common_query_conditions;
    match common_query_conditions.resource_id {
        None => {}
        Some(resource_id) => {
            if resource_id.ne(&(metadata.resource_id)) {
                return Ok(false);
            }
        }
    };
    match common_query_conditions.is_name_exact {
        None => {}
        Some(true) => {
            // 按精确名称查找的逻辑
            let name = match common_query_conditions.name {
                None => {
                    return Err("查询条件缺少 name 字段".into());
                }
                Some(s) => s,
            };
            let name_to_be_checked = match metadata.extensions.get("name") {
                None => {
                    return Err("metadata 中找不到 name 属性".into());
                }
                Some(s) => s,
            };
            if (*name_to_be_checked).ne(&name) {
                return Ok(false);
            }
        }
        Some(false) => {
            // 模糊查找
            let name = match common_query_conditions.name {
                None => {
                    return Err("查询条件缺少 name 字段".into());
                }
                Some(s) => s,
            };
            let name_to_be_checked = match metadata.extensions.get("name") {
                None => {
                    return Err("metadata 中找不到 name 属性".into());
                }
                Some(s) => s,
            };
            if !(*name_to_be_checked).contains(&name) {
                return Ok(false);
            }
        }
    };
    match common_query_conditions.is_time_exact {
        None => {}
        Some(true) => {
            // 查找指定时间点
            let time = match common_query_conditions.time {
                None => {
                    return Err("查询条件缺少 time 字段".into());
                }
                Some(s) => s,
            };
            let time_to_be_checked = metadata.timestamp;
            if time.ne(&time_to_be_checked) {
                return Ok(false);
            }
        }
        Some(false) => {
            //查找指定时间段: [after, before) 或 [after, 至今]、[起始时间, before)
            let time_to_be_checked = metadata.timestamp;
            if common_query_conditions.time_after_inclusive.is_none()
                && common_query_conditions.time_before_exclusive.is_none()
            {
                return Err(
                    "time_after_inclusive、time_before_exclusive 至少应有一个不为 None".into(),
                );
            } else if common_query_conditions.time_after_inclusive.is_some()
                && common_query_conditions.time_before_exclusive.is_some()
            {
                let time_after_inclusive = common_query_conditions.time_after_inclusive.unwrap();
                let time_before_exclusive = common_query_conditions.time_before_exclusive.unwrap();
                if time_to_be_checked.lt(&time_after_inclusive)
                    || time_to_be_checked.ge(&time_before_exclusive)
                {
                    return Ok(false);
                }
            } else {
                // 二者有且仅有之一有值时
                match common_query_conditions.time_after_inclusive {
                    None => {}
                    Some(t) => {
                        if time_to_be_checked.lt(&t) {
                            return Ok(false);
                        }
                    }
                };
                match common_query_conditions.time_before_exclusive {
                    None => {}
                    Some(t) => {
                        if time_to_be_checked.ge(&t) {
                            return Ok(false);
                        }
                    }
                };
            }
        }
    };
    // 函数运行到此处尚未返回的表示通过了以上所有筛选
    return Ok(true);
}

fn meet_entity_asset_query_conditions(
    metadata: ResMetadataStored,
    entity_asset_query_conditions: EntityAssetQueryConditions,
) -> Result<bool, String> {
    // 先匹配 common_query_conditions 之外的一系列条件
    match entity_asset_query_conditions.design_document_id {
        None => {}
        Some(design_document_id) => {
            let design_document_id_to_be_checked = match metadata.extensions.get("designDocumentId")
            {
                None => {
                    return Err("metadata 中找不到 designDocumentId 属性".into());
                }
                Some(s) => s,
            };
            if (*design_document_id_to_be_checked).ne(&design_document_id) {
                return Ok(false);
            }
        }
    };
    // 匹配 common_query_conditions 中的一系列条件
    let common_query_conditions = entity_asset_query_conditions.common_query_conditions;
    match common_query_conditions.resource_id {
        None => {}
        Some(resource_id) => {
            if resource_id.ne(&(metadata.resource_id)) {
                return Ok(false);
            }
        }
    };
    match common_query_conditions.is_name_exact {
        None => {}
        Some(true) => {
            // 按精确名称查找的逻辑
            let name = match common_query_conditions.name {
                None => {
                    return Err("查询条件缺少 name 字段".into());
                }
                Some(s) => s,
            };
            let name_to_be_checked = match metadata.extensions.get("name") {
                None => {
                    return Err("metadata 中找不到 name 属性".into());
                }
                Some(s) => s,
            };
            if (*name_to_be_checked).ne(&name) {
                return Ok(false);
            }
        }
        Some(false) => {
            // 模糊查找
            let name = match common_query_conditions.name {
                None => {
                    return Err("查询条件缺少 name 字段".into());
                }
                Some(s) => s,
            };
            let name_to_be_checked = match metadata.extensions.get("name") {
                None => {
                    return Err("metadata 中找不到 name 属性".into());
                }
                Some(s) => s,
            };
            if !(*name_to_be_checked).contains(&name) {
                return Ok(false);
            }
        }
    };
    match common_query_conditions.is_time_exact {
        None => {}
        Some(true) => {
            // 查找指定时间点
            let time = match common_query_conditions.time {
                None => {
                    return Err("查询条件缺少 time 字段".into());
                }
                Some(s) => s,
            };
            let time_to_be_checked = metadata.timestamp;
            if time.ne(&time_to_be_checked) {
                return Ok(false);
            }
        }
        Some(false) => {
            //查找指定时间段: [after, before) 或 [after, 至今]、[起始时间, before)
            let time_to_be_checked = metadata.timestamp;
            if common_query_conditions.time_after_inclusive.is_none()
                && common_query_conditions.time_before_exclusive.is_none()
            {
                return Err(
                    "time_after_inclusive、time_before_exclusive 至少应有一个不为 None".into(),
                );
            } else if common_query_conditions.time_after_inclusive.is_some()
                && common_query_conditions.time_before_exclusive.is_some()
            {
                let time_after_inclusive = common_query_conditions.time_after_inclusive.unwrap();
                let time_before_exclusive = common_query_conditions.time_before_exclusive.unwrap();
                if time_to_be_checked.lt(&time_after_inclusive)
                    || time_to_be_checked.ge(&time_before_exclusive)
                {
                    return Ok(false);
                }
            } else {
                // 二者有且仅有之一有值时
                match common_query_conditions.time_after_inclusive {
                    None => {}
                    Some(t) => {
                        if time_to_be_checked.lt(&t) {
                            return Ok(false);
                        }
                    }
                };
                match common_query_conditions.time_before_exclusive {
                    None => {}
                    Some(t) => {
                        if time_to_be_checked.ge(&t) {
                            return Ok(false);
                        }
                    }
                };
            }
        }
    };
    // 函数运行到此处尚未返回的表示通过了以上所有筛选
    return Ok(true);
}

// 按调用者、data_type 筛选
fn is_eligible(
    metadata: ResMetadataStored,
    creator: AccountId,
    data_type: String,
) -> Result<bool, String> {
    let creator_to_be_checked = metadata.creator;
    let data_type_to_be_checked = match metadata.extensions.get("dataType") {
        None => {
            return Err("metadata 中找不到 dataType 属性".into());
        }
        Some(s) => s,
    };

    return if creator_to_be_checked.eq(&creator) && (*data_type_to_be_checked).eq(&data_type) {
        Ok(true)
    } else {
        Ok(false)
    };
}

/// 从链上全部的 resource_id 中筛选出指定类别的
fn get_resource_ids_by_category(ctx: &mut Blbc, resource_ids: Vec<String>, category: String) -> Result<Vec<String>, String> {
    let mut eligible_ids: Vec<String> = Vec::new();
    for resource_id in resource_ids {
        match ctx.get_metadata(resource_id.clone()) {
            Ok(metadata) => if metadata.extensions.get("dataType").unwrap().eq(&category){
                eligible_ids.push(resource_id)
            },
            Err(msg) => return Err(msg),
        }
    }
    return Ok(eligible_ids);
}
// fn get_datetime_local_from_timestamp(timestamp: u64) -> DateTime<Local> {
//     // Convert to timestamp in nanoseconds
//     let timestamp_in_nano = (timestamp * 1_000_000) as i64;
//     let nano_unit = 1_000_000_000i64;
//     let secs = (timestamp_in_nano / nano_unit) as i64;
//     let nsecs = (timestamp_in_nano % nano_unit) as u32;

//     // Convert to timezoned datetime through NativeDateTime
//     let naive_datetime = NaiveDateTime::from_timestamp(secs, nsecs);
//     DateTime::<Utc>::from_utc(naive_datetime, Utc).into()
// }
