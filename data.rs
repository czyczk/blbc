use core::iter::FromIterator;
use ink::codegen::{EmitEvent, Env};
use ink_env::AccountId;
use ink_env::hash::Sha2x256;
use ink_lang as ink;
use ink_prelude::format;
use ink_prelude::string::String;
use ink_prelude::vec::Vec;

use crate::{
    blbc::{Blbc, ResourceCreated, EVENT_ID_FOR_RETURNED_VALUE},
    model::{
        data::{PlainData, EncryptedData, OffchainData, ResMetadataStored},
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
    if ctx.res_metadata_map.get(resource_id).is_some() {
        return Err(format!("资源 ID '{}' 已被占用", resource_id));
    }

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
    ctx.resource_ids.push(resource_id.clone());
    ctx.res_map.insert(resource_id.clone(), &data_bytes);
    ctx.res_metadata_map
        .insert(resource_id.clone(), &metadata_stored);

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
    if ctx.res_metadata_map.get(resource_id).is_some() {
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
    ctx.resource_ids.push(resource_id.clone());
    ctx.res_map.insert(resource_id.clone(), &data_bytes);
    ctx.res_key_map.insert(resource_id.clone(), &key_decoded);
    ctx.res_policy_map.insert(resource_id.clone(), &encrypted_data.policy);
    ctx.res_metadata_map
        .insert(resource_id.clone(), &metadata_stored);


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
    if ctx.res_metadata_map.get(resource_id).is_some() {
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
    ctx.resource_ids.push(resource_id.clone());
    ctx.res_map.insert(resource_id.clone(), &offchain_data.cid.clone().into_bytes());
    ctx.res_key_map.insert(resource_id.clone(), &key_decoded);
    ctx.res_policy_map.insert(resource_id.clone(), &offchain_data.policy);
    ctx.res_metadata_map
        .insert(resource_id.clone(), &metadata_stored);


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
    bookmark: String,
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
    let mut i = 0;
    let mut is_found = false;
    // 书签为空表示从头查起
    if bookmark.eq("") {
        is_found = true;
    }
    for resource_id in resource_ids {
        if is_found {
            let metadata = match ctx.get_metadata(resource_id.clone()) {
                Ok(b) => b,
                Err(msg) => return Err(msg)
            };
            let is_eligible_resource_id = match is_eligible(metadata, creator, data_type.clone()) {
                Ok(b) => b,
                Err(msg) => return Err(msg)
            };
            if is_eligible_resource_id {
                eligible_ids.push(resource_id.clone());
            }
            i += 1;
        }
        if i >= page_size {
            break;
        }
        if resource_id.eq(&bookmark) {
            is_found = true;
        }
    }

    // 准备返回值
    let last_eligible_id = match eligible_ids.last() {
        None => { bookmark.clone() }
        Some(v) => { (*v).clone() }
    };
    let pagination_result = IDsWithPagination { ids: eligible_ids, bookmark: last_eligible_id };

    //panic!("{:#?}", pagination_result);
    return Ok(pagination_result);
}

// 按调用者、data_type 筛选
pub fn is_eligible(metadata: ResMetadataStored, creator: AccountId, data_type: String) -> Result<bool, String> {
    let creator_to_be_checked = metadata.creator;
    let data_type_to_be_checked = match metadata.extensions.get("dataType") {
        None => { return Err("metadata 中找不到 dataType 属性".into()); }
        Some(s) => { s }
    };

    return if creator_to_be_checked.eq(&creator) && data_type_to_be_checked.eq(&data_type) {
        Ok(true)
    } else {
        Ok(false)
    }
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
