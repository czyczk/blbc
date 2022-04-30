use core::iter::FromIterator;
use ink::codegen::{EmitEvent, Env};
use ink_env::hash::Sha2x256;
use ink_lang as ink;
use ink_prelude::format;
use ink_prelude::string::String;
use ink_prelude::vec::Vec;

use crate::{
    blbc::{Blbc, ResourceCreated, EVENT_ID_FOR_RETURNED_VALUE},
    model::{
        data::{PlainData, ResMetadataStored},
        datetime::ScaleDateTimeLocal,
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
