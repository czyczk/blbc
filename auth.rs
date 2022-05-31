use crate::blbc::{AuthCreated, Blbc, ResourceCreated, EVENT_ID_FOR_RETURNED_VALUE};
use crate::error_code::CODE_FORBIDDEN;
use crate::model::auth::{AuthRequest, AuthRequestStored, AuthResponse, AuthResponseStored};
use crate::model::data::ResourceType;
use crate::model::datetime::ScaleDateTimeLocal;
use crate::model::query::IDsWithPagination;
use ink::codegen::{EmitEvent, Env};
use ink_lang as ink;
use ink_prelude::format;
use ink_prelude::string::String;
use ink_prelude::vec::Vec;

pub fn create_auth_request(
    ctx: &mut Blbc,
    auth_session_id: String,
    auth_request: AuthRequest,
    event_id: Option<String>,
) -> Result<(), String> {
    ink_env::debug_println!("---");
    ink_env::debug_println!("create_auth_request()");

    // 检查资源是否存在
    let resource_id = &auth_request.resource_id;
    ink_env::debug_println!("auth_request 中的资源 ID: {}", &resource_id);
    let meta_data_stored = match ctx.res_metadata_map.get(resource_id) {
        None => {
            return Err(format!("资源 ID '{}' 不存在", resource_id));
        }
        Some(r) => r,
    };

    // 检查资源是否为明文
    if meta_data_stored.resource_type == ResourceType::Plain {
        return Err("明文不需要申请访问权".into());
    }

    // 获取创建者与时间戳
    let creator = ctx.env().caller();
    let timestamp = ctx.env().block_timestamp();
    let timestamp_as_datetime: ScaleDateTimeLocal = timestamp.into();
    ink_env::debug_println!("Datetime: {}", &timestamp_as_datetime);

    // 构建 auth_request_stored，并存储上链
    let auth_request_stored = AuthRequestStored {
        auth_session_id: auth_session_id.clone(),
        resource_id: auth_request.resource_id,
        extensions: auth_request.extensions,
        creator,
        timestamp: timestamp_as_datetime.clone(),
    };
    // 存储数据
    ink_env::debug_println!("正在存储数据");
    ctx.auth_request_map
        .insert(auth_session_id.clone(), &auth_request_stored);
    ctx.auth_session_ids.push(auth_session_id.clone());

    // 通过事件返回值
    // TODO: Temporarily use ResourceCreated for debug purpose (actually it should be AuthCreated)
    ctx.env().emit_event(ResourceCreated {
        event_id: EVENT_ID_FOR_RETURNED_VALUE.into(),
        resource_id: auth_session_id.clone(),
    });

    if let Some(event_id) = event_id {
        ctx.env().emit_event(ResourceCreated {
            event_id,
            resource_id: auth_session_id,
        });
    }

    return Ok(());
}

pub fn create_auth_response(
    ctx: &mut Blbc,
    auth_session_id: String,
    auth_response: AuthResponse,
    event_id: Option<String>,
) -> Result<(), String> {
    ink_env::debug_println!("---");
    ink_env::debug_println!("create_auth_response()");

    // 检查授权会话的请求是否存在
    let auth_request_stored = match ctx.auth_request_map.get(&auth_session_id) {
        None => {
            return Err(format!("授权会话 '{}' 不存在", auth_session_id));
        }
        Some(r) => r,
    };

    // 检查授权请求是否已经被回复
    if ctx.auth_response_map.get(&auth_session_id).is_some() {
        return Err(format!("'{}' 该授权请求已经被批复", auth_session_id));
    }

    // 由 AuthRequetStored 得到 资源 ID
    let resource_id = &auth_request_stored.resource_id;

    // 根据资源 id ，得到资源的元数据，以此检查资源是否存在
    let metadata = match ctx.res_metadata_map.get(resource_id) {
        None => {
            return Err(format!("资源 '{}' 不存在", resource_id));
        }
        Some(r) => r,
    };

    // 获取创建者与时间戳
    let creator = ctx.env().caller();
    let timestamp = ctx.env().block_timestamp();
    let timestamp_as_datetime: ScaleDateTimeLocal = timestamp.into();
    ink_env::debug_println!("Datetime: {}", &timestamp_as_datetime);

    // 检查该交易的创建者是否为资源创建者
    if creator != metadata.creator {
        return Err(CODE_FORBIDDEN.into());
    }

    // 构建 AuthResponseStored 并存储上链
    let auth_response_stored = AuthResponseStored {
        auth_session_id: auth_session_id.clone(),
        result: auth_response.result,
        extensions: auth_response.extensions,
        creator,
        timestamp: timestamp_as_datetime,
    };

    // 存储数据
    ink_env::debug_println!("正在存储数据");
    ctx.auth_response_map
        .insert(auth_session_id.clone(), &auth_response_stored);

    // 通过事件返回值
    // TODO: Temporarily use ResourceCreated for debugging. Actually it should be AuthCreated.
    ctx.env().emit_event(ResourceCreated {
        event_id: EVENT_ID_FOR_RETURNED_VALUE.into(),
        resource_id: auth_session_id.clone(),
    });
    if let Some(event_id) = event_id {
        ctx.env().emit_event(ResourceCreated {
            event_id,
            resource_id: auth_session_id,
        });
    }

    return Ok(());
}
// 获取当前调用者尚未批复的请求
pub fn list_pending_auth_session_ids_by_resource_creator(
    ctx: &mut Blbc,
    page_size: u32,
    bookmark: Option<String>,
) -> Result<IDsWithPagination, String> {
    if page_size < 1 {
        return Err("page_size 应为正整数".into());
    }
    let creator = ctx.env().caller();
    let auth_session_ids = ctx.auth_session_ids.clone();
    // 遍历并收集结果
    let mut eligible_ids = Vec::new();
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
    for auth_session_id in auth_session_ids {
        if bookmark_is_found {
            // 当前调用者的未批复的申请
            let auth_request_stored = match ctx.get_auth_request(auth_session_id.clone()) {
                Ok(b) => b,
                Err(msg) => return Err(msg),
            };
            match ctx.get_auth_response(auth_session_id.clone()) {
                Ok(_) => {}
                Err(_) => {
                    if auth_request_stored.creator.eq(&creator) {
                        eligible_ids.push(auth_session_id.clone());
                        eligible_ids_num += 1;
                    }
                }
            };
        }
        if eligible_ids_num >= page_size {
            break;
        }
        if auth_session_id.eq(&bookmark_string) {
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

pub fn list_auth_session_ids_by_requestor(
    ctx: &mut Blbc,
    page_size: u32,
    bookmark: Option<String>,
    is_latest_first: bool,
) -> Result<IDsWithPagination, String> {
    if page_size < 1 {
        return Err("page_size 应为正整数".into());
    }

    let creator = ctx.env().caller();

    // 获取全部 req 的 auth_session_id 并排序
    let mut auth_session_ids = ctx.auth_session_ids.clone();
    if is_latest_first {
        auth_session_ids.sort_unstable_by(|a, b| b.cmp(a));
    } else {
        auth_session_ids.sort_unstable();
    }

    // 遍历并收集结果
    let mut eligible_ids = Vec::new();
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
    for auth_session_id in auth_session_ids {
        if bookmark_is_found {
            let auth_request_stored = match ctx.get_auth_request(auth_session_id.clone()) {
                Ok(b) => b,
                Err(msg) => return Err(msg),
            };
            if auth_request_stored.creator.eq(&creator) {
                eligible_ids.push(auth_session_id.clone());
                eligible_ids_num += 1;
            }
        }
        if eligible_ids_num >= page_size {
            break;
        }
        if auth_session_id.eq(&bookmark_string) {
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
