extern crate alloc;
use alloc::collections::BTreeMap;
use ink_prelude::string::String;
use crate::blbc::{Blbc, EVENT_ID_FOR_RETURNED_VALUE, KSCreated, KSResultCreated};
use crate::error_code;
// use crate::casbink::model::default_model::DefaultModel;
use ink_lang as ink;
use ink::codegen::{EmitEvent, Env};
use ink_prelude::format;
use ink_prelude::vec::Vec;
use crate::model::datetime::ScaleDateTimeLocal;
use crate::model::key_switch::{DepartmentIdentityStored, KeySwitchResult, KeySwitchResultQuery, KeySwitchResultStored, KeySwitchTrigger, KeySwitchTriggerStored};

pub async fn create_key_switch_trigger(ctx: &mut Blbc, ks_session_id: String, dept_identity: DepartmentIdentityStored, ks_trigger: KeySwitchTrigger, event_id: String) -> Result<(), String> {
    // 获取 auth_session_id
    let auth_session_id = ks_trigger.auth_session_id.clone();
    let mut validation_result = false;
    // 获取 resource_id，验证资源是否存在
    let resource_id = &ks_trigger.resource_id;
    let metadata = match ctx.res_metadata_map.get(resource_id) {
        None => { return Err(format!("资源 ID '{}' 不存在", resource_id)); }
        Some(r) => { r }
    };

    // 获取创建者与时间戳
    let creator = ctx.env().caller();
    let timestamp = ctx.env().block_timestamp();
    let timestamp_as_datetime: ScaleDateTimeLocal = timestamp.into();

    // 检查用于密钥置换的公钥是否为空
    if ks_trigger.key_switch_pk.is_empty() {
        return Err("用于密钥置换的公钥未指定".into());
    }

    if !auth_session_id.is_empty() {
        // 获取 auth_request_stored，验证其中资源 ID 是否相同。若请求不存在，则另外报错。
        let auth_request_stored = match ctx.get_auth_request(auth_session_id.clone()) {
            Ok(a) => { a }
            Err(msg) => { return Err(msg); }
        };
        if auth_request_stored.resource_id != *resource_id {
            return Err("资源 ID 与授权会话 ID 不匹配".into());
        }
        // 验证 auth_request_stored.creator 是否等于链码调用者 creator
        if auth_request_stored.creator != creator{
            return Err("不是申请授权者本人".into());
        }
        // 如果 auth_session_id 不为空值，则获取 auth_response_stored。若批复不存在，则另外报错。
        let auth_response_stored = match ctx.get_auth_response(auth_session_id.clone()) {
            Ok(a) => { a }
            Err(_) => { return Err("该授权会话申请未得到批复".into()); }
        };
        // 根据 auth_response_stored 中的结果得到最终判断结果
        if auth_response_stored.result {
            validation_result = true;
        } else {
            return Err(error_code::CODE_FORBIDDEN.into());
        }
    } else {
        // 如果 authSessionID 为空值，执行 abac
        // 根据资源 ID，得到资源的访问策略
        let mut policy = match ctx.get_policy((*resource_id).clone()) {
            Ok(p) => { p }
            Err(msg) => { return Err(msg); }
        };
        // 完善访问策略
        let s = policy;
        s.replace("||", "|| r.sub.").replace("(", "(r.sub.").replace("&& ", "&& r.sub.");
        policy = String::from("m = ");
        policy.push_str(&s);
        // 执行 abac，并得到最终判断结果
        let mut model_text: String = "
        [request_definition]
        r = sub, obj

            [policy_definition]
        p = act

            [policy_effect]
        e = some(where (p.eft == allow))

            [matchers]
        ".into();
        // TODO: use no-std casbin to edit
        // model_text.push_str(&policy);
        // let model = match DefaultModel::from_str(&model_text) {
        //     Ok(m) => { m }
        //     Err(_)=> {return Err("".into());}
        // };
        // let e = match Enforcer::new(model,()) {
        //     Ok(e) => { e }
        //     Err(_)=> {return Err("".into());}
        // };
        // let dept_identity_btree_map: BTreeMap<String, String> = dept_identity.into();
        // match e.enforce((dept_identity_btree_map,"","")) {
        //     Ok(false) => { return Err(error_code::CODE_FORBIDDEN.into()); }
        //     _ => {}
        // }
    }
    // 构建 key_switch_trigger_stored 并存储上链
    let ks_trigger_to_be_stored = KeySwitchTriggerStored {
        key_switch_session_id: ks_session_id.clone(),
        resource_id: ks_trigger.resource_id,
        auth_session_id,
        creator,
        key_switch_pk: ks_trigger.key_switch_pk,
        timestamp: timestamp_as_datetime,
        validation_result,
    };
    ctx.ks_trigger_map.insert(ks_session_id, &ks_trigger_to_be_stored);


    // 通过事件返回值
    ctx.env().emit_event(KSCreated {
        event_id: EVENT_ID_FOR_RETURNED_VALUE.into(),
        data: ks_trigger_to_be_stored.clone(),
    });

    ctx.env().emit_event(KSCreated {
        event_id,
        data: ks_trigger_to_be_stored,
    });

    return Ok(());
}
pub fn create_key_switch_result(ctx: &mut Blbc, ks_result: KeySwitchResult) -> Result<(), String> {
    // 获取 ks_session_id
    let ks_session_id = ks_result.key_switch_session_id.clone();
    // 检查 key_switch_trigger_stored 是否存在
    let ks_trigger_stored = match ctx.ks_trigger_map.get(&ks_session_id) {
        None => { return Err(format!("key_switch_trigger_stored '{}' 不存在", &ks_session_id)); }
        Some(r) => { r }
    };

    // 获取创建者与时间戳
    let creator = ctx.env().caller();
    let timestamp = ctx.env().block_timestamp();
    let timestamp_as_datetime: ScaleDateTimeLocal = timestamp.into();

    // 构建 KeySwitchResultStored 并存储上链
    let ks_result_stored = KeySwitchResultStored{
        key_switch_session_id: ks_session_id.clone(),
        share: ks_result.share,
        zk_proof: ks_result.zk_proof,
        key_switch_pk: ks_result.key_switch_pk,
        creator,
        timestamp: timestamp_as_datetime
    };
    let key = format!("'{}'_'{:?}'",&ks_session_id,&creator);
    ctx.ks_result_map.insert(key.clone(), &ks_result_stored);
    ctx.ks_result_keys.push(key.clone());

    // 通过事件返回值
    let event_id = format!("ks_'{}'_result",&ks_session_id);

    ctx.env().emit_event(KSResultCreated {
        event_id: EVENT_ID_FOR_RETURNED_VALUE.into(),
        value: key.clone(),
    });

    ctx.env().emit_event(KSResultCreated {
        event_id,
        value: key,
    });

    return Ok(());
}

/// 指定 ks_session_id ,任意 creator 来查
pub fn list_key_switch_results_by_id(ctx: &Blbc, ks_session_id: String) -> Result<Vec<KeySwitchResultStored>,String>{
    let mut search_result:Vec<KeySwitchResultStored> = Vec::new();

    for ks_result_key in &ctx.ks_result_keys {
        // 找到匹配 ks_session_id 的 key
        if (*ks_result_key).starts_with(&ks_session_id){
            let mut prefix = ks_session_id.clone();
            prefix.push_str("_");
            let creator_string = (*ks_result_key).clone().replace(&prefix,"");
            match ctx.get_key_switch_result_with_accountid_string(ks_session_id.clone(),creator_string){
                Ok(r) => {search_result.push(r);}
                Err(msg) => { return Err(msg);}
            }
        }
    }
    return Ok(search_result);
}