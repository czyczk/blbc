use ink_prelude::string::String;
use ink_prelude::vec::Vec;
use crate::model::key_switch::DepartmentIdentityStored;

fn enforce_policy(policy: String,dept_identity:DepartmentIdentityStored) -> Result<bool, String> {
    policy.replace(" ", "");
    let mut attribute_stack: Vec<String> = Vec::new();
    let mut brackets_stack: Vec<char> = Vec::new();
    let mut comparison_operator_stack: Vec<String> = Vec::new();
    let mut logical_operator_stack: Vec<char> = Vec::new();
    let mut policy_as_chars = policy.chars();
    // for char in policy_as_chars {
    //     if char.eq('('.into()){
    //         // 左括号进括号栈
    //         brackets_stack.push(char);
    //     }else if  {
    //         // 英文字母则继续读，直至非英文字母
    //     }
    // }
    return Err("".into())
}