use core::str::Chars;
use core::num::ParseIntError;
use ink_prelude::string::String;
use ink_prelude::string::ToString;
use ink_prelude::vec::Vec;
use crate::error_code;
use crate::model::key_switch::DepartmentIdentityStored;

pub fn enforce_policy(policy: String, dept_identity: DepartmentIdentityStored) -> Result<bool, String> {
    // 检查输入的 policy 的内容
    policy.replace(" ", "");
    let policy_as_chars = policy.chars();
    for char in policy_as_chars{
        if !char.is_ascii_punctuation() && !char.is_ascii_digit() && !char.is_ascii_alphabetic(){
            // 如果 policy 既不是标点符号又不是数字也不是英文字母，则报错
            return Err(error_code::CODE_NOT_IMPLEMENTED.into());
        }
    }
    // 首先进行词法分析 tokenize(),然后将中缀表达式转后缀表达式，然后对后缀表达式进行计算
    let tokens;
    match tokenize(policy, dept_identity) {
        Ok(t) => {
            tokens = t;
            //panic!("{:?}",tokens);
            // 逻辑表达式属于中缀表达式，以下进行中缀表达式转后缀表达式，然后对后缀表达式进行计算
            let mut operator_stack: Vec<Token> = Vec::new();
            let mut temp_stack: Vec<Token> = Vec::new();
            for token in tokens {
                if token.category == Category::NumConstant || token.category == Category::StrConstant || token.category == Category::DeptType || token.category == Category::DeptLevel || token.category == Category::SuperDeptName || token.category == Category::DeptName {
                    temp_stack.push(token);
                } else if token.category == Category::Eq || token.category == Category::Ne || token.category == Category::Gt || token.category == Category::Lt || token.category == Category::Ge || token.category == Category::Le || token.category == Category::And || token.category == Category::Or {
                    loop {
                        match operator_stack.last() {
                            None => {
                                // None时表明栈空
                                operator_stack.push(token);
                                break;
                            }
                            Some(top) => {
                                if top.category == Category::LeftBracket {
                                    operator_stack.push(token);
                                    break;
                                } else if token.priority > top.priority {
                                    operator_stack.push(token);
                                    break;
                                } else {
                                    let tmp: Token = operator_stack.pop().unwrap();
                                    temp_stack.push(tmp);
                                }
                            }
                        }
                    }
                } else if token.category == Category::LeftBracket {
                    operator_stack.push(token);
                } else if token.category == Category::RightBracket {
                    loop {
                        match operator_stack.pop() {
                            None => {
                                return Err(error_code::CODE_NOT_IMPLEMENTED.into());
                            }
                            Some(top) => {
                                if top.category == Category::LeftBracket {
                                    break;
                                }
                                temp_stack.push(top);
                            }
                        }
                    }
                }
            }
            if !operator_stack.is_empty() {
                loop {
                    match operator_stack.pop() {
                        None => { break; }
                        Some(top) => {
                            temp_stack.push(top);
                        }
                    }
                }
            }
            //以下为计算后缀表达式
            let mut calculator_stack: Vec<Token> = Vec::new();
            for token in temp_stack {
                if token.category == Category::NumConstant || token.category == Category::StrConstant || token.category == Category::DeptType || token.category == Category::DeptLevel || token.category == Category::SuperDeptName || token.category == Category::DeptName {
                    calculator_stack.push(token);
                } else if token.category == Category::Eq || token.category == Category::Ne || token.category == Category::Gt || token.category == Category::Lt || token.category == Category::Ge || token.category == Category::Le || token.category == Category::And || token.category == Category::Or {
                    let operand1;
                    let operand2;
                    match calculator_stack.pop() {
                        None => {
                            return Err(error_code::CODE_NOT_IMPLEMENTED.into());
                        }
                        Some(t) => { operand2 = t; }
                    }
                    match calculator_stack.pop() {
                        None => {
                            return Err(error_code::CODE_NOT_IMPLEMENTED.into());
                        }
                        Some(t) => { operand1 = t; }
                    }
                    if operand1.category == Category::DeptName || operand1.category == Category::SuperDeptName || operand1.category == Category::DeptType {
                        let tmp;
                        match token.category {
                            Category::Eq => {
                                if operand1.value.eq(&operand2.value) {
                                    tmp = Token {
                                        category: Category::True,
                                        value: None,
                                        priority: None,
                                    };
                                } else {
                                    tmp = Token {
                                        category: Category::False,
                                        value: None,
                                        priority: None,
                                    };
                                }
                            }
                            Category::Ne => {
                                if operand1.value.ne(&operand2.value) {
                                    tmp = Token {
                                        category: Category::True,
                                        value: None,
                                        priority: None,
                                    };
                                } else {
                                    tmp = Token {
                                        category: Category::False,
                                        value: None,
                                        priority: None,
                                    };
                                }
                            }
                            _ => {
                                return Err(error_code::CODE_NOT_IMPLEMENTED.into());
                            }
                        }
                        calculator_stack.push(tmp);
                    } else if operand1.category == Category::True || operand1.category == Category::False {
                        let tmp;
                        match token.category {
                            Category::And => {
                                if operand1.category == Category::True && operand2.category == Category::True {
                                    tmp = Token {
                                        category: Category::True,
                                        value: None,
                                        priority: None,
                                    };
                                } else {
                                    tmp = Token {
                                        category: Category::False,
                                        value: None,
                                        priority: None,
                                    };
                                }
                            }
                            Category::Or => {
                                if operand1.category == Category::False && operand2.category == Category::False {
                                    tmp = Token {
                                        category: Category::False,
                                        value: None,
                                        priority: None,
                                    };
                                } else {
                                    tmp = Token {
                                        category: Category::True,
                                        value: None,
                                        priority: None,
                                    };
                                }
                            }
                            _ => {
                                return Err(error_code::CODE_NOT_IMPLEMENTED.into());
                            }
                        }
                        calculator_stack.push(tmp);
                    } else if operand1.category == Category::DeptLevel {
                        // 当前端关键字为 DeptLevel 时，可用的操作符共6种
                        match operand1.value {
                            None => {
                                return Err(error_code::CODE_NOT_IMPLEMENTED.into());
                            }
                            Some(op1) => {
                                match operand2.value {
                                    None => {
                                        return Err(error_code::CODE_NOT_IMPLEMENTED.into());
                                    }
                                    Some(op2) => {
                                        // 如果操作数1和操作数2都有值
                                        match op1.parse::<u8>() {
                                            Ok(op1_as_u8) => {
                                                match op2.parse::<u8>() {
                                                    Ok(op2_as_u8) => {
                                                        // 如果操作数1和操作数2都能成功解析成数字
                                                        let tmp;
                                                        match token.category {
                                                            Category::Le => {
                                                                if op1_as_u8 <= op2_as_u8 {
                                                                    tmp = Token {
                                                                        category: Category::True,
                                                                        value: None,
                                                                        priority: None,
                                                                    };
                                                                }else {
                                                                    tmp = Token {
                                                                        category: Category::False,
                                                                        value: None,
                                                                        priority: None,
                                                                    };
                                                                }
                                                            }
                                                            Category::Ge => {
                                                                if op1_as_u8 >= op2_as_u8 {
                                                                    tmp = Token {
                                                                        category: Category::True,
                                                                        value: None,
                                                                        priority: None,
                                                                    };
                                                                }else {
                                                                    tmp = Token {
                                                                        category: Category::False,
                                                                        value: None,
                                                                        priority: None,
                                                                    };
                                                                }
                                                            }
                                                            Category::Eq => {
                                                                if op1_as_u8 == op2_as_u8 {
                                                                    tmp = Token {
                                                                        category: Category::True,
                                                                        value: None,
                                                                        priority: None,
                                                                    };
                                                                }else {
                                                                    tmp = Token {
                                                                        category: Category::False,
                                                                        value: None,
                                                                        priority: None,
                                                                    };
                                                                }
                                                            }
                                                            Category::Ne => {
                                                                if op1_as_u8 != op2_as_u8 {
                                                                    tmp = Token {
                                                                        category: Category::True,
                                                                        value: None,
                                                                        priority: None,
                                                                    };
                                                                }else {
                                                                    tmp = Token {
                                                                        category: Category::False,
                                                                        value: None,
                                                                        priority: None,
                                                                    };
                                                                }
                                                            }
                                                            Category::Lt => {
                                                                if op1_as_u8 < op2_as_u8 {
                                                                    tmp = Token {
                                                                        category: Category::True,
                                                                        value: None,
                                                                        priority: None,
                                                                    };
                                                                }else {
                                                                    tmp = Token {
                                                                        category: Category::False,
                                                                        value: None,
                                                                        priority: None,
                                                                    };
                                                                }
                                                            }
                                                            Category::Gt => {
                                                                if op1_as_u8 > op2_as_u8 {
                                                                    tmp = Token {
                                                                        category: Category::True,
                                                                        value: None,
                                                                        priority: None,
                                                                    };
                                                                }else {
                                                                    tmp = Token {
                                                                        category: Category::False,
                                                                        value: None,
                                                                        priority: None,
                                                                    };
                                                                }
                                                            }
                                                            _ => {
                                                                return Err(error_code::CODE_NOT_IMPLEMENTED.into());
                                                            }
                                                        }
                                                        calculator_stack.push(tmp);
                                                    }
                                                    Err(_) => {
                                                        return Err(error_code::CODE_NOT_IMPLEMENTED.into());
                                                    }
                                                }
                                            }
                                            Err(_) => {
                                                return Err(error_code::CODE_NOT_IMPLEMENTED.into());
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            return match calculator_stack.pop() {
                None => {
                    Err(error_code::CODE_NOT_IMPLEMENTED.into())
                }
                Some(result) => {
                    if result.category == Category::True {
                        Ok(true)
                    } else if result.category == Category::False {
                        Ok(false)
                    } else {
                        Err(error_code::CODE_NOT_IMPLEMENTED.into())
                    }
                }
            }
        }
        Err(msg) => { return Err(msg); }
    }
}

fn tokenize(policy: String, dept_identity: DepartmentIdentityStored) -> Result<Vec<Token>, String> {
    let mut tokens: Vec<Token> = Vec::new();
    let policy_as_chars:Vec<char> = policy.chars().collect();
    //panic!("{:?}",policy_as_chars);
    let length = policy.len();
    let mut i = 0;
    while i < length {
        ink_env::debug_println!("i的值: {},第i个字符{}", i,policy_as_chars[i]);
        if policy_as_chars[i].eq(&'(') {
            // 左括号
            let token = Token {
                category: Category::LeftBracket,
                value: None,
                priority: None,
            };
            tokens.push(token);
        } else if policy_as_chars[i].eq(&')') {
            // 右括号
            let token = Token {
                category: Category::RightBracket,
                value: None,
                priority: None,
            };
            tokens.push(token);
        } else if policy_as_chars[i].eq(&'<') && policy_as_chars[i + 1].eq(&'=') {
            // <=
            let token = Token {
                category: Category::Le,
                value: None,
                priority: Some(5),
            };
            tokens.push(token);
            i = i + 1;
        } else if policy_as_chars[i].eq(&'>') && policy_as_chars[i + 1].eq(&'=') {
            // >=
            let token = Token {
                category: Category::Ge,
                value: None,
                priority: Some(5),
            };
            tokens.push(token);
            i = i + 1;
        } else if policy_as_chars[i].eq(&'=') && policy_as_chars[i + 1].eq(&'=') {
            // ==
            let token = Token {
                category: Category::Eq,
                value: None,
                priority: Some(5),
            };
            tokens.push(token);
            i = i + 1;
        } else if policy_as_chars[i].eq(&'!') && policy_as_chars[i + 1].eq(&'=') {
            // !=
            let token = Token {
                category: Category::Ne,
                value: None,
                priority: Some(5),
            };
            tokens.push(token);
            i = i + 1;
        } else if policy_as_chars[i].eq(&'<') && policy_as_chars[i + 1].ne(&'=') {
            // <
            let token = Token {
                category: Category::Lt,
                value: None,
                priority: Some(5),
            };
            tokens.push(token);
        } else if policy_as_chars[i].eq(&'>') && policy_as_chars[i + 1].ne(&'=') {
            // >
            let token = Token {
                category: Category::Gt,
                value: None,
                priority: Some(5),
            };
            tokens.push(token);
        } else if policy_as_chars[i].eq(&'&') && policy_as_chars[i + 1].eq(&'&') {
            // &&
            let token = Token {
                category: Category::And,
                value: None,
                priority: Some(4),
            };
            tokens.push(token);
            i = i + 1;
        } else if policy_as_chars[i].eq(&'|') && policy_as_chars[i + 1].eq(&'|') {
            // ||
            let token = Token {
                category: Category::Or,
                value: None,
                priority: Some(3),
            };
            tokens.push(token);
            i = i + 1;
        } else if !policy.get(i..i + 8).is_none() && policy.get(i..i + 8).unwrap().eq("DeptType") {
            // DeptType
            let token = Token {
                category: Category::DeptType,
                value: Some(dept_identity.dept_type.clone()),
                priority: None,
            };
            tokens.push(token);
            i = i + 7;
        } else if !policy.get(i..i + 9).is_none() && policy.get(i..i + 9).unwrap().eq("DeptLevel") {
            // DeptLevel
            let token = Token {
                category: Category::DeptLevel,
                value: Some(dept_identity.dept_level.to_string()),
                priority: None,
            };
            tokens.push(token);
            i = i + 8;
        } else if !policy.get(i..i + 8).is_none() && policy.get(i..i + 8).unwrap().eq("DeptName") {
            // DeptName
            let token = Token {
                category: Category::DeptName,
                value: Some(dept_identity.dept_name.clone()),
                priority: None,
            };
            tokens.push(token);
            i = i + 7;
        } else if !policy.get(i..i + 13).is_none() && policy.get(i..i + 13).unwrap().eq("SuperDeptName") {
            // SuperDeptName
            let token = Token {
                category: Category::SuperDeptName,
                value: Some(dept_identity.super_dept_name.clone()),
                priority: None,
            };
            tokens.push(token);
            i = i + 12;
        } else if policy_as_chars[i].eq(&'\'') {
            // 字符串常量
            match policy.get(i + 1..) {
                Some(substr) => {
                    match substr.find('\'') {
                        // 寻找右引号的位置
                        None => { return Err(error_code::CODE_NOT_IMPLEMENTED.into()); }
                        Some(pos) => {
                            let token = Token {
                                category: Category::StrConstant,
                                value: Some(policy.get(i + 1..i + pos + 1).unwrap().into()),
                                priority: None,
                            };
                            tokens.push(token);
                            i = i + pos + 1;
                        }
                    }
                }
                _ => {}
            }
        } else if policy_as_chars[i].is_ascii_digit() {
            // 数值常量
            let mut pos = i + 1;
            while pos < length && policy_as_chars[pos].is_ascii_digit() {
                pos = pos + 1;
            }
            match policy.get(i..pos) {
                None => {}
                Some(num_constant) => {
                    let token = Token {
                        category: Category::NumConstant,
                        value: Some(num_constant.into()),
                        priority: None,
                    };
                    tokens.push(token);
                    i = pos - 1;
                }
            }
        }
        i = i + 1;
    }
    return Ok(tokens);
}

#[derive(Debug)]
/// Token 结构体表示词法分析后产生的词法单元
struct Token {
    // 词法单元的种别码
    category: Category,
    // 词法单元的字面值,仅当词法单元为操作数时启用
    value: Option<String>,
    // 当词法单元为运算符时，此项启用。表示运算符的优先级，数值越大优先级越高
    priority: Option<i8>,
}

#[derive(PartialEq, Debug)]
/// Category 表示词法单元的种别码
enum Category {
    // 左括号
    LeftBracket,
    // 右括号
    RightBracket,
    // <=
    Le,
    // >=
    Ge,
    // ==
    Eq,
    // !=
    Ne,
    // <
    Lt,
    // >
    Gt,
    // &&
    And,
    // ||
    Or,
    // 部门类型
    DeptType,
    // 部门级别
    DeptLevel,
    // 部门名称
    DeptName,
    // 上级部门名称
    SuperDeptName,
    // 字符串常量
    StrConstant,
    // 数值常量
    NumConstant,
    // 布尔值,仅在后缀表达式计算时使用
    True,
    // 布尔值,仅在后缀表达式计算时使用
    False,
}