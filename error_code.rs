/// CODE_FORBIDDEN 表示参数被理解，但无权进行操作。收到的错误中若是这样的错误信息则表示是操作权限的问题，而非链码运行出错。对应 HTTP 状态码的 403。
pub const CODE_FORBIDDEN: &str = "~FORBIDDEN~";
/// CODE_NOT_FOUND 表示资源未找到。收到的错误中若是这样的错误信息则表示是资源未找到，而非链码运行出错。对应 HTTP 状态码的 404。
pub const CODE_NOT_FOUND: &str = "~NOTFOUND~";
/// CodeNotImplemented 是个在这个项目中约定俗成的代号。收到错误中若是这样的错误信息则表示是暂时未实现的功能而非链码运行出错。对应 HTTP 状态码的 500。
pub const CODE_NOT_IMPLEMENTED: &str = "~NOTIMPLEMENTED~";
/// CodeGatewayTimeout 是个在这个项目中约定俗成的代号。收到错误中若是这样的错误信息则表示是因操作超时引起的。对应 HTTP 状态码的 504。
pub const CODE_GATEWAY_TIMEOUT: &str = "~GATEWAYTIMEOUT~";
