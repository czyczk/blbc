use crate::model::key_switch::DepartmentIdentityStored;
use x509_cert::{Certificate, ext::Extension, time::Time};
use der::{
    asn1::{BitStringRef, ContextSpecific, ObjectIdentifier, UIntRef},
    Decode, DecodeValue, Encode, FixedTag, Header, Reader, Tag, Tagged,
};
use spki::AlgorithmIdentifier;
use ink_prelude::string::ToString;
use ink_prelude::string::String;
use serde_json::{Value};
use ink_prelude::vec::Vec;
use ink_env::Environment;
use crate::blbc::Blbc;
use ink_lang::codegen::Env;
use crate::CertificateReadErr;
// use rsa::pss::{BlindedSigningKey, VerifyingKey};
// use rsa::signature::{Keypair,RandomizedSigner, SignatureEncoding, Verifier};
// use sha2::{Digest, Sha256};
// use rsa::{RsaPublicKey, pkcs1::DecodeRsaPublicKey};


/// 调用后访问chain extension,获取runtime侧存储的证书
pub(crate) fn get_dept_identity_by_chain_extension(ctx: &mut Blbc) -> Result<DepartmentIdentityStored, String> {
    ink_env::debug_println!("chain extension尝试获取链上证书.....");
    let creator = ctx.env().caller();
    ink_env::debug_println!("chain extension尝试获取{:?}的链上证书.....",&creator);
    let received_buffer: Result<[u8; 2000], CertificateReadErr> = ctx.env().extension().fetch_account_certificate(creator.clone());
    match received_buffer {
        Ok(cert) => {
            ink_env::debug_println!("chain extension获取到的证书{:?}",cert.clone());
            let timestamp = ctx.env().block_timestamp() as u64;
            //let tt = 1001u64.checked_div(100).unwrap();
            ink_env::debug_println!("链上时间{:?}",timestamp.checked_div(1000).unwrap());
            match parse_x509(cert, timestamp.checked_div(1000).unwrap()){
                Ok(dept_identity) => {
                    return Ok(dept_identity);
                }
                Err(msg) => {
                    return Err(msg);
                }
            }
        }
        Err(_) => {
            ink_env::debug_println!("chain extension无法从链上获取证书");
            return Err("chain extension无法从链上获取证书".into());
        }
    }
}

/// 校验并解析 x509 证书,如果出错返回错误信息，解析成功则返回部门身份信息
pub(crate) fn parse_x509(der_encoded_cert: [u8;2000], current_time: u64) -> Result<DepartmentIdentityStored, String> {
    ink_env::debug_println!("parse_x509 函数打印获取到的证书{:?}",der_encoded_cert);
    // chain extension返回值的最后10位表示证书的长度
    let mut cert_len:u32 = 0;
    for i in 1990..2000 {
        cert_len = cert_len+ der_encoded_cert[i] as u32;
    }
    ink_env::debug_println!("test函数获取到的证书长度为{}",cert_len);
    if cert_len == 2000{
        ink_env::debug_println!("此账户对应的证书过长，超出1990的长度限制");
        return Err("此账户对应的证书过长，超出1990的长度限制".into());
    }

    // 由 u8 数组解析出证书
    match Certificate::from_der(&der_encoded_cert[0..cert_len as usize]){
        Ok(result) => {
            let extensions =  result.tbs_certificate.extensions.unwrap();

            let dept_identity=get_department_identity_easier(&extensions);

            let validity = result.tbs_certificate.validity;
            let before_time = validity.not_before.to_date_time().unix_duration().as_secs();
            let after_time = validity.not_after.to_date_time().unix_duration().as_secs();

            ink_env::debug_println!("afterTime: {:?} beforeTime: {:?} current_time{:?}", after_time, before_time, current_time);

            // 验证证书是否过期
            if !(current_time > before_time && current_time < after_time) {
                ink_env::debug_println!("x509证书已过期，请重新配置");
                return Err("x509证书已过期，请重新配置".into());
            }

            // todo 验证证书签名是否正确
            // PSS signatures
//
//
//             let mut rng = rand::thread_rng();
//
//             let bits = 2048;
//             let private_key = RsaPrivateKey::new(&mut rng, bits).expect("failed to generate a key");
//             let signing_key = BlindedSigningKey::<Sha256>::new(private_key);
//             let verifying_key = signing_key.verifying_key();
//
//             // Sign
//             let data = b"hello world";
//             let signature = signing_key.sign_with_rng(&mut rng, data);
//             assert_ne!(signature.to_bytes().as_ref(), data);
//
//             // Verify
//             verifying_key.verify(data, &signature).expect("failed to verify");
//
//
//             let pem = "-----BEGIN PUBLIC KEY-----
// MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvLD6Rgexw2dQaN7HaJ3Y
// JPaxOF4rBGKM2HdVNIixvfAWv+xYECvzq32uCJ9RNyb7tRwBX6LhJXK9Y0ZWwY98
// I91Ts/RujMjc+m8lSuxqdfE87xZzugo+0AGMRMudjNa8/5bJX8HKPSbuJoGAw4Ex
// T59TtlYDkHFxaF1MSvbyJcw67XMHC5cNEIpDpxQ1+q8blAbK9ihKQFd7zycFazCV
// mBSKPhjJ/TIEy9N2ekx01SXtm08Lpq6MuTIE7mfWzNtNXUS5rlzxwb+w6QOhRkAi
// xkV1v3ZzWZdkB50ZLyJAv3d/SjKM1gZ46NCJYFqfskv+weXQ7ii8cRtJkYkAAMhG
// 0QIDAQAB
// -----END PUBLIC KEY-----";
//
//             let public_key = RsaPublicKey::from_pkcs1_pem(pem)?;
//

            return Ok(dept_identity);
        }
        Err(_) => {
            ink_env::debug_println!("证书格式有误，x509证书解析失败");
            return Err("证书格式有误，x509证书解析失败".into());
        }
    }
}




/// 由证书 extension 得到部门信息 dept_identity
pub(crate) fn get_department_identity_easier(extensions: &Vec<Extension>) -> DepartmentIdentityStored{
    let mut dept_identity =DepartmentIdentityStored{
        dept_type: "".into(),
        dept_level: 0,
        dept_name: "".into(),
        super_dept_name: "".into()
    };

    let ipOid:ObjectIdentifier = ObjectIdentifier::new_unwrap("1.2.3.4.5.6.7.8.1");
    for extension in extensions.iter() {
        if (extension.extn_id.eq(&ipOid)) {
            let data = decimals_to_string_not_check(extension.extn_value);
            // 因为此处的处理，以符号分割来提取字段值，故这些字段值中都不能包含这些符号，并且字段的顺序也不能改变,字段值的前后也不能出现双引号
            // eg. {attrs:{DeptLevel:1,DeptName:838,DeptType:admin,SuperDeptName:101}}
            let v: Vec<&str> = data.split(&[':', ',', ';', '}'][..]).collect();
            dept_identity.dept_level = u8::from_str_radix(v[2], 10).unwrap();
            dept_identity.dept_name = v[4].into();
            dept_identity.dept_type = v[6].into();
            dept_identity.super_dept_name = v[8].into();
            //panic!("{:?}{:?}",data,dept_identity);
        }
    }
    dept_identity
}

/// 阿斯克码转字符串
// pub fn decimals_to_string(dec_vec: &[u8]) -> Result<String, String>{
//     let mut text = String::new();
//     for d in dec_vec.iter(){
//         if !(d >=  &32 && d <= &126) {
//             return Err("the number is outside the ascii range".to_string());
//         } else {
//             text.push(*d as char);
//         }
//     }
//     Ok(text)
// }


/// 阿斯克码转字符串,在输入的值超出阿斯克码表示范围时不提示，也不报错，直接忽略
pub fn decimals_to_string_not_check(dec_vec: &[u8]) -> String{
    let mut text = String::new();
    for d in dec_vec.iter(){
        if d >= &32 && d <= &126 {
            text.push(*d as char);
        }
    }
    text
}


pub struct DeferDecodeCertificate<'a> {
    /// tbsCertificate       TBSCertificate,
    pub tbs_certificate: &'a [u8],
    /// signatureAlgorithm   AlgorithmIdentifier,
    pub signature_algorithm: &'a [u8],
    /// signature            BIT STRING
    pub signature: &'a [u8],
}

impl<'a> DecodeValue<'a> for DeferDecodeCertificate<'a> {
    fn decode_value<R: Reader<'a>>(
        reader: &mut R,
        header: Header,
    ) -> der::Result<DeferDecodeCertificate<'a>> {
        reader.read_nested(header.length, |reader| {
            Ok(Self {
                tbs_certificate: reader.tlv_bytes()?,
                signature_algorithm: reader.tlv_bytes()?,
                signature: reader.tlv_bytes()?,
            })
        })
    }
}

impl FixedTag for DeferDecodeCertificate<'_> {
    const TAG: Tag = Tag::Sequence;
}

///Structure supporting deferred decoding of fields in the TBSCertificate SEQUENCE
pub struct DeferDecodeTbsCertificate<'a> {
    /// Decoded field
    pub version: u8,
    /// Defer decoded field
    pub serial_number: &'a [u8],
    /// Defer decoded field
    pub signature: &'a [u8],
    /// Defer decoded field
    pub issuer: &'a [u8],
    /// Defer decoded field
    pub validity: &'a [u8],
    /// Defer decoded field
    pub subject: &'a [u8],
    /// Defer decoded field
    pub subject_public_key_info: &'a [u8],
    /// Decoded field (never present)
    pub issuer_unique_id: Option<BitStringRef<'a>>,
    /// Decoded field (never present)
    pub subject_unique_id: Option<BitStringRef<'a>>,
    /// Defer decoded field
    pub extensions: &'a [u8],
}

impl<'a> DecodeValue<'a> for DeferDecodeTbsCertificate<'a> {
    fn decode_value<R: Reader<'a>>(
        reader: &mut R,
        header: Header,
    ) -> der::Result<DeferDecodeTbsCertificate<'a>> {
        reader.read_nested(header.length, |reader| {
            let version = ContextSpecific::decode_explicit(reader, ::der::TagNumber::N0)?
                .map(|cs| cs.value)
                .unwrap_or_else(Default::default);

            Ok(Self {
                version,
                serial_number: reader.tlv_bytes()?,
                signature: reader.tlv_bytes()?,
                issuer: reader.tlv_bytes()?,
                validity: reader.tlv_bytes()?,
                subject: reader.tlv_bytes()?,
                subject_public_key_info: reader.tlv_bytes()?,
                issuer_unique_id: reader.decode()?,
                subject_unique_id: reader.decode()?,
                extensions: reader.tlv_bytes()?,
            })
        })
    }
}

impl FixedTag for DeferDecodeTbsCertificate<'_> {
    const TAG: Tag = Tag::Sequence;
}