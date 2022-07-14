#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;
extern crate core;

pub mod auth;
pub mod data;
pub mod error_code;
pub mod extension;
pub mod key_switch;
pub mod model;

use ink_lang as ink;

#[ink::contract]
mod blbc {
    use crate::model::auth::{AuthRequest, AuthRequestStored, AuthResponse, AuthResponseStored};
    use crate::model::datetime::ScaleDateTimeLocal;
    use crate::model::key_switch::{
        DepartmentIdentityStored, KeySwitchResult, KeySwitchResultQuery, KeySwitchResultStored,
        KeySwitchTrigger, KeySwitchTriggerStored,
    };
    use crate::model::query::QueryConditions;
    use crate::{
        auth, data, error_code, key_switch,
        model::data::{EncryptedData, OffchainData, PlainData, ResMetadataStored},
        model::query::IDsWithPagination,
    };
    use ink_prelude::format;
    use ink_prelude::string::String;
    use ink_prelude::vec::Vec;
    use ink_storage::{traits::SpreadAllocate, Mapping};

    #[ink(storage)]
    #[derive(SpreadAllocate)]
    pub struct Blbc {
        /// 存储所有的资源 ID
        pub resource_ids: Vec<String>,
        /// 存储所有请求的 auth_session_id
        pub auth_session_ids: Vec<String>,
        /// 存储所有的 ${ksSessionID}_${creator_as_base64}
        pub ks_result_keys: Vec<String>,
        /// 存储通过资源 ID 可以找到的链上资源，资源内容是字节数组
        pub res_map: Mapping<String, Vec<u8>>,
        /// 存储通过资源 ID 可以找到的资源元数据
        pub res_metadata_map: Mapping<String, ResMetadataStored>,
        /// 存储通过资源 ID 可以找到的对称密钥
        pub res_key_map: Mapping<String, Vec<u8>>,
        /// 存储通过资源 ID 可以找到的策略
        pub res_policy_map: Mapping<String, String>,
        /// 存储通过 auth_session_id 可以找到的 AuthRequestStored
        pub auth_request_map: Mapping<String, AuthRequestStored>,
        /// 存储通过 auth_session_id 可以找到的 AuthResponseStored
        pub auth_response_map: Mapping<String, AuthResponseStored>,
        /// 存储通过 ks_session_id 可以找到的 KeySwitchTriggerStored
        pub ks_trigger_map: Mapping<String, KeySwitchTriggerStored>,
        /// 存储通过 ${ks_session_id}_${creator_as_base64} 可以找到的 KeySwitchResultStored
        pub ks_result_map: Mapping<String, KeySwitchResultStored>,
    }

    #[ink(event)]
    pub struct ResourceCreated {
        pub event_id: String,
        pub resource_id: String,
    }

    #[ink(event)]
    pub struct AuthCreated {
        pub event_id: String,
        pub auth_session_id: String,
    }

    #[ink(event)]
    pub struct KSCreated {
        pub event_id: String,
        pub data: String, // Manually JSONfied KeySwitchTriggerStored object
    }

    #[ink(event)]
    pub struct KSResultCreated {
        pub event_id: String,
        pub value: String,
    }

    pub const EVENT_ID_FOR_RETURNED_VALUE: &'static str = "~TXRET~";

    impl Blbc {
        #[ink(constructor)]
        pub fn default() -> Self {
            ink_lang::utils::initialize_contract(|_: &mut Self| {})
        }

        #[ink(message)]
        pub fn create_plain_data(
            &mut self,
            plain_data: PlainData,
            event_id: Option<String>,
        ) -> Result<(), String> {
            data::create_plain_data(self, plain_data, event_id)
        }

        #[ink(message)]
        pub fn create_encrypted_data(
            &mut self,
            encrypted_data: EncryptedData,
            event_id: Option<String>,
        ) -> Result<(), String> {
            data::create_encrypted_data(self, encrypted_data, event_id)
        }

        #[ink(message)]
        pub fn create_offchain_data(
            &mut self,
            offchain_data: OffchainData,
            event_id: Option<String>,
        ) -> Result<(), String> {
            data::create_offchain_data(self, offchain_data, event_id)
        }

        #[ink(message)]
        pub fn get_metadata(&self, resource_id: String) -> Result<ResMetadataStored, String> {
            ink_env::debug_println!("---");
            ink_env::debug_println!("get_metadata");

            // 读 metadata 并返回，若未找到则返回 CODE_NOT_FOUND
            let metadata = match self.res_metadata_map.get(&resource_id) {
                Some(it) => it,
                None => return Err(error_code::CODE_NOT_FOUND.into()),
            };

            // 合约现还不支持范型，故不能指定 lifetime，只能把有所有权的东西传出。
            return Ok(metadata.clone());
        }

        #[ink(message)]
        // 与 Go 链码不同，此处因为 API 不传输二进制，数据内容须以 Base64 编码传出。
        pub fn get_data(&self, resource_id: String) -> Result<String, String> {
            ink_env::debug_println!("---");
            ink_env::debug_println!("get_data");

            // 读 data 并返回，若未找到则返回 codeNotFound
            let data_bytes = self
                .res_map
                .get(&resource_id)
                .ok_or::<String>(error_code::CODE_NOT_FOUND.into())?;

            // 因为 API 不传输二进制，数据内容须以 Base64 编码传出。
            ink_env::debug_println!("正在将数据以 Base64 编码");
            let data_as_base64: String = base64::encode(data_bytes);
            return Ok(data_as_base64);
        }

        #[ink(message)]
        // 与 Go 链码不同，此处因为 API 不传输二进制，数据内容须以 Base64 编码传出。
        pub fn get_key(&self, resource_id: String) -> Result<String, String> {
            ink_env::debug_println!("---");
            ink_env::debug_println!("get_key");

            // 读 key 并返回，若未找到则返回 codeNotFound
            let key_bytes = self
                .res_key_map
                .get(&resource_id)
                .ok_or::<String>(error_code::CODE_NOT_FOUND.into())?;

            // 因为 API 不传输二进制，数据内容须以 Base64 编码传出。
            ink_env::debug_println!("正在将数据以 Base64 编码");
            let key_as_base64: String = base64::encode(key_bytes);
            return Ok(key_as_base64);
        }

        #[ink(message)]
        pub fn get_policy(&self, resource_id: String) -> Result<String, String> {
            ink_env::debug_println!("---");
            ink_env::debug_println!("get_policy");

            // 读 policy 并返回，若未找到则返回 codeNotFound
            let policy_bytes = self
                .res_policy_map
                .get(&resource_id)
                .ok_or::<String>(error_code::CODE_NOT_FOUND.into())?;

            return Ok(policy_bytes);
        }

        #[ink(message)]
        pub fn list_resource_ids_by_creator(
            &mut self,
            data_type: String,
            is_desc: bool,
            page_size: u64,
            bookmark: Option<String>,
        ) -> Result<IDsWithPagination, String> {
            data::list_resource_ids_by_creator(self, data_type, is_desc, page_size, bookmark)
        }

        #[ink(message)]
        pub fn list_resource_ids_by_conditions(
            &mut self,
            query_conditions: QueryConditions,
            page_size: u64,
        ) -> Result<IDsWithPagination, String> {
            data::list_resource_ids_by_conditions(self, query_conditions, page_size)
        }

        #[ink(message)]
        pub fn create_auth_request(
            &mut self,
            auth_session_id: String,
            auth_request: AuthRequest,
            event_id: Option<String>,
        ) -> Result<(), String> {
            ink_env::debug_println!("---");
            ink_env::debug_println!("create_auth_request");
            auth::create_auth_request(self, auth_session_id, auth_request, event_id)
        }

        #[ink(message)]
        pub fn create_auth_response(
            &mut self,
            auth_session_id: String,
            auth_response: AuthResponse,
            event_id: Option<String>,
        ) -> Result<(), String> {
            ink_env::debug_println!("---");
            ink_env::debug_println!("create_auth_response");
            auth::create_auth_response(self, auth_session_id, auth_response, event_id)
        }

        #[ink(message)]
        pub fn get_auth_request(
            &self,
            auth_session_id: String,
        ) -> Result<AuthRequestStored, String> {
            ink_env::debug_println!("---");
            ink_env::debug_println!("get_auth_request");

            // 读 auth_req 并返回，若未找到则返回 CODE_NOT_FOUND
            let auth_req = match self.auth_request_map.get(&auth_session_id) {
                Some(it) => it,
                None => return Err(error_code::CODE_NOT_FOUND.into()),
            };

            // 合约现还不支持范型，故不能指定 lifetime，只能把有所有权的东西传出。
            return Ok(auth_req.clone());
        }

        #[ink(message)]
        pub fn get_auth_response(
            &self,
            auth_session_id: String,
        ) -> Result<AuthResponseStored, String> {
            ink_env::debug_println!("---");
            ink_env::debug_println!("get_auth_response");

            // 读 auth_res 并返回，若未找到则返回 CODE_NOT_FOUND
            let auth_res = match self.auth_response_map.get(&auth_session_id) {
                Some(it) => it,
                None => return Err(error_code::CODE_NOT_FOUND.into()),
            };

            // 合约现还不支持范型，故不能指定 lifetime，只能把有所有权的东西传出。
            return Ok(auth_res.clone());
        }

        #[ink(message)]
        pub fn list_pending_auth_session_ids_by_resource_creator(
            &mut self,
            page_size: u32,
            bookmark: Option<String>,
        ) -> Result<IDsWithPagination, String> {
            ink_env::debug_println!("---");
            ink_env::debug_println!("list_pending_auth_session_ids_by_resource_creator");
            auth::list_pending_auth_session_ids_by_resource_creator(self, page_size, bookmark)
        }

        #[ink(message)]
        pub fn list_auth_session_ids_by_requestor(
            &mut self,
            page_size: u32,
            bookmark: Option<String>,
            is_latest_first: bool,
        ) -> Result<IDsWithPagination, String> {
            ink_env::debug_println!("---");
            ink_env::debug_println!("list_auth_session_ids_by_requestor");
            auth::list_auth_session_ids_by_requestor(self, page_size, bookmark, is_latest_first)
        }

        #[ink(message)]
        pub fn create_key_switch_trigger(
            &mut self,
            ks_session_id: String,
            dept_identity: DepartmentIdentityStored,
            ks_trigger: KeySwitchTrigger,
            event_id: String,
        ) -> Result<(), String> {
            key_switch::create_key_switch_trigger(
                self,
                ks_session_id,
                dept_identity,
                ks_trigger,
                event_id,
            )
        }

        #[ink(message)]
        pub fn create_key_switch_result(
            &mut self,
            ks_result: KeySwitchResult,
        ) -> Result<(), String> {
            key_switch::create_key_switch_result(self, ks_result)
        }

        #[ink(message)]
        pub fn get_key_switch_result(
            &self,
            query: KeySwitchResultQuery,
        ) -> Result<KeySwitchResultStored, String> {
            ink_env::debug_println!("---");
            ink_env::debug_println!("get_key_switch_result");

            // 获取 ks_session_id and result_creator
            let ks_session_id = query.key_switch_session_id;
            let result_creator = query.result_creator;

            let key = format!("'{}'_'{:?}'", &ks_session_id, &result_creator);

            // 读 KeySwitchResultStored 并返回，若未找到则返回 CODE_NOT_FOUND
            let ks_result_stored = match self.ks_result_map.get(&key) {
                Some(it) => it,
                None => return Err(error_code::CODE_NOT_FOUND.into()),
            };

            // 合约现还不支持范型，故不能指定 lifetime，只能把有所有权的东西传出。
            return Ok(ks_result_stored.clone());
        }

        #[ink(message)]
        pub fn list_key_switch_results_by_id(
            &mut self,
            ks_session_id: String,
        ) -> Result<Vec<KeySwitchResultStored>, String> {
            key_switch::list_key_switch_results_by_id(self, ks_session_id)
        }
    }

    /// Unit tests in Rust are normally defined within such a `#[cfg(test)]`
    /// module and test functions are marked with a `#[test]` attribute.
    /// The below code is technically just normal Rust code.
    #[cfg(test)]
    mod tests {
        /// Imports all the definitions from the outer scope so we can use them here.
        use super::*;

        extern crate alloc;

        use crate::model::data::{PlainData, ResMetadata, ResourceType};
        use crate::model::document::DocumentType;
        use crate::model::query::{
            CommonQueryConditions, DocumentQueryConditions, EntityAssetQueryConditions,
        };
        use alloc::collections::BTreeMap;
        /// Imports `ink_lang` so we can use `#[ink::test]`.
        use ink_lang as ink;
        use sha2::{Digest, Sha256};
        use std::iter::FromIterator;

        #[ink::test]
        fn default_works() {
            let _blbc = Blbc::default();

            // The new `Mapping` since rc-9 cannot be counted
            // assert_eq!(blbc.res_map.len(), 0);
            // assert_eq!(blbc.res_metadata_map.len(), 0);
        }

        #[ink::test]
        fn create_plain_data_with_normal_data_works() {
            // Prepare
            let mut blbc = Blbc::default();
            let sample_plain_data1 = get_sample_plain_data1();
            let resource_id = sample_plain_data1.metadata.resource_id.clone();
            let metadata = sample_plain_data1.metadata.clone();

            // Invoke with sample_plain_data1 and expect the return value to be Ok()
            assert!(blbc.create_plain_data(sample_plain_data1, None).is_ok());

            // Check if the data in map is as expected
            assert_eq!(
                blbc.res_map.get(&resource_id),
                Some(DATA1.as_bytes().to_owned())
            );

            // Check if the stored metadata is correct
            let metadata_stored = blbc.res_metadata_map.get(&resource_id);
            assert!(metadata_stored.is_some());
            let metadata_stored = metadata_stored.unwrap();
            assert_eq!(resource_id, metadata_stored.resource_id);
            assert_eq!(metadata.resource_type, metadata_stored.resource_type);
            assert_eq!(metadata.hash, metadata_stored.hash);
            assert_eq!(metadata.hash, metadata_stored.hash_stored);
            assert_eq!(metadata.size, metadata_stored.size);
            assert_eq!(metadata.size, metadata_stored.size_stored);
        }

        #[ink::test]
        fn create_plain_data_with_duplicate_resource_ids_fails_with_err() {
            // Prepare
            let mut blbc = Blbc::default();
            let sample_plain_data1 = get_sample_plain_data1();
            let mut sample_plain_data2 = get_sample_plain_data2();
            // Deliberately change the resource ID of data2 to be the same as data1
            sample_plain_data2.metadata.resource_id =
                sample_plain_data1.metadata.resource_id.clone();

            // Invoke with data1 and expect the return value to be Ok()
            assert!(blbc.create_plain_data(sample_plain_data1, None).is_ok());

            // Invoke with data2 and expect the return value to be Err()
            assert!(blbc.create_plain_data(sample_plain_data2, None).is_err());
        }

        #[ink::test]
        fn create_plain_data_with_corrupt_hash_and_size_fails_with_err() {
            // Prepare
            let mut blbc = Blbc::default();

            let mut sample_plain_data_corrupt_hash = get_sample_plain_data1();
            sample_plain_data_corrupt_hash.metadata.hash = "cbabcbabc".into();

            let mut sample_plain_data_corrupt_size = get_sample_plain_data2();
            sample_plain_data_corrupt_size.metadata.size += 233;

            // Invoke with "corrupt_hash" and expect the return value to be Err()
            assert!(blbc
                .create_plain_data(sample_plain_data_corrupt_hash, None)
                .is_err());

            // Invoke with "corrupt_size" and expect the return value to be Err()
            assert!(blbc
                .create_plain_data(sample_plain_data_corrupt_size, None)
                .is_err());
        }

        #[ink::test]
        fn create_encrypted_data_with_normal_data_works() {
            // Prepare
            let mut blbc = Blbc::default();
            let sample_encrypted_data1 = get_sample_encrypted_data1();
            let resource_id = sample_encrypted_data1.metadata.resource_id.clone();
            let metadata = sample_encrypted_data1.metadata.clone();
            let key = sample_encrypted_data1.key.clone();
            let policy = sample_encrypted_data1.policy.clone();

            // 将 key 从 Base64 解码
            let key_decoded = match base64::decode(key) {
                Ok(b) => Vec::from_iter(b),
                Err(err) => panic!("无法解析 key: {}", err),
            };
            // Invoke with sample_encrypted_data1 and expect the return value to be Ok()
            assert!(blbc
                .create_encrypted_data(sample_encrypted_data1, None)
                .is_ok());

            // Check if the data in maps is as expected
            assert_eq!(
                blbc.res_map.get(&resource_id),
                Some(DATA1.as_bytes().to_owned())
            );
            assert_eq!(
                blbc.res_key_map.get(&resource_id),
                Some(key_decoded.to_owned())
            );
            assert_eq!(blbc.res_policy_map.get(&resource_id), Some(policy));

            // Check if the stored metadata is correct
            let metadata_stored = blbc.res_metadata_map.get(&resource_id);
            assert!(metadata_stored.is_some());
            let metadata_stored = metadata_stored.unwrap();
            assert_eq!(resource_id, metadata_stored.resource_id);
            assert_eq!(metadata.resource_type, metadata_stored.resource_type);
            assert_eq!(metadata.hash, metadata_stored.hash);
            assert_eq!(metadata.hash, metadata_stored.hash_stored);
            assert_eq!(metadata.size, metadata_stored.size);
            assert_eq!(metadata.size, metadata_stored.size_stored);
        }

        #[ink::test]
        fn create_encrypted_data_with_duplicate_resource_ids_fails_with_err() {
            // Prepare
            let mut blbc = Blbc::default();
            let sample_encrypted_data1 = get_sample_encrypted_data1();
            let mut sample_encrypted_data2 = get_sample_encrypted_data2();
            // Deliberately change the resource ID of data2 to be the same as data1
            sample_encrypted_data2.metadata.resource_id =
                sample_encrypted_data1.metadata.resource_id.clone();

            // Invoke with data1 and expect the return value to be Ok()
            assert!(blbc
                .create_encrypted_data(sample_encrypted_data1, None)
                .is_ok());

            // Invoke with data2 and expect the return value to be Err()
            assert!(blbc
                .create_encrypted_data(sample_encrypted_data2, None)
                .is_err());
        }

        #[ink::test]
        fn create_offchain_data_with_normal_data_works() {
            // Prepare
            let mut blbc = Blbc::default();
            let sample_offchain_data1 = get_sample_offchain_data1();
            let resource_id = sample_offchain_data1.metadata.resource_id.clone();
            let metadata = sample_offchain_data1.metadata.clone();
            let cid = sample_offchain_data1.cid.clone();
            let key = sample_offchain_data1.key.clone();
            let policy = sample_offchain_data1.policy.clone();

            // 将 key 从 Base64 解码
            let key_decoded = match base64::decode(key) {
                Ok(b) => Vec::from_iter(b),
                Err(err) => panic!("无法解析 key: {}", err),
            };
            // Invoke with sample_encrypted_data1 and expect the return value to be Ok()
            assert!(blbc
                .create_offchain_data(sample_offchain_data1, None)
                .is_ok());

            // Check if the data in maps is as expected
            assert_eq!(
                blbc.res_map.get(&resource_id),
                Some(Vec::from_iter(cid.into_bytes()))
            );
            assert_eq!(
                blbc.res_key_map.get(&resource_id),
                Some(key_decoded.to_owned())
            );
            assert_eq!(blbc.res_policy_map.get(&resource_id), Some(policy));

            // Check if the stored metadata is correct
            let metadata_stored = blbc.res_metadata_map.get(&resource_id);
            assert!(metadata_stored.is_some());
            let metadata_stored = metadata_stored.unwrap();
            assert_eq!(resource_id, metadata_stored.resource_id);
            assert_eq!(metadata.resource_type, metadata_stored.resource_type);
            assert_eq!(metadata.hash, metadata_stored.hash);
            assert_eq!(metadata.size, metadata_stored.size);
        }

        #[ink::test]
        fn create_offchain_data_with_duplicate_resource_ids_fails_with_err() {
            // Prepare
            let mut blbc = Blbc::default();
            let sample_offchain_data1 = get_sample_offchain_data1();
            let mut sample_offchain_data2 = get_sample_offchain_data2();
            // Deliberately change the resource ID of data2 to be the same as data1
            sample_offchain_data2.metadata.resource_id =
                sample_offchain_data1.metadata.resource_id.clone();

            // Invoke with data1 and expect the return value to be Ok()
            assert!(blbc
                .create_offchain_data(sample_offchain_data1, None)
                .is_ok());

            // Invoke with data2 and expect the return value to be Err()
            assert!(blbc
                .create_offchain_data(sample_offchain_data2, None)
                .is_err());
        }

        #[ink::test]
        fn test_get_metadata() {
            // Prepare
            let mut blbc = Blbc::default();
            let sample_plain_data1 = get_sample_plain_data1();
            let resource_id = sample_plain_data1.metadata.resource_id.clone();
            let extensions = sample_plain_data1.metadata.extensions.clone();
            let hash = sample_plain_data1.metadata.hash.clone();
            let size = sample_plain_data1.metadata.size.clone();
            let resource_type = sample_plain_data1.metadata.resource_type.clone();

            // Invoke with sample_plain_data1 and expect the return value to be Ok()
            assert!(blbc.create_plain_data(sample_plain_data1, None).is_ok());

            // Invoke get_metadata and expect the return value to be Ok()
            let res_meta_data = match blbc.get_metadata(resource_id.clone()) {
                Ok(b) => b,
                Err(msg) => panic!("{}", msg),
            };

            // Check if data is as expected
            assert_eq!(resource_id, res_meta_data.resource_id);
            assert_eq!(extensions, res_meta_data.extensions);
            assert_eq!(hash, res_meta_data.hash);
            assert_eq!(size, res_meta_data.size);
            assert_eq!(resource_type, res_meta_data.resource_type);
        }

        #[ink::test]
        fn test_get_metadata_with_non_existent_id() {
            // Prepare
            let mut blbc = Blbc::default();
            let sample_plain_data1 = get_sample_plain_data1();
            let resource_id = sample_plain_data1.metadata.resource_id.clone();
            let mut non_existent_resource_id = resource_id.clone();
            non_existent_resource_id.push_str("_non_existent");

            // Invoke with sample_plain_data1 and expect the return value to be Ok()
            assert!(blbc.create_plain_data(sample_plain_data1, None).is_ok());

            // Invoke with a non existent resource ID and expect the response status to be ERROR
            assert!(blbc.get_metadata(non_existent_resource_id).is_err());
        }

        #[ink::test]
        fn test_get_data() {
            // Prepare
            let mut blbc = Blbc::default();
            let sample_plain_data1 = get_sample_plain_data1();
            let resource_id = sample_plain_data1.metadata.resource_id.clone();
            let data_as_base64: String = sample_plain_data1.data.clone();

            // Invoke with sample_plain_data1 and expect the return value to be Ok()
            assert!(blbc.create_plain_data(sample_plain_data1, None).is_ok());

            // Invoke get_data and expect the return value to be Ok()
            let data_to_be_checked = match blbc.get_data(resource_id) {
                Ok(b) => b,
                Err(msg) => panic!("{}", msg),
            };

            // Check if the data in map is as expected
            assert_eq!(data_as_base64, data_to_be_checked);
        }

        #[ink::test]
        fn test_get_data_with_non_existent_id() {
            // Prepare
            let mut blbc = Blbc::default();
            let sample_plain_data1 = get_sample_plain_data1();
            let resource_id = sample_plain_data1.metadata.resource_id.clone();
            let mut non_existent_resource_id = resource_id.clone();
            non_existent_resource_id.push_str("_non_existent");

            // Invoke with sample_plain_data1 and expect the return value to be Ok()
            assert!(blbc.create_plain_data(sample_plain_data1, None).is_ok());

            // Invoke with a non existent resource ID and expect the response status to be ERROR
            assert!(blbc.get_data(non_existent_resource_id).is_err());
        }

        #[ink::test]
        fn test_get_key() {
            // Prepare
            let mut blbc = Blbc::default();
            let sample_encrypted_data1 = get_sample_encrypted_data1();
            let resource_id = sample_encrypted_data1.metadata.resource_id.clone();
            let key_as_base64: String = sample_encrypted_data1.key.clone();

            // Invoke with sample_encrypted_data1 and expect the return value to be Ok()
            assert!(blbc
                .create_encrypted_data(sample_encrypted_data1, None)
                .is_ok());

            // Invoke get_key and expect the return value to be Ok()
            let key_to_be_checked = match blbc.get_key(resource_id) {
                Ok(b) => b,
                Err(msg) => panic!("{}", msg),
            };

            // Check if the key in res_key_map is as expected
            assert_eq!(key_as_base64, key_to_be_checked);
        }

        #[ink::test]
        fn test_get_key_with_non_existent_id() {
            // Prepare
            let mut blbc = Blbc::default();
            let sample_encrypted_data1 = get_sample_encrypted_data1();
            let resource_id = sample_encrypted_data1.metadata.resource_id.clone();
            let mut non_existent_resource_id = resource_id.clone();
            non_existent_resource_id.push_str("_non_existent");

            // Invoke with sample_encrypted_data1 and expect the return value to be Ok()
            assert!(blbc
                .create_encrypted_data(sample_encrypted_data1, None)
                .is_ok());

            // Invoke with a non existent resource ID and expect the response status to be ERROR
            assert!(blbc.get_key(non_existent_resource_id).is_err());
        }

        #[ink::test]
        fn test_get_policy() {
            // Prepare
            let mut blbc = Blbc::default();
            let sample_encrypted_data1 = get_sample_encrypted_data1();
            let resource_id = sample_encrypted_data1.metadata.resource_id.clone();
            let policy = sample_encrypted_data1.policy.clone();

            // Invoke with sample_encrypted_data1 and expect the return value to be Ok()
            assert!(blbc
                .create_encrypted_data(sample_encrypted_data1, None)
                .is_ok());

            // Invoke get_policy and expect the return value to be Ok()
            let policy_to_be_checked = match blbc.get_policy(resource_id) {
                Ok(b) => b,
                Err(msg) => panic!("{}", msg),
            };

            // Check if the policy in res_policy_map is as expected
            assert_eq!(policy, policy_to_be_checked);

            // Prepare the arg with offchain data and do it again
            let sample_offchain_data1 = get_sample_offchain_data1();
            let resource_id2 = sample_offchain_data1.metadata.resource_id.clone();
            let policy2 = sample_offchain_data1.policy.clone();
            assert!(blbc
                .create_offchain_data(sample_offchain_data1, None)
                .is_ok());
            let policy_to_be_checked2 = match blbc.get_policy(resource_id2) {
                Ok(b) => b,
                Err(msg) => panic!("{}", msg),
            };
            assert_eq!(policy2, policy_to_be_checked2);
        }

        #[ink::test]
        fn test_get_policy_with_non_existent_id() {
            // Prepare
            let mut blbc = Blbc::default();
            let sample_encrypted_data1 = get_sample_encrypted_data1();
            let resource_id = sample_encrypted_data1.metadata.resource_id.clone();
            let mut non_existent_resource_id = resource_id.clone();
            non_existent_resource_id.push_str("_non_existent");

            // Invoke with sample_encrypted_data1 and expect the return value to be Ok()
            assert!(blbc
                .create_encrypted_data(sample_encrypted_data1, None)
                .is_ok());

            // Invoke with a non existent resource ID and expect the response status to be ERROR
            assert!(blbc.get_policy(non_existent_resource_id).is_err());
        }

        //此函数可以用来测试除 creator 以外的各种边界条件
        // #[ink::test]
        // fn test_list_resource_ids_by_creator() {
        //     // Prepare
        //     let mut blbc = Blbc::default();
        //     let sample_offchain_data1 = get_sample_offchain_data1();
        //     let sample_offchain_data2 = get_sample_offchain_data2();
        //     let sample_plain_data1 = get_sample_plain_data1();
        //     let sample_plain_data2 = get_sample_plain_data2();
        //     let sample_encrypted_data2 = get_sample_encrypted_data2();
        //     let sample_encrypted_data1 = get_sample_encrypted_data1();
        //
        //     // Invoke with sample_encrypted_data1 and expect the return value to be Ok()
        //     assert!(blbc.create_offchain_data(sample_offchain_data1, None).is_ok());
        //     assert!(blbc.create_encrypted_data(sample_encrypted_data2, None).is_ok());
        //     assert!(blbc.create_offchain_data(sample_offchain_data2, None).is_ok());
        //     assert!(blbc.create_encrypted_data(sample_encrypted_data1, None).is_ok());
        //     assert!(blbc.create_plain_data(sample_plain_data2, None).is_ok());
        //     assert!(blbc.create_plain_data(sample_plain_data1, None).is_ok());
        //     assert!(blbc.list_resource_ids_by_creator("documen".into(), false, 9, None).is_ok());
        //     assert!(blbc.list_resource_ids_by_creator("document".into(), true, 5, Some("201".into())).is_ok());
        // }

        // 关于时间的查询无法测试，其余查询条件已测试通过
        // #[ink::test]
        // fn test_list_resource_ids_by_conditions() {
        //     // Prepare
        //     let mut blbc = Blbc::default();
        //     let sample_offchain_data1 = get_sample_offchain_data1();
        //     let sample_offchain_data2 = get_sample_offchain_data2();
        //     let sample_plain_data1 = get_sample_plain_data1();
        //     let sample_plain_data2 = get_sample_plain_data2();
        //     let sample_encrypted_data2 = get_sample_encrypted_data2();
        //     let sample_encrypted_data1 = get_sample_encrypted_data1();
        //
        //     // Invoke with sample_encrypted_data1 and expect the return value to be Ok()
        //     assert!(blbc.create_offchain_data(sample_offchain_data1, None).is_ok());
        //     assert!(blbc.create_encrypted_data(sample_encrypted_data2, None).is_ok());
        //     assert!(blbc.create_offchain_data(sample_offchain_data2, None).is_ok());
        //     assert!(blbc.create_encrypted_data(sample_encrypted_data1, None).is_ok());
        //     assert!(blbc.create_plain_data(sample_plain_data2, None).is_ok());
        //     assert!(blbc.create_plain_data(sample_plain_data1, None).is_ok());
        //
        //     // let query_conditions1 = QueryConditions::DocumentQueryConditions(DocumentQueryConditions {
        //     //     common_query_conditions: CommonQueryConditions {
        //     //         is_desc: false,
        //     //         resource_id: None,
        //     //         is_name_exact: None,
        //     //         name: None,
        //     //         is_time_exact: None,
        //     //         time: None,
        //     //         time_after_inclusive: None,
        //     //         time_before_exclusive: None,
        //     //         last_resource_id: Some("".into()),
        //     //     },
        //     //     document_type: Some(DocumentType::UsageDocument),
        //     //     preceding_document_id: None,
        //     //     head_document_id: Some("1000".into()),
        //     //     entity_asset_id: None,
        //     // });
        //     // assert!(blbc.list_resource_ids_by_conditions(query_conditions1, 88).is_ok());
        //     let query_conditions2 = QueryConditions::EntityAssetQueryConditions(EntityAssetQueryConditions{
        //         common_query_conditions: CommonQueryConditions {
        //         is_desc: true,
        //         resource_id: None,
        //         is_name_exact: None,
        //         name: None,
        //         is_time_exact: None,
        //         time: None,
        //         time_after_inclusive: None,
        //         time_before_exclusive: None,
        //         last_resource_id: Some("201".into())
        //     }, design_document_id: Some("101".into()) });
        //     assert!(blbc.list_resource_ids_by_conditions(query_conditions2, 9).is_ok());
        // }


        /// 测试区分文档和资产 id
        // #[ink::test]
        // fn test_list_resource_ids_by_conditions_category() {
        //     // Prepare
        //     let mut blbc = Blbc::default();
        //     let sample_offchain_data1 = get_sample_offchain_data1();
        //     let sample_offchain_data2 = get_sample_offchain_data2();
        //     let sample_plain_data1 = get_sample_plain_data1();
        //     let sample_plain_data2 = get_sample_plain_data2();
        //     let sample_encrypted_data2 = get_sample_encrypted_data2();
        //     let sample_encrypted_data1 = get_sample_encrypted_data1();
        //
        //     // Invoke with sample_encrypted_data1 and expect the return value to be Ok()
        //     assert!(blbc.create_offchain_data(sample_offchain_data1, None).is_ok());
        //     assert!(blbc.create_encrypted_data(sample_encrypted_data2, None).is_ok());
        //     assert!(blbc.create_offchain_data(sample_offchain_data2, None).is_ok());
        //     assert!(blbc.create_encrypted_data(sample_encrypted_data1, None).is_ok());
        //     assert!(blbc.create_plain_data(sample_plain_data2, None).is_ok());
        //     assert!(blbc.create_plain_data(sample_plain_data1, None).is_ok());
        //
        //     let query_conditions1 = QueryConditions::DocumentQueryConditions(DocumentQueryConditions {
        //         common_query_conditions: CommonQueryConditions {
        //             is_desc: false,
        //             resource_id: None,
        //             is_name_exact: None,
        //             name: None,
        //             is_time_exact: None,
        //             time: None,
        //             time_after_inclusive: None,
        //             time_before_exclusive: None,
        //             last_resource_id: Some("".into()),
        //         },
        //         document_type: Some(DocumentType::UsageDocument),
        //         preceding_document_id: None,
        //         head_document_id: Some("1000".into()),
        //         entity_asset_id: None,
        //     });
        //     assert!(blbc.list_resource_ids_by_conditions(query_conditions1, 88).is_ok());
        //     // let query_conditions2 = QueryConditions::EntityAssetQueryConditions(EntityAssetQueryConditions{
        //     //     common_query_conditions: CommonQueryConditions {
        //     //     is_desc: true,
        //     //     resource_id: None,
        //     //     is_name_exact: None,
        //     //     name: None,
        //     //     is_time_exact: None,
        //     //     time: None,
        //     //     time_after_inclusive: None,
        //     //     time_before_exclusive: None,
        //     //     last_resource_id: Some("201".into())
        //     // }, design_document_id: Some("101".into()) });
        //     // assert!(blbc.list_resource_ids_by_conditions(query_conditions2, 9).is_ok());
        // }

        #[ink::test]
        fn test_create_auth_request_with_encrypted_data() {
            // 初始化
            let mut blbc = Blbc::default();
            // 创建加密文档
            let auth_session_id: String = "1".into();
            let sample_encrypted_data1 = get_sample_encrypted_data1();
            let resource_id = sample_encrypted_data1.metadata.resource_id.clone();
            assert!(blbc
                .create_encrypted_data(sample_encrypted_data1, None)
                .is_ok());
            // 创建授权请求
            let sample_auth_request1 = get_sample_auth_request1(resource_id.clone());
            assert!(blbc
                .create_auth_request(auth_session_id.clone(), sample_auth_request1.clone(), None)
                .is_ok());
            // 验证
            let auth_request_to_be_checked = match blbc.get_auth_request(auth_session_id.clone()) {
                Ok(a) => a,
                Err(msg) => {
                    panic!("{}", msg)
                }
            };
            assert_eq!(auth_session_id, auth_request_to_be_checked.auth_session_id);
            assert_eq!(resource_id, auth_request_to_be_checked.resource_id);
            assert_eq!(
                sample_auth_request1.extensions,
                auth_request_to_be_checked.extensions
            );
        }

        #[ink::test]
        fn test_create_auth_request_with_offchain_data() {
            // 初始化
            let mut blbc = Blbc::default();
            // 创建链下数据
            let auth_session_id: String = "1".into();
            let sample_offchain_data1 = get_sample_offchain_data1();
            let resource_id = sample_offchain_data1.metadata.resource_id.clone();
            assert!(blbc
                .create_offchain_data(sample_offchain_data1, None)
                .is_ok());
            // 创建授权请求
            let sample_auth_request1 = get_sample_auth_request1(resource_id.clone());
            assert!(blbc
                .create_auth_request(auth_session_id.clone(), sample_auth_request1.clone(), None)
                .is_ok());
            // 验证
            let auth_request_to_be_checked = match blbc.get_auth_request(auth_session_id.clone()) {
                Ok(a) => a,
                Err(msg) => {
                    panic!("{}", msg)
                }
            };
            assert_eq!(auth_session_id, auth_request_to_be_checked.auth_session_id);
            assert_eq!(resource_id, auth_request_to_be_checked.resource_id);
            assert_eq!(
                sample_auth_request1.extensions,
                auth_request_to_be_checked.extensions
            );
        }

        #[ink::test]
        fn test_create_auth_request_with_plain_data() {
            // 初始化
            let mut blbc = Blbc::default();
            // 创建明文文档
            let auth_session_id: String = "1".into();
            let sample_plain_data1 = get_sample_plain_data1();
            let resource_id = sample_plain_data1.metadata.resource_id.clone();
            assert!(blbc.create_plain_data(sample_plain_data1, None).is_ok());
            // 创建授权请求
            let sample_auth_request1 = get_sample_auth_request1(resource_id.clone());
            // 期待状态为 ERROR
            assert!(blbc
                .create_auth_request(auth_session_id, sample_auth_request1.clone(), None)
                .is_err());
        }

        #[ink::test]
        fn test_create_auth_request_with_non_existent_id() {
            // Prepare
            let mut blbc = Blbc::default();
            let auth_session_id: String = "1".into();
            let sample_auth_request1 = get_sample_auth_request1("NON_EXISTENT_RESOURCE_ID".into());
            // 期待状态为 ERROR
            assert!(blbc
                .create_auth_request(auth_session_id, sample_auth_request1.clone(), None)
                .is_err());
        }

        #[ink::test]
        fn test_create_auth_response_with_normal_process() {
            // 初始化
            let mut blbc = Blbc::default();
            // user1 创建加密数据
            let auth_session_id: String = "1".into();
            let sample_encrypted_data = get_sample_encrypted_data1();
            let resource_id = sample_encrypted_data.metadata.resource_id.clone();
            assert!(blbc
                .create_encrypted_data(sample_encrypted_data, None)
                .is_ok());
            // user1 创建授权请求
            let sample_auth_request1 = get_sample_auth_request1(resource_id.clone());
            assert!(blbc
                .create_auth_request(auth_session_id.clone(), sample_auth_request1.clone(), None)
                .is_ok());
            // user1 创建授权批复
            let sample_auth_response1 = get_sample_auth_response1(resource_id.clone());
            assert!(blbc
                .create_auth_response(auth_session_id.clone(), sample_auth_response1.clone(), None)
                .is_ok());
            // 验证
            let auth_response_to_be_checked = match blbc.get_auth_response(auth_session_id.clone())
            {
                Ok(a) => a,
                Err(msg) => {
                    panic!("{}", msg)
                }
            };
            assert_eq!(auth_session_id, auth_response_to_be_checked.auth_session_id);
            assert_eq!(
                sample_auth_response1.result,
                auth_response_to_be_checked.result
            );
        }

        #[ink::test]
        fn test_create_auth_response_twice() {
            // 初始化
            let mut blbc = Blbc::default();
            // user1 创建加密数据
            let auth_session_id: String = "1".into();
            let sample_encrypted_data = get_sample_encrypted_data1();
            let resource_id = sample_encrypted_data.metadata.resource_id.clone();
            assert!(blbc
                .create_encrypted_data(sample_encrypted_data, None)
                .is_ok());
            // user1 创建授权请求
            let sample_auth_request1 = get_sample_auth_request1(resource_id.clone());
            assert!(blbc
                .create_auth_request(auth_session_id.clone(), sample_auth_request1.clone(), None)
                .is_ok());
            // user1 创建授权批复
            let sample_auth_response1 = get_sample_auth_response1(resource_id.clone());
            assert!(blbc
                .create_auth_response(auth_session_id.clone(), sample_auth_response1.clone(), None)
                .is_ok());
            // user1 再次创建授权批复, 期待状态为 ERR
            assert!(blbc
                .create_auth_response(auth_session_id.clone(), sample_auth_response1, None)
                .is_err());
        }

        #[ink::test]
        fn test_create_auth_response_with_non_existent_session_id() {
            // 初始化
            let mut blbc = Blbc::default();
            // user1 创建加密数据
            let auth_session_id: String = "1".into();
            let sample_encrypted_data = get_sample_encrypted_data1();
            let resource_id = sample_encrypted_data.metadata.resource_id.clone();
            assert!(blbc
                .create_encrypted_data(sample_encrypted_data, None)
                .is_ok());
            // user1 直接创建授权批复，期待状态为 ERR
            let sample_auth_response1 = get_sample_auth_response1(resource_id.clone());
            assert!(blbc
                .create_auth_response(auth_session_id, sample_auth_response1.clone(), None)
                .is_err());
        }

        #[ink::test]
        fn test_get_auth_request_with_normal_parameters() {
            // 初始化
            let mut blbc = Blbc::default();
            // user1 创建加密数据
            let auth_session_id: String = "1".into();
            let sample_encrypted_data = get_sample_encrypted_data1();
            let resource_id = sample_encrypted_data.metadata.resource_id.clone();
            assert!(blbc
                .create_encrypted_data(sample_encrypted_data, None)
                .is_ok());
            // user1 创建授权请求
            let sample_auth_request1 = get_sample_auth_request1(resource_id.clone());
            assert!(blbc
                .create_auth_request(auth_session_id.clone(), sample_auth_request1.clone(), None)
                .is_ok());
            // 验证
            let auth_request_to_be_checked = match blbc.get_auth_request(auth_session_id.clone()) {
                Ok(a) => a,
                Err(msg) => {
                    panic!("{}", msg)
                }
            };
            assert_eq!(auth_session_id, auth_request_to_be_checked.auth_session_id);
            assert_eq!(
                sample_auth_request1.resource_id,
                auth_request_to_be_checked.resource_id
            );
            assert_eq!(
                sample_auth_request1.extensions,
                auth_request_to_be_checked.extensions
            );
        }

        #[ink::test]
        fn test_get_auth_request_with_non_existent_session_id() {
            // 初始化
            let mut blbc = Blbc::default();
            // 直接调用，期待状态为 ERROR 且错误内容为 codeNotFound
            assert_eq!(
                Err(error_code::CODE_NOT_FOUND.into()),
                blbc.get_auth_request("01".into())
            );
        }

        // #[ink::test]
        // fn test_create_key_switch_result_with_normal_process() {
        //     // 初始化
        //     let mut blbc = Blbc::default();
        //     let ks_result = KeySwitchResult{
        //         key_switch_session_id: "0987645".to_string(),
        //         share: "".to_string(),
        //         zk_proof: "".to_string(),
        //         key_switch_pk: "".to_string()
        //     };
        //     // 直接调用，期待状态为 ERROR 且错误内容为 codeNotFound
        //     assert_eq!(
        //         Err(error_code::CODE_NOT_FOUND.into()),
        //         blbc.create_key_switch_result(ks_result)
        //     );
        // }


        const DATA1: &str = "data1";
        const DATA2: &str = "data2";
        //const DATA3: &str = "data3";

        // 明文数据
        // 资源 ID: "001"
        // 名称: "Sample Plain Data 1"
        // 内容: base64(data1)
        fn get_sample_plain_data1() -> PlainData {
            use ink_prelude::string::String;

            let mut hasher = Sha256::new();
            hasher.update(DATA1.as_bytes());
            let hash_bytes = hasher.finalize();
            let extension_map: BTreeMap<String, String> = BTreeMap::from([
                ("dataType".into(), "Document".into()),
                ("name".into(), "Sample PlainData 1".into()),
                ("documentType".into(), DocumentType::DesignDocument.into()),
                ("headDocumentId".into(), "1000".into()),
                ("designDocumentId".into(), "101".into()),
            ]);

            return PlainData {
                metadata: ResMetadata {
                    resource_type: ResourceType::Plain,
                    resource_id: "001".into(),
                    hash: base64::encode(hash_bytes),
                    size: DATA1.as_bytes().len() as u64,
                    extensions: extension_map,
                },
                data: base64::encode(DATA1.as_bytes()),
            };
        }

        // 明文数据
        // 资源 ID: "002"
        // 名称: "示例明文数据2"
        // 内容: base64(data2)
        fn get_sample_plain_data2() -> PlainData {
            use ink_prelude::string::String;

            let mut hasher = Sha256::new();
            hasher.update(DATA2.as_bytes());
            let hash_bytes = hasher.finalize();
            let extension_map: BTreeMap<String, String> = BTreeMap::from([
                ("dataType".into(), "EntityAsset".into()),
                ("name".into(), "示例明文数据2".into()),
                ("documentType".into(), DocumentType::TransferDocument.into()),
                ("headDocumentId".into(), "1000".into()),
                ("designDocumentId".into(), "101".into()),
            ]);

            return PlainData {
                metadata: ResMetadata {
                    resource_type: ResourceType::Plain,
                    resource_id: "002".into(),
                    hash: base64::encode(hash_bytes),
                    size: DATA2.as_bytes().len() as u64,
                    extensions: extension_map,
                },
                data: base64::encode(DATA2.as_bytes()),
            };
        }

        // 加密数据
        // 资源 ID: "101"
        // 名称: "Sample Encrypted Data 1"
        // 内容: base64(encrypt(data1))
        fn get_sample_encrypted_data1() -> EncryptedData {
            use ink_prelude::string::String;

            let mut hasher = Sha256::new();
            hasher.update(DATA1.as_bytes());
            let hash_bytes = hasher.finalize();
            let extension_map: BTreeMap<String, String> = BTreeMap::from([
                ("dataType".into(), "EntityAsset".into()),
                ("name".into(), "Sample Encrypted Data 1".into()),
                ("documentType".into(), DocumentType::UsageDocument.into()),
                ("headDocumentId".into(), "1000".into()),
                ("designDocumentId".into(), "101".into()),
            ]);

            return EncryptedData {
                metadata: ResMetadata {
                    resource_type: ResourceType::Encrypted,
                    resource_id: "101".into(),
                    hash: base64::encode(hash_bytes),
                    size: DATA1.as_bytes().len() as u64,
                    extensions: extension_map,
                },
                // 加解密都在链码外进行
                data: base64::encode(DATA1.as_bytes()),
                key: base64::encode("123456".as_bytes()),
                policy: "(DeptType == \"computer\" && DeptLevel == 2)".into(),
            };
        }

        // 加密数据
        // 资源 ID: "102"
        // 名称: "示例加密数据2"
        // 内容: base64(encrypt(data2))
        fn get_sample_encrypted_data2() -> EncryptedData {
            use ink_prelude::string::String;

            let mut hasher = Sha256::new();
            hasher.update(DATA2.as_bytes());
            let hash_bytes = hasher.finalize();
            let extension_map: BTreeMap<String, String> = BTreeMap::from([
                ("dataType".into(), "Document".into()),
                ("name".into(), "示例加密数据2".into()),
                ("documentType".into(), DocumentType::UsageDocument.into()),
                ("headDocumentId".into(), "1000".into()),
                ("designDocumentId".into(), "101".into()),
            ]);

            return EncryptedData {
                metadata: ResMetadata {
                    resource_type: ResourceType::Encrypted,
                    resource_id: "102".into(),
                    hash: base64::encode(hash_bytes),
                    size: DATA2.as_bytes().len() as u64,
                    extensions: extension_map,
                },
                // 加解密都在链码外进行
                data: base64::encode(DATA2.as_bytes()),
                key: base64::encode("123456".as_bytes()),
                policy: "(DeptType == \"computer\" && DeptLevel == 1)".into(),
            };
        }

        // 链下数据
        // 资源 ID: "201"
        // 名称: "Sample Offchain Data 1"
        fn get_sample_offchain_data1() -> OffchainData {
            use ink_prelude::string::String;

            let mut hasher = Sha256::new();
            hasher.update(DATA1.as_bytes());
            let hash_bytes = hasher.finalize();
            let extension_map: BTreeMap<String, String> = BTreeMap::from([
                ("dataType".into(), "EntityAsset".into()),
                ("name".into(), "Sample Offchain Data 1".into()),
                ("documentType".into(), DocumentType::RepairDocument.into()),
                ("headDocumentId".into(), "1000".into()),
                ("designDocumentId".into(), "101".into()),
            ]);

            return OffchainData {
                metadata: ResMetadata {
                    resource_type: ResourceType::Offchain,
                    resource_id: "201".into(),
                    hash: base64::encode(hash_bytes),
                    size: DATA1.as_bytes().len() as u64,
                    extensions: extension_map,
                },
                cid: "654321".into(),
                key: base64::encode("123456".as_bytes()),
                policy: "Encryption strategy".into(),
            };
        }

        // 链下数据
        // 资源 ID: "202"
        // 名称: "示例链下数据2"
        fn get_sample_offchain_data2() -> OffchainData {
            use ink_prelude::string::String;

            let mut hasher = Sha256::new();
            hasher.update(DATA2.as_bytes());
            let hash_bytes = hasher.finalize();
            let extension_map: BTreeMap<String, String> = BTreeMap::from([
                ("dataType".into(), "EntityAsset".into()),
                ("name".into(), "示例链下数据2".into()),
                (
                    "documentType".into(),
                    DocumentType::ProductionDocument.into(),
                ),
                ("headDocumentId".into(), "10001".into()),
                ("designDocumentId".into(), "101".into()),
            ]);

            return OffchainData {
                metadata: ResMetadata {
                    resource_type: ResourceType::Offchain,
                    resource_id: "202".into(),
                    hash: base64::encode(hash_bytes),
                    size: DATA2.as_bytes().len() as u64,
                    extensions: extension_map,
                },
                cid: "654321".into(),
                key: base64::encode("123456".as_bytes()),
                policy: "Encryption strategy".into(),
            };
        }

        fn get_sample_auth_request1(resource_id: String) -> AuthRequest {
            let extension_map: BTreeMap<String, String> = BTreeMap::from([
                ("dataType".into(), "authRequest".into()),
                ("name".into(), "sampleAuthRequest1".into()),
            ]);

            return AuthRequest {
                resource_id,
                extensions: extension_map,
            };
        }

        fn get_sample_auth_request2(resource_id: String) -> AuthRequest {
            let extension_map: BTreeMap<String, String> = BTreeMap::from([
                ("dataType".into(), "authRequest".into()),
                ("name".into(), "sampleAuthRequest2".into()),
            ]);

            return AuthRequest {
                resource_id,
                extensions: extension_map,
            };
        }

        fn get_sample_auth_response1(resource_id: String) -> AuthResponse {
            let extension_map: BTreeMap<String, String> =
                BTreeMap::from([("dataType".into(), "authResponse".into())]);

            return AuthResponse {
                auth_session_id: "1".into(),
                extensions: extension_map,
                result: true,
            };
        }

        fn get_sample_auth_response2(resource_id: String) -> AuthResponse {
            let extension_map: BTreeMap<String, String> =
                BTreeMap::from([("dataType".into(), "authResponse".into())]);

            return AuthResponse {
                auth_session_id: "2".into(),
                extensions: extension_map,
                result: false,
            };
        }
    }
}
