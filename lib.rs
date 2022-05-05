#![cfg_attr(not(feature = "std"), no_std)]

pub mod data;
pub mod error_code;
pub mod extension;
pub mod model;

use ink_lang as ink;

#[ink::contract]
mod blbc {
    use crate::{
        data, error_code,
        model::data::{PlainData, EncryptedData, OffchainData, ResMetadataStored},
    };
    use ink_prelude::string::String;
    use ink_prelude::vec::Vec;
    use ink_storage::{traits::SpreadAllocate, Mapping};


    #[ink(storage)]
    #[derive(SpreadAllocate)]
    pub struct Blbc {
        /// 存储通过资源 ID 可以找到的链上资源，资源内容是字节数组
        pub res_map: Mapping<String, Vec<u8>>,
        /// 存储通过资源 ID 可以找到的资源元数据
        pub res_metadata_map: Mapping<String, ResMetadataStored>,
        /// 存储通过资源 ID 可以找到的对称密钥
        pub res_key_map: Mapping<String, Vec<u8>>,
        /// 存储通过资源 ID 可以找到的策略
        pub res_policy_map: Mapping<String, String>,
    }

    #[ink(event)]
    pub struct ResourceCreated {
        pub event_id: String,
        pub resource_id: String,
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
        use alloc::collections::BTreeMap;
        use std::iter::FromIterator;
        /// Imports `ink_lang` so we can use `#[ink::test]`.
        use ink_lang as ink;
        use sha2::{Digest, Sha256};

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
            assert!(blbc.create_encrypted_data(sample_encrypted_data1, None).is_ok());

            // Check if the data in maps is as expected
            assert_eq!(
                blbc.res_map.get(&resource_id),
                Some(DATA1.as_bytes().to_owned())
            );
            assert_eq!(
                blbc.res_key_map.get(&resource_id),
                Some(key_decoded.to_owned())
            );
            assert_eq!(
                blbc.res_policy_map.get(&resource_id),
                Some(policy)
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
        fn create_encrypted_data_with_duplicate_resource_ids_fails_with_err() {
            // Prepare
            let mut blbc = Blbc::default();
            let sample_encrypted_data1 = get_sample_encrypted_data1();
            let mut sample_encrypted_data2 = get_sample_encrypted_data2();
            // Deliberately change the resource ID of data2 to be the same as data1
            sample_encrypted_data2.metadata.resource_id =
                sample_encrypted_data1.metadata.resource_id.clone();

            // Invoke with data1 and expect the return value to be Ok()
            assert!(blbc.create_encrypted_data(sample_encrypted_data1, None).is_ok());

            // Invoke with data2 and expect the return value to be Err()
            assert!(blbc.create_encrypted_data(sample_encrypted_data2, None).is_err());
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
            assert!(blbc.create_offchain_data(sample_offchain_data1, None).is_ok());

            // Check if the data in maps is as expected
            assert_eq!(
                blbc.res_map.get(&resource_id),
                Some(Vec::from_iter(cid.into_bytes()))
            );
            assert_eq!(
                blbc.res_key_map.get(&resource_id),
                Some(key_decoded.to_owned())
            );
            assert_eq!(
                blbc.res_policy_map.get(&resource_id),
                Some(policy)
            );

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
            assert!(blbc.create_offchain_data(sample_offchain_data1, None).is_ok());

            // Invoke with data2 and expect the return value to be Err()
            assert!(blbc.create_offchain_data(sample_offchain_data2, None).is_err());
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
            assert_eq!(
                data_as_base64,
                data_to_be_checked
            );
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
            assert!(blbc.create_encrypted_data(sample_encrypted_data1, None).is_ok());

            // Invoke get_key and expect the return value to be Ok()
            let key_to_be_checked = match blbc.get_key(resource_id) {
                Ok(b) => b,
                Err(msg) => panic!("{}", msg),
            };

            // Check if the key in res_key_map is as expected
            assert_eq!(
                key_as_base64,
                key_to_be_checked
            );
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
            assert!(blbc.create_encrypted_data(sample_encrypted_data1, None).is_ok());

            // Invoke with a non existent resource ID and expect the response status to be ERROR
            assert!(blbc.get_key(non_existent_resource_id).is_err());
        }


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
                ("dataType".into(), "document".into()),
                ("name".into(), "Sample PlainData 1".into()),
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
                ("dataType".into(), "document".into()),
                ("name".into(), "示例明文数据2".into()),
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
                ("dataType".into(), "document".into()),
                ("name".into(), "Sample Encrypted Data 1".into()),
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
                ("dataType".into(), "document".into()),
                ("name".into(), "示例加密数据2".into()),
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
                ("dataType".into(), "document".into()),
                ("name".into(), "Sample Offchain Data 1".into()),
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
                ("dataType".into(), "document".into()),
                ("name".into(), "示例链下数据2".into()),
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
    }
}
