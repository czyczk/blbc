#![cfg_attr(not(feature = "std"), no_std)]

pub mod data;
pub mod error_code;
pub mod extension;
pub mod model;

use ink_lang as ink;

#[ink::contract(dynamic_storage_allocator = true)]
mod blbc {
    use crate::{
        data, error_code,
        model::data::{PlainData, ResMetadataStored},
    };
    use ink_prelude::string::String;
    use ink_prelude::vec::Vec;
    use ink_storage::collections::HashMap;

    #[ink(storage)]
    pub struct Blbc {
        /// 存储通过资源 ID 可以找到的链上资源，资源内容是字节数组
        pub res_map: HashMap<String, Vec<u8>>,
        /// 存储通过资源 ID 可以找到的资源元数据
        pub res_metadata_map: HashMap<String, ResMetadataStored>,
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
            Self {
                res_map: Default::default(),
                res_metadata_map: Default::default(),
            }
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
        /// Imports `ink_lang` so we can use `#[ink::test]`.
        use ink_lang as ink;
        use sha2::{Digest, Sha256};

        #[ink::test]
        fn default_works() {
            let blbc = Blbc::default();
            assert_eq!(blbc.res_map.len(), 0);
            assert_eq!(blbc.res_metadata_map.len(), 0);
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
                Some(&DATA1.as_bytes().to_owned())
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

        const DATA1: &str = "data1";
        const DATA2: &str = "data2";
        const DATA3: &str = "data3";

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
    }
}
