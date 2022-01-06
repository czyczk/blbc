#![cfg_attr(not(feature = "std"), no_std)]

pub mod error_code;
pub mod extension;
pub mod model;

use ink_lang as ink;

#[ink::contract(dynamic_storage_allocator = true)]
mod blbc {
    use crate::error_code;
    use crate::model::data::{PlainData, ResMetadataStored};
    use core::iter::FromIterator;
    use ink_env::hash::Sha2x256;
    use ink_prelude::format;
    use ink_prelude::string::String;
    use ink_prelude::vec::Vec;
    use ink_storage::collections::HashMap;

    #[ink(storage)]
    pub struct Blbc {
        /// 存储通过资源 ID 可以找到的链上资源，资源内容是字节数组
        res_map: HashMap<String, Vec<u8>>,
        /// 存储通过资源 ID 可以找到的资源元数据
        res_metadata_map: HashMap<String, ResMetadataStored>,
    }

    #[ink(event)]
    pub struct ResourceCreated {
        event_id: String,
        resource_id: String,
    }

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
            // 检查资源 ID 是否被占用
            let resource_id = &plain_data.metadata.resource_id;
            if self.res_metadata_map.contains_key(resource_id) {
                return Err(format!("资源 ID '{}' 已被占用", resource_id));
            }

            // 将数据本体从 Base64 解码
            let data_bytes = match base64::decode(plain_data.data) {
                Ok(b) => Vec::from_iter(b),
                Err(err) => return Err(format!("无法解析数据本体: {}", err)),
            };

            // 计算哈希和大小并检查是否与用户提供的值相同
            let size_stored = data_bytes.len() as u64;
            if size_stored != plain_data.metadata.size {
                return Err(format!(
                    "大小不匹配，应有大小为 {}，实际大小为 {}",
                    plain_data.metadata.size, size_stored
                ));
            }

            let hash_stored = self.env().hash_bytes::<Sha2x256>(&data_bytes);
            let hash_stored_base64 = base64::encode(hash_stored);
            if hash_stored_base64 != plain_data.metadata.hash {
                return Err("哈希不匹配".into());
            }

            // 获取创建者与时间戳
            let creator = self.env().caller();
            let timestamp = self.env().block_timestamp();

            // 准备存储元数据
            let metadata_stored = ResMetadataStored {
                resource_type: plain_data.metadata.resource_type,
                resource_id: resource_id.clone(),
                hash: plain_data.metadata.hash,
                size: plain_data.metadata.size,
                extensions: plain_data.metadata.extensions,
                creator,
                timestamp,
                block_number: self.env().block_number(),
                hash_stored: hash_stored_base64,
                size_stored,
            };

            // 存储数据
            self.res_map.insert(resource_id.clone(), data_bytes);
            self.res_metadata_map
                .insert(resource_id.clone(), metadata_stored);

            if let Some(event_id) = event_id {
                self.env().emit_event(ResourceCreated {
                    event_id,
                    resource_id: resource_id.clone(),
                });
            }

            return Ok(());
        }

        #[ink(message)]
        pub fn get_metadata(&self, resource_id: String) -> Result<ResMetadataStored, String> {
            // 读 metadata 并返回，若未找到则返回 CODE_NOT_FOUND
            let metadata = match self.res_metadata_map.get(&resource_id) {
                Some(it) => it,
                None => return Err(error_code::CODE_NOT_FOUND.into()),
            };

            // 合约现还不支持范型，故不能指定 lifetime，只能把有所有权的东西传出。
            return Ok(metadata.clone());
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
