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
    use ink_storage::Box;

    #[ink(storage)]
    pub struct Blbc {
        /// 存储通过资源 ID 可以找到的链上资源，资源内容是字节数组
        res_map: HashMap<String, Box<Vec<u8>>>,
        /// 存储通过资源 ID 可以找到的资源元数据
        res_metadata_map: HashMap<String, ResMetadataStored>,
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
            self.res_map
                .insert(resource_id.clone(), Box::new(data_bytes));
            self.res_metadata_map
                .insert(resource_id.clone(), metadata_stored);

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

        /// Imports `ink_lang` so we can use `#[ink::test]`.
        use ink_lang as ink;

        /// We test if the default constructor does its job.
        #[ink::test]
        fn default_works() {
            let blbc = Blbc::default();
            assert_eq!(blbc.get(), false);
        }

        /// We test a simple use case of our contract.
        #[ink::test]
        fn it_works() {
            let mut blbc = Blbc::new(false);
            assert_eq!(blbc.get(), false);
            blbc.flip();
            assert_eq!(blbc.get(), true);
        }
    }
}
