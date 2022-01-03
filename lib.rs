#![cfg_attr(not(feature = "std"), no_std)]

pub mod extension;
pub mod model;

use ink_lang as ink;

#[ink::contract(dynamic_storage_allocator = true)]
mod blbc {
    use crate::model::data::{PlainData, ResMetadataStored};
    use ink_env::hash::Sha2x256;
    use ink_prelude::string::String;
    use ink_storage::collections::{HashMap, Vec};
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
                Ok(b) => b,
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

            return Ok(());
        }

        /// Simply returns the current value of our `bool`.
        #[ink(message)]
        pub fn get(&self) -> bool {
            self.value
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
