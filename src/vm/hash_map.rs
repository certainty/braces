use fasthash::{metro, MetroHasher};
use hashbrown;

pub type HashMap<K, V> = hashbrown::HashMap<K, V>;

pub fn new<K, V>() -> HashMap<K, V> {
    HashMap::new()
}

pub fn hash(s: &str) -> u64 {
    metro::hash64(s)
}
