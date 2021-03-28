use fasthash::city;
use hashbrown;

pub type HashMap<K, V> = hashbrown::HashMap<K, V>;

pub fn new<K, V>() -> HashMap<K, V> {
    HashMap::new()
}

pub fn hash(s: &str) -> u32 {
    city::hash32(s)
}
