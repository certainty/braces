use fasthash::city;
use rustc_hash::FxHashMap;

pub type HashMap<K, V> = FxHashMap<K, V>;

pub fn new<K, V>() -> HashMap<K, V> {
    FxHashMap::default()
}

pub fn hash(s: &str) -> u32 {
    city::hash32(s)
}
