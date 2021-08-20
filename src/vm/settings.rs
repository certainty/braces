use rustc_hash::FxHashMap;

#[derive(Debug)]
pub struct Settings {
    inner: FxHashMap<Setting, bool>,
}

impl Settings {
    pub fn new() -> Self {
        Self {
            inner: FxHashMap::default(),
        }
    }

    pub fn enable(&mut self, setting: Setting) {
        self.inner.insert(setting, true);
    }

    pub fn disable(&mut self, setting: Setting) {
        self.inner.insert(setting, false);
    }

    pub fn is_enabled(&self, setting: &Setting) -> bool {
        match self.inner.get(setting) {
            Some(v) => *v,
            _ => false,
        }
    }

    pub fn as_vec(&self) -> Vec<(Setting, bool)> {
        self.inner
            .iter()
            .map(|p| (p.0.clone(), *p.1))
            .collect::<Vec<_>>()
    }
}

impl Default for Settings {
    fn default() -> Settings {
        let mut settings = Settings::new();

        settings.disable(Setting::Debug);

        settings
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Setting {
    Debug,
}

impl std::fmt::Display for Setting {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        let name = match self {
            Setting::Debug => "debug",
        };

        fmt.write_str(name)
    }
}
