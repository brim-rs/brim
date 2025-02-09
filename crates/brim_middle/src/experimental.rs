use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Experimental {
    pub generics: bool,
}

impl Default for Experimental {
    fn default() -> Self {
        Self { generics: false }
    }
}