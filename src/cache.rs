use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// A caching mechanism to prevent loading the same file multiple times
/// Not (yet) thread safe, so beware
#[derive(Clone, Debug)]
pub struct FileCache {
    files: HashMap<PathBuf, Arc<String>>,
}

impl FileCache {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
        }
    }
    pub fn get(&self, path: &Path) -> Option<Arc<String>> {
        self.files.get(path).cloned()
    }

    /// Load a file if it isn't already in the cache.
    /// Should be used instead of any other loading mechanism
    pub fn load<P: AsRef<Path>>(&mut self, path: P) -> std::io::Result<Arc<String>> {
        let buf = path.as_ref().to_path_buf();
        if let Some(file) = self.files.get(path.as_ref()) {
            return Ok(file.clone());
        }
        let file = std::fs::read_to_string(path)?;
        let file = Arc::new(file);
        self.files.insert(buf, file.clone());

        Ok(file)
    }
}
