use std::path::PathBuf;
use std::rc::Rc;
use std::{collections::HashMap, path::Path};

/// A caching mechanism to prevent loading the same file multiple times
/// Not (yet) thread safe, so beware
#[derive(Clone, Debug)]
pub struct FileCache {
    files: HashMap<PathBuf, Rc<String>>,
}

impl FileCache {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
        }
    }
    pub fn get(&self, path: &PathBuf) -> Option<Rc<String>> {
        match self.files.get(path) {
            Some(f) => Some(f.clone()),
            None => None,
        }
    }

    /// Load a file if it isn't already in the cache.
    /// Should be used instead of any other loading mechanism
    pub fn load<P: AsRef<Path>>(&mut self, path: P) -> std::io::Result<Rc<String>> {
        let buf = path.as_ref().to_path_buf();
        if let Some(file) = self.files.get(path.as_ref()) {
            return Ok(file.clone());
        }
        let file = std::fs::read_to_string(path)?;
        let file = Rc::new(file);
        self.files.insert(buf.to_path_buf(), file.clone());

        return Ok(file);
    }
}
