use std::{path::PathBuf, rc::Rc};

use crate::cache::FileCache;

/// An abstraction for handling source code.
/// A `Source` can either be a file, or arbitrary text.
#[derive(Clone, Debug)]
pub enum Source {
    File(PathBuf),
    Text(Rc<String>),
}

impl Source {
    pub fn get(&self, cache: &FileCache) -> Rc<String> {
        match self {
            Source::File(path) => cache.get(&path).unwrap().clone(),
            Source::Text(t) => t.clone(),
        }
    }
}
