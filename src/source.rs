use std::{path::PathBuf, sync::Arc};

use crate::cache::FileCache;

/// An abstraction for handling source code.
/// A `Source` can either be a file, or arbitrary text.
#[derive(Clone, Debug)]
pub enum Source {
    File(PathBuf),
    Text(Arc<String>),
}

impl Source {
    pub fn get(&self, cache: &FileCache) -> Arc<String> {
        match self {
            Source::File(path) => cache.get(path).unwrap(),
            Source::Text(t) => t.clone(),
        }
    }

    pub fn slice(&self, cache: &FileCache, slice: (usize, usize)) -> String {
        let got = self.get(cache);
        String::from(&got[slice.0..slice.1])
    }
}
