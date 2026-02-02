use std::fs;
use std::io;
use std::ops::Range;
use std::path::Path;

/// Represents a span of content in some source
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,     // start of next character
    source_id: SourceId
}

impl Span {
    pub fn new(start: usize, end: usize, source_id: SourceId) -> Self {
        Self { start, end, source_id }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn source_id(&self) -> SourceId {
        self.source_id
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn range(&self) -> Range<usize> {
        self.start..self.end
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceKind {
    Text
}

/// Represents a code source.
#[derive(Debug, Clone, PartialEq)]
pub struct Source {
    path: String,
    kind: SourceKind
}

impl Source {
    pub fn new(path: String, kind: SourceKind) -> Self {
        Self { path, kind }
    }

    pub fn text_content(&self) -> Result<String, io::Error> {
        if self.kind == SourceKind::Text {
            fs::read_to_string(Path::new(&self.path))
        } else {
            panic!("change this")
        }
    }
}

/// A map between source indices and the actual source material. It is intended that the program will have one SourceMap. When a source is needed to be resolved, the "global" source map can be passed.
/// 
/// Main source is always given a source id of 0.
#[derive(Debug, Clone, PartialEq)]
pub struct SourceMap {
    map: Vec<Source>
}

impl SourceMap {
    pub fn new() -> Self {
        Self {
            map: vec![]
        }
    }

    pub fn add_source(&mut self, source: Source) -> SourceId {
        self.map.push(source);
        self.map.len() - 1
    }

    pub fn get_source(&self, id: SourceId) -> &Source {
        &self.map[id]
    }
}

pub type SourceId = usize;
