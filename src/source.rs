use std::ops::Range;
use std::path::PathBuf;

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

    pub fn set_start(&mut self, start: usize) {
        self.start = start;
    }

    pub fn set_end(&mut self, end: usize) {
        self.end = end;
    }

    pub fn set_source_id(&mut self, source_id: SourceId) {
        self.source_id = source_id;
    }

    pub fn get_lexeme<'s>(&self, source_map: &'s SourceMap) -> &'s str{ 
        &source_map.get_source(self.source_id()).data()[self.range()]
    }

}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SourceKind {
    Text
}

/// Represents a code source.
#[derive(Debug, Clone, PartialEq)]
pub struct Source {
    path: PathBuf,
    kind: SourceKind,
    data: String
    // line_offsets: Vec<usize>
}

impl Source {
    pub fn new(path: PathBuf, kind: SourceKind, data: String) -> Self {
        Self { path, kind, data }
    }

    pub fn kind(&self) -> SourceKind {
        self.kind
    }

    pub fn data(&self) -> &str {
        &self.data
    }
}

/// A map between source indices and the actual source material. It is intended that the program will have one SourceMap. When a source is needed to be resolved, the "global" source map can be passed.
/// 
/// A source id of 0 refers to a synthesized source, meaning that it doesn't come from any actual source file.
#[derive(Debug, Clone, PartialEq)]
pub struct SourceMap {
    sources: Vec<Source>
}

impl SourceMap {
    pub const SYNTHETIC_SOURCE_ID: SourceId = 0;

    pub const fn new() -> Self {
        Self {
            sources: vec![]
        }
    }

    pub fn add_source(&mut self, source: Source) -> SourceId {
        self.sources.push(source);
        self.sources.len()
    }

    pub fn get_source(&self, id: SourceId) -> &Source {
        &self.sources[id - 1]
    }

    pub fn get_source_mut(&mut self, id: SourceId) -> &mut Source {
        &mut self.sources[id - 1]
    }

    pub fn synthetic_span() -> Span {
        Span::new(0, 0, Self::SYNTHETIC_SOURCE_ID)
    }
}

pub type SourceId = usize;
