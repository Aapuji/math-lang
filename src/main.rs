use std::env;
use std::error::Error;
use std::fs;
use std::path::Path;

mod config;
mod lexer;
mod source;
mod token;


use crate::{lexer::Lexer, source::{Source, SourceId, SourceKind, SourceMap}};

fn main() -> Result<(), Box<dyn Error>> {
    let main_source = match env::args().skip(1).next() {
        None => panic!("incorrect usage; usage: math-lang <FILE-NAME>"),
        Some(path) => Source::new(path, SourceKind::Text)
    };

    let mut source_map = SourceMap::new();
    let main_source: SourceId = source_map.add_source(main_source);

    let content = source_map.get_source(main_source).text_content().unwrap();

    println!("{}\n=====\n", content.clone());

    let lexer = Lexer::new(&content, main_source);
    let tokens = lexer.lex();

    println!("{:#?}\n", tokens);

    Ok(())
}
