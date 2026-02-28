use std::env;
use std::fs::read_to_string;
use std::path::PathBuf;
use std::error::Error;

mod ast;
mod config;
mod lexer;
mod parser;
mod source;
mod token;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::source::{Source, SourceId, SourceKind, SourceMap};

fn main() -> Result<(), Box<dyn Error>> {
    let main_source = match env::args().skip(1).next() {
        None => panic!("incorrect usage; usage: math-lang <FILE-NAME>"),
        Some(path) => Source::new(PathBuf::from(path.clone()), SourceKind::Text, read_to_string(path)?)
    };

    let mut source_map = SourceMap::new();
    let main_source: SourceId = source_map.add_source(main_source);
    let content = source_map
        .get_source_mut(main_source)
        .data()
        .to_owned();

    println!("{}\n== TOKENS ==", content);

    let lexer = Lexer::new(&content, main_source);
    let tokens = lexer.lex(&mut source_map);

    println!("{:#?}\n\n== AST ==", tokens);

    let parser = Parser::new(tokens);
    let stmts = parser.parse(&mut source_map);

    println!("{:#?}", stmts);

    Ok(())
}
