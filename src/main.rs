use std::env;
use std::fs::read_to_string;
use std::path::PathBuf;
use std::error::Error;

mod ast;
mod alias_resolver;
mod config;
mod lexer;
mod parser;
mod source;
mod token;

use crate::alias_resolver::AliasResolver;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::source::{Source, SourceId, SourceKind, SourceMap};
use crate::token::ActiveInterner;

fn main() -> Result<(), Box<dyn Error>> {
    let main_source = match env::args().skip(1).next() {
        None => panic!("incorrect usage; usage: math-lang <FILE-NAME>"),
        Some(path) => Source::new(PathBuf::from(path.clone()), SourceKind::Text, read_to_string(path)?)
    };

    let mut source_map = SourceMap::new();
    let main_source: SourceId = source_map.add_source(main_source);
    let content = source_map
        .get_source(main_source)
        .data()
        .to_owned();

    println!("{}\n== TOKENS ==", content);

    let mut interner = ActiveInterner::new();
    let lexer = Lexer::new(&content, main_source);
    let tokens = lexer.lex(&mut source_map, &mut interner);
    let interner = interner.into_resolver();

    println!("{:#?}", tokens);
    println!("\n== AST ==");

    let parser = Parser::new(tokens);
    let mut stmts = parser.parse(&mut source_map, &interner);

    // println!("{:#?}\n", stmts);

    let mut alias_resolver = AliasResolver::new();
    alias_resolver.resolve_aliases(&mut stmts);
    
    println!("== ALIAS RESOLVER ==\n{:#?}\n", alias_resolver);
    // println!("== ALIASED AST ==\n{:#?}", stmts);
    
    Ok(())
}
