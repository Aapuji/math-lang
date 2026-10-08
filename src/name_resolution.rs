// use crate::{ast::Type, source::Span, token::LexemeId};

// #[derive(Debug)]
// pub struct SymbolTable {
//     entries: Vec<SymbolEntry>
// }

// #[derive(Debug, Clone)]
// pub struct SymbolEntry {
//     lexeme_id: LexemeId,
//     kind: SymbolKind,
//     def_span: Span
//     // scope level?
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub enum SymbolKind {
//     Variable {
//         mutability: bool,
//         ty: Type
//     },
//     Function {
//         arity: u32, // less than MAX_ARGS
//         ty: Type
//     }
// }

// #[derive(Debug)]
// pub struct NameResolver {
//     envs: Vec<Environment>
// }

// #[derive(Debug, Clone, Copy)]
// pub struct Environment {
    
// }
