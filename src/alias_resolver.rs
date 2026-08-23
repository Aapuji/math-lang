use std::collections::HashMap;
use std::hash::Hash;
use std::mem::discriminant;

use crate::ast::{AliasLeft, AliasRight, Expr, OpLit, Oper, Stmt, Var};

/// A struct used for registering and resolving aliases.
/// 
/// Uses a form of a version-chain structure for registering aliases, because it allows for 
/// shadowing aliases in older scopes but not in the current scope.
/// 
/// Also uses a vector to record all alias resolutions for the LSP/editor to use. It is naturally ordered 
/// because it is assumed that all insertions are handled sequentially as they appear in the source file.
/// If this assumtion breaks, then this part will need to be reimplemented. 
#[derive(Debug, Clone)]
pub struct AliasResolver<'ast> {
    current_defs: HashMap<AliasItem<'ast>, usize>,
    alias_timeline: Vec<TimelineEntry<'ast>>,
    scope_starts: Vec<usize>
}

impl<'ast> AliasResolver<'ast> {
    pub fn new() -> Self {
        Self {
            current_defs: HashMap::new(),
            alias_timeline: Vec::new(),
            scope_starts: Vec::new(),   // if empty, it is assumed that the current scope is global scope (ie. index 0)
        }
    }

    /// Register and resolve all aliases in given statements.
    pub fn resolve_aliases(&mut self, stmts: &'ast mut Vec<Stmt>) {
        for stmt in stmts {
            match stmt {
                Stmt::Alias { new, old, .. } => {
                    let new_item = AliasItem::from(new);
                    let old_item = AliasItem::from(old);

                    self.register_alias(new_item, old_item);
                }

                Stmt::Expr { expr, .. } => {
                    match expr {
                        Expr::Ident(name) => {
                            // let item = AliasItem::from(*name);

                            // if Some(resolved) = self.get_alias(&item) {
                            //     *name = 
                            // }
                        }

                        _ => todo!()
                    }
                }

                _ => todo!()
            }
        }
    }

    fn register_alias(&mut self, mut new_item: AliasItem<'ast>, old_item: AliasItem<'ast>) {
        let scope_start_idx = self.scope_starts.last().unwrap_or(&0);
        let prev_idx = self.current_defs.get(&new_item);

        if let Some(idx) = prev_idx {
            if idx >= scope_start_idx {
                todo!("error: cannot redefine an alias in the same scope")
            }
        }

        let old_item = self.get_alias(&old_item).unwrap_or(old_item);
        if old_item == new_item {
            todo!("error: circular alias detected")
        }

        println!("OLD ITEM: {old_item:?}\nNEW ITEM: {new_item:?}");

        match (new_item.kind, old_item.kind) {
            (AliasKind::Ident, kind) => new_item.kind = kind,
            (AliasKind::Oper, AliasKind::Oper) => (),
            (AliasKind::Oper, _) => todo!("error: invalid alias kinds. new is oper, old is not."),
            (AliasKind::Expr, AliasKind::Expr) => (),
            (AliasKind::Expr, _) => todo!("error: invalid alias kinds. new is expr, old is not.")
        }

        let i = self.alias_timeline.len();
        self.alias_timeline.push(TimelineEntry {
            old_item,
            prev_def: prev_idx.copied()
        });

        self.current_defs.insert(new_item, i);
    }

    fn get_alias(&self, item: &AliasItem<'ast>) -> Option<AliasItem<'ast>> {
        self.current_defs
            .get(&item)
            .map(|i| self.alias_timeline[*i].old_item)
    }
}

#[derive(Debug, Clone)]
pub struct TimelineEntry<'ast> {
    old_item: AliasItem<'ast>,
    prev_def: Option<usize>     // index of previous alias of this symbol (ie. shadowed alias)
}

#[derive(Debug, Clone, Copy, Eq)]
pub struct AliasItem<'ast> {
    frag: AliasFragment<'ast>,
    kind: AliasKind
}

impl<'ast> PartialEq for AliasItem<'ast> {
    fn eq(&self, other: &Self) -> bool {
        self.frag == other.frag
    }
}

impl<'ast> Hash for AliasItem<'ast> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.frag.hash(state)
    }
}

impl<'ast> From<&'ast mut AliasLeft> for AliasItem<'ast> {
    fn from(value: &'ast mut AliasLeft) -> Self {
        match value {
            AliasLeft::Ident(ident) => AliasItem {
                frag: AliasFragment::Ident(*ident),
                kind: AliasKind::Ident
            },

            AliasLeft::Oper(op) => AliasItem {
                frag: AliasFragment::Oper(*op),
                kind: AliasKind::Oper
            }
        }
    }
}

impl<'ast> From<&'ast mut AliasRight> for AliasItem<'ast> {
    fn from(value: &'ast mut AliasRight) -> Self {
        match value {
            AliasRight::Ident(ident) => AliasItem {
                frag: AliasFragment::Ident(*ident),
                kind: AliasKind::Ident
            },

            AliasRight::Oper(op) => AliasItem {
                frag: AliasFragment::Oper(*op),
                kind: AliasKind::Oper
            },

            AliasRight::OpLit(oplit) => AliasItem {
                frag: AliasFragment::OpLit(*oplit),
                kind: AliasKind::Oper
            },

            AliasRight::Expr(expr) => AliasItem {
                frag: AliasFragment::Expr(&*expr),
                kind: AliasKind::Expr
            }
        }
    }
}

impl<'ast> From<Var> for AliasItem<'ast> {
    fn from(value: Var) -> Self {
        AliasItem {
            frag: AliasFragment::Ident(value),
            kind: AliasKind::Ident
        }
    }
}

impl<'ast> From<Oper> for AliasItem<'ast> {
    fn from(value: Oper) -> Self {
        AliasItem {
            frag: AliasFragment::Oper(value),
            kind: AliasKind::Oper
        }
    }
}

#[derive(Debug, Clone, Copy, Eq)]
pub enum AliasFragment<'ast> {
    Ident(Var),
    Oper(Oper),
    OpLit(OpLit),
    Expr(&'ast Expr)
}

impl<'ast> Hash for AliasFragment<'ast> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        discriminant(self).hash(state);

        match self {
            AliasFragment::Ident(ident) => ident.id().hash(state),
            AliasFragment::Oper(op) => op.id().hash(state),
            AliasFragment::OpLit(oplit) => oplit.name().id().hash(state),
            AliasFragment::Expr(expr) => expr.hash(state),
        }
    }
}

impl<'ast> PartialEq for AliasFragment<'ast> {
    fn eq(&self, other: &Self) -> bool {
        use AliasFragment::*;

        match (self, other) {
            (Ident(id1), Ident(id2)) => id1.id() == id2.id(),
            (Oper(op1), Oper(op2)) => op1.id() == op2.id(),
            (OpLit(oplit1), OpLit(oplit2)) => oplit1.name().id() == oplit2.name().id(),
            (Expr(x1), Expr(x2)) => x1 == x2,
            _ => false
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AliasKind {
    Ident,
    Oper,
    Expr
}
