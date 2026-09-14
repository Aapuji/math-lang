use std::collections::HashMap;
use std::hash::Hash;
use std::mem::discriminant;

// TODO: rewrite without references
// TODO: Have a phase (or maybe in this phase when resolving a prefix or infix custom op) to reassociate resolved expressions

use crate::{ast::{AliasLeft, AliasRight, Binding, Endpoint, Expr, FnHeader, Generic, Let, OpLit, Oper, Operation, RangeStep, Shape, ShapeSpec, Stmt, StringPart, Type, Var, Variant}, source::Span};

/// A struct used for registering and resolving aliases.
/// 
/// Uses a form of a version-chain structure for registering aliases, because it allows for 
/// shadowing aliases in older scopes but not in the current scope.
/// 
/// Also uses a vector to record all alias resolutions for the LSP/editor to use. It is naturally ordered 
/// because it is assumed that all insertions are handled sequentially as they appear in the source file.
/// If this assumtion breaks, then this part will need to be reimplemented. 
#[derive(Debug, Clone)]
pub struct AliasResolver {
    current_defs: HashMap<AliasItem, usize>,
    alias_timeline: Vec<TimelineEntry>,
    scope_starts: Vec<usize>,
    record_table: Vec<AliasRecord> 
}

impl AliasResolver {
    pub fn new() -> Self {
        Self {
            current_defs: HashMap::new(),
            alias_timeline: Vec::new(),
            scope_starts: Vec::new(),   // if empty, it is assumed that the current scope is global scope (ie. index 0)
            record_table: Vec::new()
        }
    }

    /// Register and resolve all aliases in given statements.
    pub fn resolve_aliases(&mut self, stmts: &mut Vec<Stmt>) {
        for stmt in stmts {
            self.resolve_stmt(stmt);
        }
    }

    fn resolve_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Alias { new, old, .. } => {
                let new_item = AliasItem::from(&*new);
                let old_item = match old {
                    AliasRight::Expr(expr) => {
                        self.resolve_expr(expr);
                        AliasItem::from(&AliasRight::Expr(expr.clone()))
                    }

                    _ => self.get_alias_from_right_and_record(&*old)
                };

                self.register_alias(new_item, old_item);
            }

            Stmt::Expr { expr, .. } => self.resolve_expr(expr),

            Stmt::Var   { name, ty, value, .. } |
            Stmt::Const { name, ty, value, .. } => {
                self.resolve_var_to_var(name);
                
                if let Some(ty) = ty {
                    self.resolve_type(ty);
                }

                if let Some(expr) = value {
                    self.resolve_expr(expr);
                }
            }

            Stmt::Let { def, .. } => {
                for binding in def.bindings_mut() {
                    self.resolve_binding(binding);
                }

                if let Some(value) = def.value_mut() {
                    self.resolve_expr(value);
                }
            }

            Stmt::Fn { header, value, .. } => {
                self.resolve_header(header);
                self.resolve_expr(value);
            }

            Stmt::Sym { name, args, ty, .. } => {
                self.resolve_var_to_var(name);
                for (arg, ty) in args {
                    self.resolve_var_to_var(arg);
                    
                    if let Some(ty) = ty {
                        self.resolve_type(ty);
                    }
                }

                if let Some(ty) = ty {
                    self.resolve_type(ty);
                }
            }

            Stmt::Type { name, ty_args, def, .. } => {
                self.resolve_var_to_var(name);
                self.resolve_generics(ty_args);
                self.resolve_type(def);
            }

            Stmt::Enum { name, ty_args, variants, .. } => {
                self.resolve_var_to_var(name);
                self.resolve_generics(ty_args);
                
                for variant in variants {
                    match variant {
                        Variant::Const(name) => self.resolve_var_to_var(name),
                        Variant::Tuple(types) => for ty in types {
                            self.resolve_type(ty);
                        }
                        Variant::Record(fields) => for (field, ty) in fields {
                            self.resolve_var_to_var(field);
                            self.resolve_type(ty);
                        }
                    }
                }
            }

            Stmt::Struct { name, ty_args, fields, .. } => {
                self.resolve_var_to_var(name);
                self.resolve_generics(ty_args);
                for (field, ty) in fields {
                    self.resolve_var_to_var(field);
                    self.resolve_type(ty);
                }
            }
        }
    }

    fn resolve_binding(&mut self, binding: &mut Binding) {
        match binding {
            Binding::Name(name, ty) => {
                self.resolve_var_to_var(name);
                if let Some(ty) = ty {
                    self.resolve_type(ty);
                }
            }

            Binding::Fn(header) |
            Binding::Call(header) => self.resolve_header(header)
        }
    }

    fn resolve_header(&mut self, header: &mut FnHeader) {
        self.resolve_var_to_var(header.name_mut());
        self.resolve_generics(header.ty_args_mut());
        
        for (name, ty) in header.args_mut() {
            self.resolve_var_to_var(name);
            if let Some(ty) = ty {
                self.resolve_type(ty);
            }
        }

        for (name, ty) in header.kwargs_mut() {
            self.resolve_var_to_var(name);
            if let Some(ty) = ty {
                self.resolve_type(ty);
            }
        }

        if let Some(ty) = header.ty_mut() {
            self.resolve_type(ty);
        }
    }
    
    fn resolve_type(&mut self, ty: &mut Type) {
        match ty {
            Type::Unit { .. } => (),
            Type::Named(name) => self.resolve_var_to_var(name),
            Type::Array { shape, ty, .. } => {
                match shape {
                    Shape::Empty | Shape::Dynamic => (),
                    Shape::Specified(specs) => for spec in specs {
                        match spec {
                            ShapeSpec::Known(expr) => self.resolve_expr(expr),
                            ShapeSpec::Unknown => ()
                        }
                    }
                }

                self.resolve_type(ty);
            }

            Type::Tuple { types, .. } => {
                for ty in types {
                    self.resolve_type(ty);
                }
            }

            Type::Exponent { ty, exp, .. } => {
                self.resolve_type(ty);
                self.resolve_expr(exp);
            }
        }
    }

    fn resolve_generics(&mut self, generics: &mut Vec<Generic>) {
        for Generic { name } in generics {
            self.resolve_var_to_var(name);
        }
    }

    /// Gets corresponding `AliasItem` from the given `AliasRight` and records it in the record table if found or otherwise defaults to inputted `AliasRight` converted to an `AliasItem`.
    fn get_alias_from_right_and_record(&mut self, right: &AliasRight) -> AliasItem {
        let item = AliasItem::from(right);
        self.get_alias(&item)
            .inspect(|it| {
                self.record_table.push(AliasRecord {
                    usage_span: item.frag.span(),
                    def_span: it.frag.span()
                });
            })
            .unwrap_or(AliasItem::from(right))
    }

    fn resolve_var_to_var(&mut self, name: &mut Var) {
        if let Some(resolved) = self.get_alias(&AliasItem::from(*name)) {
            if resolved.kind == AliasKind::Ident {
                let new_name = match resolved.frag {
                    AliasFragment::Ident(id) => {
                        Var::new(id.id(), name.span())
                    },
                    _ => unreachable!()
                };

                self.record_table.push(AliasRecord {
                    usage_span: name.span(),
                    def_span: new_name.span()
                });
                *name = new_name;
            } else {
                todo!("expected identifier, found {:?}", resolved.kind);
            }
        }
    }

    fn resolve_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Block { stmts, tail, .. } => {
                self.enter_scope();
                
                self.resolve_aliases(stmts);
                if let Some(expr) = tail {
                    self.resolve_expr(expr);
                }

                self.exit_scope();
            }
            
            Expr::Ident(name) => if let Some(resolved) = self.get_alias(&AliasItem::from(*name)) {
                let (new_expr, def_span) = match resolved.frag {
                    AliasFragment::Ident(resolved_name) => {
                        (Expr::Ident(Var::new(resolved_name.id(), name.span())), resolved_name.span())
                    }

                    AliasFragment::Expr(resolved_expr) => {
                        let mut expr = resolved_expr.clone();
                        let span = expr.span();
                        *expr.span_mut() = name.span();

                        (expr, span)
                    }
                    _ => todo!("expected expression found {:?}", resolved.kind)
                };

                self.record_table.push(AliasRecord {
                    usage_span: name.span(),
                    def_span
                });
                *expr = new_expr;
            }

            Expr::String { parts, .. } => {
                for part in parts {
                    match part {
                        StringPart::Expr(expr) => self.resolve_expr(expr),
                        StringPart::Text(_) => ()
                    }
                }
            }

            Expr::Latex(..) => todo!(),

            Expr::Not { expr, .. } |
            Expr::UnaryPlus { expr, .. } |
            Expr::Neg { expr, .. } |
            Expr::Spread { expr, .. } => self.resolve_expr(expr),

            // builtin binary operations
            Expr::Or { lhs, rhs, ..} |
            Expr::Xor { lhs, rhs, ..} |
            Expr::And { lhs, rhs, ..} |
            Expr::Eq { lhs, rhs, ..} |
            Expr::NotEq { lhs, rhs, ..} |
            Expr::Less { lhs, rhs, ..} |
            Expr::Greater { lhs, rhs, ..} |
            Expr::LessEq { lhs, rhs, ..} |
            Expr::GreaterEq { lhs, rhs, ..} |
            Expr::In { lhs, rhs, ..} |
            Expr::Plus { lhs, rhs, ..} |
            Expr::Minus { lhs, rhs, ..} |
            Expr::PlusMinus { lhs, rhs, ..} |
            Expr::MinusPlus { lhs, rhs, ..} |
            Expr::Times { lhs, rhs, ..} |
            Expr::Divide { lhs, rhs, ..} |
            Expr::IntDivide { lhs, rhs, ..} |
            Expr::Mod { lhs, rhs, ..} |
            Expr::ModClass { lhs, rhs, ..} |
            Expr::Exp { lhs, rhs, ..} => {
                self.resolve_expr(lhs);
                self.resolve_expr(rhs);
            }

            Expr::Array { rows, .. } => {
                for row in rows {
                    for expr in row {
                        self.resolve_expr(expr);
                    }
                }
            }

            Expr::Range { lhs, rhs, step, .. } => {
                match lhs {
                    Endpoint::Inclusive(expr) |
                    Endpoint::Exclusive(expr) => self.resolve_expr(expr),
                    Endpoint::Unspecified => ()
                }

                match rhs {
                    Endpoint::Inclusive(expr) |
                    Endpoint::Exclusive(expr) => self.resolve_expr(expr),
                    Endpoint::Unspecified => ()
                }

                match step {
                    RangeStep::Discrete(expr) => self.resolve_expr(expr),
                    RangeStep::Continuous => ()
                }
            }

            Expr::Prefix { operator, operand, .. } => {
                match operator {
                    Operation::Ident(name) => if let Some(resolved) = self.get_alias(&AliasItem::from(*name)) {
                        let operation = match resolved.frag {
                            AliasFragment::Ident(name) => Operation::Ident(name),
                            AliasFragment::Oper(oper) => Operation::Oper(oper),
                            AliasFragment::OpLit(oplit) => Operation::OpLit(oplit),
                            _ => todo!("expected variable, operator, or operator literal but found expression")
                        };

                        self.record_table.push(AliasRecord {
                            usage_span: name.span(),
                            def_span: operation.span()
                        });
                        *operator = operation;
                    }

                    Operation::Oper(oper) => if let Some(resolved) = self.get_alias(&AliasItem::from(*oper)) {
                        let operation = match resolved.frag {
                            AliasFragment::Ident(name) => Operation::Ident(name),
                            AliasFragment::Oper(oper) => Operation::Oper(oper),
                            AliasFragment::OpLit(oplit) => Operation::OpLit(oplit),
                            _ => todo!("expected variable, operator, or operator literal but found expression")
                        };

                        self.record_table.push(AliasRecord {
                            usage_span: oper.span(),
                            def_span: operation.span()
                        });
                        *operator = operation;
                    }

                    Operation::OpLit(oplit) => {
                        self.resolve_var_to_var(&mut oplit.name());
                    }
                };

                self.resolve_expr(operand);

                // TODO: reduce custom prefix and infix AST nodes into  
                // Thoughts: perhaps allow for associativity and precedence in operator literals
                /* Like maybe
                    `f`
                    `f:4`
                    `@lassoc f`
                    `@lassoc f:4`
                    `@rassoc f`
                    `@rassoc f:4`
                */
                
            }

            Expr::Infix { lhs, operator, rhs, .. } => {
                self.resolve_expr(lhs);
                
                match operator {
                    Operation::Ident(name) => if let Some(resolved) = self.get_alias(&AliasItem::from(*name)) {
                        let operation = match resolved.frag {
                            AliasFragment::Ident(name) => Operation::Ident(name),
                            AliasFragment::Oper(oper) => Operation::Oper(oper),
                            AliasFragment::OpLit(oplit) => Operation::OpLit(oplit),
                            _ => todo!("expected variable, operator, or operator literal but found expression")
                        };

                        self.record_table.push(AliasRecord {
                            usage_span: name.span(),
                            def_span: operation.span()
                        });
                        *operator = operation;
                    }

                    Operation::Oper(oper) => if let Some(resolved) = self.get_alias(&AliasItem::from(*oper)) {
                        let operation = match resolved.frag {
                            AliasFragment::Ident(name) => Operation::Ident(name),
                            AliasFragment::Oper(oper) => Operation::Oper(oper),
                            AliasFragment::OpLit(oplit) => Operation::OpLit(oplit),
                            _ => todo!("expected variable, operator, or operator literal but found expression")
                        };

                        self.record_table.push(AliasRecord {
                            usage_span: oper.span(),
                            def_span: operation.span()
                        });
                        *operator = operation;
                    }

                    Operation::OpLit(oplit) => {
                        self.resolve_var_to_var(&mut oplit.name());
                    }
                };

                self.resolve_expr(rhs);
            }
            
            Expr::Int { .. } |
            Expr::Real { .. } |
            Expr::Imag { .. } |
            Expr::Unit { .. } => (),

            _ => todo!()
        }
    }

    fn register_alias(&mut self, mut new_item: AliasItem, mut old_item: AliasItem) {
        let scope_start_idx = &self.current_scope_start();
        let prev_idx = self.current_defs.get(&new_item);

        if let Some(idx) = prev_idx {
            if idx >= scope_start_idx {
                todo!("error: cannot redefine an alias in the same scope")
            }
        }

        // let old_item = self.get_alias(&old_item).unwrap_or(old_item);
        if old_item == new_item {
            todo!("error: alias cycle detected")
        }

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
            prev_def: prev_idx.copied(),
            new_item: new_item.clone()
        });

        // self.insert_or_update_map(new_item, i);
        self.current_defs.insert(new_item, i);
    }

    /// Gets the corresponding `AliasItem` registered for the given item, or outputs `None` if the item does not exist.
    fn get_alias(&self, item: &AliasItem) -> Option<AliasItem> {
        self.current_defs
            .get(&item)
            .map(|i| self.alias_timeline[*i].old_item.clone())
    }

    fn enter_scope(&mut self) {
        self.scope_starts.push(self.alias_timeline.len());
    }

    fn exit_scope(&mut self) {
        self.alias_timeline.drain(self.current_scope_start()..)
            .rev()
            .for_each(|entry| {
                self.current_defs.remove(&entry.new_item);

                // needs to be removed prior to update the kind of the key item
                if let Some(i) = entry.prev_def {
                    self.current_defs.insert(entry.new_item, i);
                }
            });
        self.scope_starts.pop();
    }

    fn current_scope_start(&self) -> usize {
        *self.scope_starts.last().unwrap_or(&0)
    }

    // Removes and then inserts key back to map so that any fields not part of Eq or Hash can be updated.
    // fn insert_or_update_map(&mut self, new_item: AliasItem<'ast>, i: usize) {
    //     self.current_defs.remove(&new_item); // has to be removed so that the key is updated
    //     self.current_defs.insert(new_item, i);
    // }
}

#[derive(Debug, Clone)]
pub struct TimelineEntry {
    old_item: AliasItem,
    prev_def: Option<usize>,
    new_item: AliasItem
}

#[derive(Debug, Clone, Eq)]
pub struct AliasItem {
    frag: AliasFragment,
    kind: AliasKind
}

impl PartialEq for AliasItem {
    fn eq(&self, other: &Self) -> bool {
        self.frag == other.frag
    }
}

impl Hash for AliasItem {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.frag.hash(state)
    }
}

impl From<&AliasLeft> for AliasItem {
    fn from(value: &AliasLeft) -> Self {
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

impl From<&AliasRight> for AliasItem {
    fn from(value: &AliasRight) -> Self {
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
                frag: AliasFragment::Expr(expr.clone()),
                kind: AliasKind::Expr
            }
        }
    }
}

impl From<Var> for AliasItem {
    fn from(value: Var) -> Self {
        AliasItem {
            frag: AliasFragment::Ident(value),
            kind: AliasKind::Ident
        }
    }
}

impl From<Oper> for AliasItem {
    fn from(value: Oper) -> Self {
        AliasItem {
            frag: AliasFragment::Oper(value),
            kind: AliasKind::Oper
        }
    }
}

#[derive(Debug, Clone, Eq)]
pub enum AliasFragment {
    Ident(Var),
    Oper(Oper),
    OpLit(OpLit),
    Expr(Expr)
}

impl Hash for AliasFragment {
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

impl PartialEq for AliasFragment {
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

impl AliasFragment {
    fn get_var(&self) -> Option<Var> {
        match self {
            Self::Ident(name) => Some(*name),
            _ => None
        }
    }

    fn get_op(&self) -> Option<Oper> {
        match self {
            Self::Oper(op) => Some(*op),
            _ => None
        }
    }

    fn get_oplit(&self) -> Option<OpLit> {
        match self {
            Self::OpLit(oplit) => Some(*oplit),
            _ => None
        }
    }

    fn get_expr(&self) -> Option<&Expr> {
        match self {
            Self::Expr(expr) => Some(expr),
            _ => None
        }
    }

    fn span(&self) -> Span {
        match self {
            Self::Ident(name) => name.span(),
            Self::Oper(op) => op.span(),
            Self::OpLit(oplit) => oplit.span(),
            Self::Expr(expr) => expr.span()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AliasKind {
    Ident,
    Oper,
    Expr
}

#[derive(Debug, Clone, Copy)]
pub struct AliasRecord {
    usage_span: Span,
    def_span: Span
}
