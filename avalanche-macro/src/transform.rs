use proc_macro2::{Span, TokenStream};
use std::{collections::HashSet, hash::Hash, ops::Deref, ops::DerefMut};
use syn::{
    parse2, parse_quote, spanned::Spanned, token::Semi, Block, Expr, ExprPath, Ident, Lit, Pat,
    PatType, Path, Stmt,
};

// Span line and column information with proc macros is not available on stable
// To emulate unique identities for given component instantiations,
// we currently instead generate random line and column numbers
use proc_macro_error::abort;
use quote::{quote_spanned, ToTokens};
use rand::random;

use crate::{
    avalanche_path::get_avalanche_path,
    macro_expr::{EncloseBody, ExprList, MatchesBody, Tracked, Try, VecBody},
};

const EXPR_CONVERSION_ERROR: &str = "internal error: unable to process Expr within tracked";

#[derive(Clone, Default, Debug)]
pub(crate) struct Dependencies(HashSet<Ident>);

impl Dependencies {
    pub(crate) fn new() -> Self {
        Default::default()
    }

    pub(crate) fn extend(&mut self, other: Dependencies) {
        self.0.extend(other.0);
    }
}

impl Deref for Dependencies {
    type Target = HashSet<Ident>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Dependencies {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl PartialEq for Dependencies {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for Dependencies {}

impl Hash for Dependencies {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for ident in self.0.iter() {
            (*ident).hash(state);
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Var {
    pub(crate) name: String,
    pub(crate) dependencies: UnitDeps,
    pub(crate) span: Span,
}

impl PartialEq for Var {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.dependencies == other.dependencies
    }
}

impl Eq for Var {}

impl Hash for Var {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.dependencies.hash(state);
    }
}

#[derive(Default, Debug)]
pub(crate) struct Scope {
    pub(crate) vars: Vec<Var>,
    /// Whether the scope is that of a function or closure
    pub(crate) function: bool,
}

impl Scope {
    pub(crate) fn new() -> Self {
        Default::default()
    }

    pub(crate) fn function() -> Self {
        Self {
            vars: Vec::new(),
            function: true,
        }
    }
}

#[derive(Clone, Debug, Default, Hash, Eq, PartialEq)]
pub(crate) struct UnitDeps {
    /// The named tracked dependencies of a unit's value
    tracked_deps: Dependencies,
    /// whether the unit has `tracked!` components
    has_tracked: bool,
}

impl UnitDeps {
    fn new() -> Self {
        Self {
            tracked_deps: Dependencies::new(),
            has_tracked: false,
        }
    }

    fn extend(&mut self, other: UnitDeps) {
        self.tracked_deps.extend(other.tracked_deps);
        self.has_tracked = self.has_tracked || other.has_tracked;
    }
}

impl From<Dependencies> for UnitDeps {
    fn from(tracked_deps: Dependencies) -> Self {
        Self {
            tracked_deps,
            has_tracked: false,
        }
    }
}

fn parse_expr(stream: TokenStream) -> Expr {
    parse2(stream).unwrap_or_else(|_| parse_quote! {::std::compile_error!(#EXPR_CONVERSION_ERROR)})
}

fn enable_expr_tracking(expr: &mut Expr, deps: &UnitDeps) {
    if deps.has_tracked {
        let expr_span = expr.span();
        let avalanche_path = get_avalanche_path();
        let transformed = quote_spanned! { expr_span=>
            {
                let mut __avalanche_internal_gen = #avalanche_path::tracked::Gen::escape_hatch_new(false);
                #avalanche_path::Tracked::new(#expr, __avalanche_internal_gen)
            }
        };
        *expr = parse_expr(transformed);
    }
}

#[derive(Default, Debug)]
pub(crate) struct Function {
    pub(crate) scopes: Vec<Scope>,
}

impl Function {
    pub(crate) fn new() -> Self {
        Default::default()
    }

    pub(crate) fn get_var_mut(&mut self, name: &str) -> Option<&mut Var> {
        for scope in self.scopes.iter_mut().rev() {
            match scope.vars.iter_mut().rev().find(|item| item.name == name) {
                Some(var) => return Some(var),
                None => continue,
            }
        }

        None
    }

    pub(crate) fn get_var_function_scope(&self, name: &str) -> Option<&Var> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.vars.iter().rev().find(|item| item.name == name) {
                return Some(var);
            }
            // if we have reached a function scope and found nothing, abort search
            else if scope.function {
                break;
            }
        }
        None
    }

    pub(crate) fn block(&mut self, block: &mut Block) -> UnitDeps {
        let mut deps = UnitDeps::new();

        self.scopes.push(Scope::new());
        block.stmts = block
            .stmts
            .drain(..)
            .flat_map(|stmt| {
                // don't include non-value deps, as only expr deps contribute to the dependencies of the block
                let (stmts, mut stmt_deps) = self.stmt(stmt, false);
                // remove variables internal to the block from the dependencies list, as they are not external dependencies
                stmt_deps
                    .tracked_deps
                    .retain(|dep| self.get_var_function_scope(&dep.to_string()).is_none());
                deps.extend(stmt_deps);
                stmts
            })
            .collect();

        self.scopes.pop();
        deps
    }

    pub(crate) fn closure_block(&mut self, block: &mut Block) -> UnitDeps {
        let mut deps = UnitDeps::new();

        self.scopes.push(Scope::new());
        block.stmts = block
            .stmts
            .drain(..)
            .flat_map(|stmt| {
                // include non value deps, as they are relevant to the value of the closure
                let (stmts, mut stmt_deps) = self.stmt(stmt, true);
                // remove variables internal to the block from the dependencies list, as they are not external dependencies
                // TODO: fix let a = tracked!(a); currently this would not report a as a dependency
                stmt_deps
                    .tracked_deps
                    .retain(|dep| self.get_var_function_scope(&dep.to_string()).is_none());
                deps.extend(stmt_deps);
                stmts
            })
            .collect();

        self.scopes.pop();
        deps.has_tracked = !deps.tracked_deps.is_empty();
        deps
    }

    /// Statements with a ; have value (), which has no dependencies, but
    /// if `include_non_value_deps` is true `tracked!` dependencies will be included.
    /// This is useful for closures. Returns a vec of statements that the given statement
    /// has been converted into, along with the statement's dependencies.
    fn stmt(&mut self, mut stmt: Stmt, include_non_value_deps: bool) -> (Vec<Stmt>, UnitDeps) {
        match &mut stmt {
            Stmt::Local(local) => {
                let (stmts, mut init_dependencies, vars) = match &mut local.init {
                    Some((_, expr)) => {
                        let deps = self.expr(expr, false);
                        let vars = vars_from_pat(&local.pat, deps.clone());
                        match &mut local.pat {
                            // for a simple let name = expr; statement, create simpler codegen
                            Pat::Ident(_) => {
                                enable_expr_tracking(expr, &deps);
                                (vec![stmt], deps, vars)
                            }
                            // the case of let x: Tracked<u8> = .. should be treated the same as let x = ..
                            Pat::Type(PatType { pat, .. }) if matches!(**pat, Pat::Ident(_)) => {
                                enable_expr_tracking(expr, &deps);
                                (vec![stmt], deps, vars)
                            }
                            pat => {
                                if deps.has_tracked {
                                    // remove the type annotation given by the user, as it will have tracked types
                                    // in incorrect positions due to inner codegen details.
                                    if let Pat::Type(PatType { pat: inner_pat, .. }) = pat {
                                        *pat = *inner_pat.clone();
                                    }
                                    let avalanche_path = get_avalanche_path();
                                    let init_gen: Stmt = parse_quote! {
                                        let mut __avalanche_internal_gen = #avalanche_path::tracked::Gen::escape_hatch_new(false);
                                    };
                                    let mut stmts = Vec::with_capacity(vars.len() + 2);
                                    let expr_span = expr.span();
                                    stmts.push(init_gen);
                                    stmts.push(stmt);
                                    stmts.extend(vars.iter().map(|var| {
                                    let var = Ident::new(&var.name, var.span);
                                    let spanned_expr = quote_spanned! {expr_span=> #avalanche_path::tracked::Tracked::new(#var, __avalanche_internal_gen)};
                                    parse_quote!{
                                        let #var = #spanned_expr;
                                    }
                                }));
                                    (stmts, deps, vars)
                                } else {
                                    (vec![stmt], deps, vars)
                                }
                            }
                        }
                    }
                    None => (vec![stmt], UnitDeps::new(), Vec::new()),
                };
                let scope = self.scopes.last_mut().unwrap();
                scope.vars.extend(vars);

                if !include_non_value_deps {
                    init_dependencies.has_tracked = false;
                }

                (stmts, init_dependencies)
            }
            Stmt::Item(item) => {
                let deps = match item {
                    syn::Item::Macro(macro_item) => {
                        let (mut deps, transformed) = self.mac(&mut macro_item.mac, false);
                        if let Some(transformed) = transformed {
                            stmt = Stmt::Semi(transformed, Semi(Span::call_site()));
                        }
                        if !include_non_value_deps {
                            deps.has_tracked = false;
                        }
                        deps
                    }
                    _ => UnitDeps::new(),
                };
                (vec![stmt], deps)
            }
            Stmt::Expr(expr) => {
                let deps = self.expr(expr, false);
                (vec![stmt], deps)
            }
            Stmt::Semi(expr, _) => {
                let mut escape_expr = self.escape_expr(expr, false);
                if !escape_expr.escape {
                    enable_expr_tracking(expr, &escape_expr.dependencies);
                }
                if !(include_non_value_deps || escape_expr.escape) {
                    escape_expr.dependencies.has_tracked = false;
                }
                (vec![stmt], escape_expr.dependencies)
            }
        }
    }

    /// Returns the dependencies of a macro, and the transformed version of the input.
    fn mac(&mut self, mac: &mut syn::Macro, nested_tracked: bool) -> (UnitDeps, Option<Expr>) {
        let avalanche_path = get_avalanche_path();
        let name = mac.path.segments.last().unwrap().ident.to_string();
        match &*name {
            "addr_of" | "addr_of_mut" => {
                if let Ok(mut expr) = mac.parse_body::<Expr>() {
                    let deps = self.expr(&mut expr, nested_tracked);
                    mac.tokens = expr.into_token_stream();
                    return (deps, None);
                }
            }
            // formatting macros
            "assert" | "assert_eq" | "assert_ne" | "debug_assert" | "debug_assert_eq"
            | "debug_assert_ne" | "eprint" | "eprintln" | "format" | "format_args" | "panic"
            | "print" | "println" => {
                if let Ok(mut format) = mac.parse_body::<ExprList>() {
                    let mut unit_deps = UnitDeps::new();
                    for expr in format.exprs.iter_mut() {
                        // interpret assignment as providing named parameter
                        if let Expr::Assign(assign) = expr {
                            unit_deps.extend(self.expr(&mut assign.right, nested_tracked));
                        } else {
                            unit_deps.extend(self.expr(expr, nested_tracked));
                        }
                    }
                    mac.tokens = format.into_token_stream();
                    return (unit_deps, None);
                }
            }
            "dbg" => {
                if let Ok(mut dbg) = mac.parse_body::<ExprList>() {
                    if dbg.exprs.len() == 1 {
                        let deps = self.expr(&mut dbg.exprs[0], nested_tracked);
                        mac.tokens = dbg.into_token_stream();
                        return (deps, None);
                    } else if dbg.exprs.len() > 1 {
                        let mut unit_deps = UnitDeps::new();
                        for expr in dbg.exprs.iter_mut() {
                            unit_deps.extend(self.expr(expr, nested_tracked))
                        }
                        mac.tokens = dbg.into_token_stream();
                        return (unit_deps, None);
                    }
                }
            }
            "enclose" => {
                if let Ok(mut enclose) = mac.parse_body::<EncloseBody>() {
                    let deps = self.expr(&mut enclose.expr, nested_tracked);
                    mac.tokens = enclose.into_token_stream();
                    return (deps, None);
                }
            }
            "matches" => {
                if let Ok(mut matches) = mac.parse_body::<MatchesBody>() {
                    let mut unit_deps = self.expr(&mut matches.expr, nested_tracked);
                    if let Some(if_expr) = &mut matches.if_expr {
                        unit_deps.extend(self.expr(if_expr, nested_tracked));
                    }
                    mac.tokens = matches.into_token_stream();
                    return (unit_deps, None);
                }
            }
            "try" => {
                if let Ok(mut try_) = mac.parse_body::<Try>() {
                    let deps = self.expr(&mut try_.expr, nested_tracked);
                    mac.tokens = try_.into_token_stream();
                    return (deps, None);
                }
            }
            "vec" => {
                if let Ok(mut vec) = mac.parse_body::<VecBody>() {
                    let unit_deps = match &mut vec {
                        VecBody::Repeat(repeat) => {
                            let mut deps = self.expr(&mut repeat.expr, nested_tracked);
                            deps.extend(self.expr(&mut repeat.n_expr, nested_tracked));
                            deps
                        }
                        VecBody::Literal(literal) => {
                            let mut unit_deps = UnitDeps::new();
                            for expr in literal.iter_mut() {
                                unit_deps.extend(self.expr(expr, nested_tracked));
                            }
                            unit_deps
                        }
                    };
                    mac.tokens = vec.into_token_stream();
                    return (unit_deps, None);
                }
            }
            "write" | "writeln" => {
                if let Ok(mut write) = mac.parse_body::<ExprList>() {
                    let mut unit_deps = UnitDeps::new();
                    for expr in write.exprs.iter_mut().skip(1) {
                        if let Expr::Assign(assign) = expr {
                            unit_deps.extend(self.expr(&mut assign.right, nested_tracked));
                        } else {
                            unit_deps.extend(self.expr(expr, nested_tracked));
                        }
                    }
                    mac.tokens = write.into_token_stream();
                    return (unit_deps, None);
                }
            }
            // TODO: factor out first part of the function
            "tracked" | "tracked_keyed" | "updated" | "updated_keyed" => {
                let is_keyed = matches!(&*name, "tracked_keyed" | "updated_keyed");
                if let Ok(tracked) = mac.parse_body::<Tracked>() {
                    let mac_span = mac.span();
                    let mut unit_deps = UnitDeps::new();
                    let is_expr_trivial = matches!(
                        tracked,
                        Tracked::Named(_) | Tracked::Unnamed(Expr::Reference(_))
                    );
                    let expr = match tracked {
                        Tracked::Named(ident) => {
                            unit_deps.tracked_deps.0.insert(ident.clone());
                            Expr::Path(ExprPath {
                                attrs: Vec::new(),
                                qself: None,
                                path: ident.into(),
                            })
                        }
                        Tracked::Unnamed(mut expr) => {
                            // An ident that is tracked by reference should still be tracked
                            // as a value dependency
                            if let Expr::Reference(reference) = &expr {
                                if let Expr::Path(path) = &*reference.expr {
                                    if let Some(ident) = path.path.get_ident() {
                                        unit_deps.tracked_deps.0.insert(ident.clone());
                                    }
                                }
                            };
                            unit_deps.extend(self.expr(&mut expr, is_keyed));
                            expr
                        }
                    };
                    let tracked_path = &mac.path;
                    let transformed = match &*name {
                        "tracked" | "tracked_keyed" => {
                            if nested_tracked {
                                quote_spanned! {mac_span=> #tracked_path!(#expr)}
                            } else if is_expr_trivial {
                                quote_spanned! {mac_span=>
                                    #tracked_path!({
                                        __avalanche_internal_gen = ::std::cmp::max((#expr).__avalanche_internal_gen, __avalanche_internal_gen);
                                        #expr
                                    })
                                }
                            } else {
                                quote_spanned! {mac_span=>
                                    #tracked_path!({
                                        let value = #expr;
                                        __avalanche_internal_gen = ::std::cmp::max(__avalanche_internal_gen, value.__avalanche_internal_gen);
                                        value
                                    })
                                }
                            }
                        }
                        "updated_keyed" => {
                            quote_spanned! {mac_span=> #tracked_path!(@internal __avalanche_hook_context; (#expr).__avalanche_internal_gen)}
                        }
                        "updated" => quote_spanned! {mac_span=>
                            {
                                let __avalanche_outer_gen = __avalanche_internal_gen;
                                // Other tracked expressions in the parent expression but outside of the current instance of
                                // updated!() should not influence the result
                                __avalanche_internal_gen = #avalanche_path::tracked::Gen::escape_hatch_new(false);
                                // #expr may modify __avalanche_internal_gen so this is not a noop
                                __avalanche_internal_gen = ::std::cmp::max((#expr).__avalanche_internal_gen, __avalanche_internal_gen);
                                let __avalanche_updated = #tracked_path!(@internal __avalanche_hook_context; __avalanche_internal_gen);
                                // Restore external gen, but update it if a higher one was found
                                __avalanche_internal_gen = ::std::cmp::max(__avalanche_outer_gen, __avalanche_internal_gen);
                                __avalanche_updated
                            }
                        },
                        _ => unreachable!(),
                    };
                    let transformed = parse_expr(transformed);
                    unit_deps.has_tracked = true;
                    return (unit_deps, Some(transformed));
                }
            }
            _ => {
                // TODO: warn about use of unknown macros? They work poorly with tracked macro calls
                // as inputs.
            }
        };
        (UnitDeps::new(), None)
    }

    /// Allow providing dependencies to closures being indirectly executed by functions.
    /// Returns trabsformed closure expr, which handles marking a closure as updated
    fn closure(&mut self, closure: &mut syn::ExprClosure, args_deps: UnitDeps) -> (UnitDeps, Expr) {
        let avalanche_path = get_avalanche_path();
        let mut closure_scope = Scope::function();

        for input in closure.inputs.iter() {
            let vars = vars_from_pat(input, args_deps.clone());
            closure_scope.vars.extend(vars);
        }

        self.scopes.push(closure_scope);
        // TODO: correct nested tracked arg?
        let deps = match &mut *closure.body {
            Expr::Block(expr) => self.closure_block(&mut expr.block),
            Expr::Async(expr) => self.closure_block(&mut expr.block),
            _ => {
                let mut deps = self.expr(&mut closure.body, false);
                deps.tracked_deps
                    .retain(|dep| self.get_var_function_scope(&dep.to_string()).is_none());
                deps
            }
        };
        let closure_body = &closure.body;
        closure.body = parse_quote! {
            {
                let mut __avalanche_internal_gen = #avalanche_path::tracked::Gen::escape_hatch_new(false);
                #closure_body
            }
        };
        let ident_dep = deps.tracked_deps.iter();
        let output = parse_quote! {
            {
                #(__avalanche_internal_gen = ::std::cmp::max(__avalanche_internal_gen, #ident_dep.__avalanche_internal_gen);)*
                #closure
            }
        };
        self.scopes.pop();

        (deps, output)
    }

    fn component(
        &mut self,
        call: syn::ExprCall,
        path: &Path,
        nested_tracked: bool,
    ) -> (UnitDeps, Expr) {
        let mut props = parse_component_call(call);

        let mut component_dependencies = UnitDeps::new();
        let mut prop_construct_expr = Vec::with_capacity(props.len());
        let avalanche_path = get_avalanche_path();

        // first prop must be the context `self`, which we skip
        for prop in props.iter_mut() {
            let dependencies = self.expr(&mut prop.value, nested_tracked);
            let field_ident = &prop.name;
            let init_expr = &prop.value;
            let field_span = prop.value.span();

            let construct_expr = if dependencies.has_tracked {
                quote_spanned! { field_span=>
                    #field_ident({
                        __avalanche_internal_gen = #avalanche_path::tracked::Gen::escape_hatch_new(false);
                        #init_expr
                    },
                    {
                        *__avalanche_internal_outer_gen = ::std::cmp::max(*__avalanche_internal_outer_gen, __avalanche_internal_gen);
                        __avalanche_internal_gen

                    })
                }
            } else {
                quote_spanned! { field_span=>
                    #field_ident(#init_expr, #avalanche_path::tracked::Gen::escape_hatch_new(false))
                }
            };
            prop_construct_expr.push(construct_expr);
            component_dependencies.extend(dependencies.clone());
        }
        // emulating Span information on stable
        // TODO: utilize actual Span info on nightly
        let line: u32 = random();
        let column: u32 = random();

        let transformed = parse_quote! {
            {
                let __avalanche_internal_built = #path::new(__avalanche_render_context.bump);
                let __avalanche_internal_outer_gen = &mut __avalanche_internal_gen;
                let mut __avalanche_internal_gen = #avalanche_path::tracked::Gen::escape_hatch_new(false);
                #avalanche_path::vdom::render_child(
                    __avalanche_internal_built
                    #(.#prop_construct_expr)*
                    .build((#line, #column)),
                    &__avalanche_render_context
                )
            }
        };

        (component_dependencies, transformed)
    }

    fn expr(&mut self, expr: &mut Expr, nested_tracked: bool) -> UnitDeps {
        self.escape_expr(expr, nested_tracked).dependencies
    }

    fn escape_expr(&mut self, expr: &mut Expr, nested_tracked: bool) -> EscapeExprRet {
        let mut dependencies: Option<UnitDeps> = None;
        let mut escape = false;

        match expr {
            Expr::Array(array) => {
                let mut unit_deps = UnitDeps::new();
                for expr in array.elems.iter_mut() {
                    unit_deps.extend(self.expr(expr, nested_tracked));
                }
                dependencies = Some(unit_deps);
            }
            Expr::Assign(assign) => {
                let mut rhs = self.expr(&mut assign.right, nested_tracked);
                rhs.has_tracked = false;
                enable_expr_tracking(&mut assign.right, &rhs);
                let vars = from_expr(&assign.left, rhs.clone());

                for var in vars.into_iter() {
                    if let Some(old) = self.get_var_mut(&var.name) {
                        old.dependencies = var.dependencies;
                    }
                }

                // TODO: should lhs be used as a return value
                self.expr(&mut assign.left, nested_tracked);

                dependencies = Some(rhs)
            }
            Expr::AssignOp(assign_op) => {
                let mut rhs = self.expr(&mut assign_op.right, nested_tracked);
                rhs.has_tracked = false;
                enable_expr_tracking(&mut assign_op.right, &rhs);
                let lhs_vars = from_expr(&assign_op.left, rhs.clone());
                for lhs_var in lhs_vars.into_iter() {
                    if let Some(var) = self.get_var_mut(&lhs_var.name) {
                        var.dependencies.extend(lhs_var.dependencies);
                    }
                }

                // TODO: should lhs be used as a return value
                self.expr(&mut assign_op.left, nested_tracked);

                dependencies = Some(rhs)
            }
            Expr::Async(async_) => dependencies = Some(self.block(&mut async_.block)),
            Expr::Await(await_) => {
                dependencies = Some(self.expr(&mut *await_.base, nested_tracked))
            }
            Expr::Binary(binary) => {
                let mut deps = self.expr(&mut binary.left, nested_tracked);
                let rhs = self.expr(&mut binary.right, nested_tracked);
                deps.extend(rhs);

                dependencies = Some(deps);
            }
            Expr::Block(block) => {
                dependencies = Some(self.block(&mut block.block));
            }
            Expr::Box(expr_box) => {
                dependencies = Some(self.expr(&mut expr_box.expr, nested_tracked))
            }
            Expr::Break(break_expr) => match &mut break_expr.expr {
                Some(expr) => {
                    dependencies = Some(self.expr(expr, nested_tracked));
                    escape = true;
                }
                None => {}
            },
            Expr::Call(call) => {
                let deps = match &*call.func {
                    Expr::Path(path) if is_component(call, &path.path) => {
                        let (deps, transformed) =
                            self.component(call.clone(), &path.path, nested_tracked);
                        *expr = transformed;
                        deps
                    }
                    _ => {
                        let mut deps = self.expr(&mut call.func, nested_tracked);
                        for arg in call.args.iter_mut() {
                            let arg_dep = self.expr(arg, nested_tracked);
                            deps.extend(arg_dep);
                        }
                        deps
                    }
                };

                dependencies = Some(deps);
            }
            Expr::Cast(cast) => {
                dependencies = Some(self.expr(&mut cast.expr, nested_tracked));
            }
            Expr::Closure(closure) => {
                // TODO: actual data for second arg? Otherwise, remove it from closure
                let (deps, transformed) = self.closure(closure, UnitDeps::new());
                dependencies = Some(deps);
                *expr = transformed;
            }
            Expr::Continue(_) => {}
            Expr::Field(field) => {
                let deps = self.expr(&mut field.base, nested_tracked);
                dependencies = Some(deps);
            }
            Expr::ForLoop(for_expr) => {
                // Create scope for the variables created by
                // for pat in expr {}
                let mut scope = Scope::new();

                let expr = self.expr(&mut for_expr.expr, nested_tracked);

                let vars = vars_from_pat(&for_expr.pat, expr);
                scope.vars = vars;
                self.scopes.push(scope);

                // Get dependencies of the for expr
                dependencies = Some(self.block(&mut for_expr.body));

                //variables created by pat no longer present
                self.scopes.pop();

                // TODO: effect deps
            }
            Expr::Group(group) => {
                dependencies = Some(self.expr(&mut group.expr, nested_tracked));
            }
            Expr::If(if_expr) => {
                // TODO: handle conditional dependency updates within block

                // the value of an if else branch doess NOT directly depend on the condition
                // as the actual dependencies are derived from the bodies, which are currently
                // unified
                // however, we must process it in order to account for let guards.

                //this scope is for variables created via let.
                let mut if_scope = Scope::new();
                let cond_dependencies = self.expr(&mut if_expr.cond, nested_tracked);

                if let Expr::Let(let_expr) = &mut *if_expr.cond {
                    let let_dependencies = self.expr(&mut let_expr.expr, nested_tracked);
                    if_scope.vars = vars_from_pat(&let_expr.pat, let_dependencies);
                }

                //allow vars created by let guard to be accessed in block
                self.scopes.push(if_scope);

                let mut deps = self.block(&mut if_expr.then_branch);
                deps.extend(cond_dependencies);
                match &mut if_expr.else_branch {
                    Some(else_branch) => deps.extend(self.expr(&mut else_branch.1, nested_tracked)),
                    None => {}
                }

                //remove vars created by if let
                self.scopes.pop();

                dependencies = Some(deps);
            }
            Expr::Index(index) => {
                let mut deps = self.expr(&mut index.expr, nested_tracked);
                let index = self.expr(&mut index.index, nested_tracked);
                deps.extend(index);
                dependencies = Some(deps);
            }
            Expr::Let(let_expr) => {
                // note: this should not be called,
                // but instead is special-cased by If
                // as variables may be created within its block
                // however, this is implemented here for completion's sake
                dependencies = Some(self.expr(&mut let_expr.expr, nested_tracked));
            }
            Expr::Lit(_) => {}
            Expr::Loop(loop_expr) => {
                dependencies = Some(self.block(&mut loop_expr.body));
            }
            Expr::Macro(macro_expr) => {
                let (deps, transformed) = self.mac(&mut macro_expr.mac, nested_tracked);
                if let Some(transformed) = transformed {
                    *expr = transformed;
                }
                dependencies = Some(deps);
            }
            Expr::Match(match_expr) => {
                let mut deps = self.expr(&mut match_expr.expr, nested_tracked);
                for arm in match_expr.arms.iter_mut() {
                    deps.extend(self.expr(&mut arm.body, nested_tracked));
                    if let Some(if_guard) = &mut arm.guard {
                        deps.extend(self.expr(&mut *if_guard.1, nested_tracked));
                    }
                }
                dependencies = Some(deps);
            }
            Expr::MethodCall(method) => {
                let mut deps = self.expr(&mut method.receiver, nested_tracked);
                for arg in method.args.iter_mut() {
                    deps.extend(self.expr(arg, nested_tracked));
                }
                dependencies = Some(deps)
            }
            Expr::Paren(paren) => {
                dependencies = Some(self.expr(&mut paren.expr, nested_tracked));
            }
            Expr::Path(path) => {
                // No dependencies; dependencies are only created by tracked!()
                // macro

                if path.path.is_ident("self") {
                    path.path = Ident::new("__avalanche_hook_context", path.path.span()).into()
                }
            }
            Expr::Range(range) => {
                // range.from.as_ref().map(|r| dependencies.extend(self.expr(&r).drain()));
                // range.to.as_ref().map(|r| dependencies.extend(self.expr(&r).drain()));
                //let range = range.clone();
                let mut deps = range.from.as_mut().map(|r| self.expr(r, nested_tracked));
                if let Some(to) = &mut range.to {
                    match &mut deps {
                        Some(deps) => {
                            deps.extend(self.expr(to, nested_tracked));
                        }
                        None => {
                            deps = Some(self.expr(to, nested_tracked));
                        }
                    }
                }
                dependencies = deps
            }
            Expr::Reference(reference) => {
                dependencies = Some(self.expr(&mut reference.expr, nested_tracked));
            }
            Expr::Repeat(repeat) => {
                dependencies = Some(self.expr(&mut repeat.expr, nested_tracked));
            }
            Expr::Return(ret) => match &mut ret.expr {
                Some(expr) => {
                    dependencies = Some(self.expr(expr, nested_tracked));
                    escape = true;
                }
                None => {}
            },
            // TODO: handle rest
            Expr::Struct(struct_expr) => {
                let mut deps = struct_expr
                    .rest
                    .as_mut()
                    .map(|rest| self.expr(rest, nested_tracked));
                for field in struct_expr.fields.iter_mut() {
                    match &mut deps {
                        Some(deps) => {
                            deps.extend(self.expr(&mut field.expr, nested_tracked));
                        }
                        None => {
                            deps = Some(self.expr(&mut field.expr, nested_tracked));
                        }
                    }
                }
                dependencies = deps;
            }
            Expr::Try(try_expr) => {
                dependencies = Some(self.expr(&mut try_expr.expr, nested_tracked));
            }
            Expr::TryBlock(try_block) => {
                dependencies = Some(self.block(&mut try_block.block));
            }
            Expr::Tuple(tuple) => {
                let mut deps = UnitDeps::new();
                for expr in tuple.elems.iter_mut() {
                    deps.extend(self.expr(expr, nested_tracked));
                }

                dependencies = Some(deps);
            }
            Expr::Type(_) => {}
            Expr::Unary(unary) => {
                dependencies = Some(self.expr(&mut unary.expr, nested_tracked));
            }
            Expr::Unsafe(unsafe_block) => {
                dependencies = Some(self.block(&mut unsafe_block.block));
            }
            Expr::Verbatim(_) => {}
            Expr::While(while_expr) => {
                //while loops have value ()
                //() has no dependencies, so none are returned here
                dependencies = Some(self.block(&mut while_expr.body));
            }
            Expr::Yield(_) => {
                abort!(expr, "yield unsupported")
            }
            _ => {
                eprintln!("warn: unexpected Expr type encountered. this version of avalanche may not support your version of rustc");
            }
        }

        EscapeExprRet {
            dependencies: dependencies.unwrap_or_default(),
            escape,
        }
    }
}

struct EscapeExprRet {
    dependencies: UnitDeps,
    // whether this expression may result in a jump from its enclosing block
    // examples: return, break, yield in generators (although unsupported)
    escape: bool,
}

fn vars_from_pat(lhs: &Pat, rhs: UnitDeps) -> Vec<Var> {
    match lhs {
        Pat::Box(box_) => return vars_from_pat(&box_.pat, rhs),
        Pat::Ident(ident) => {
            return vec![Var {
                name: ident.ident.to_string(),
                dependencies: rhs,
                span: ident.span(),
            }]
        }
        Pat::Lit(_) => {}
        Pat::Macro(_) => {}
        Pat::Or(or_pat) => {
            let mut vars = HashSet::new();
            for pat in or_pat.cases.iter() {
                vars.extend(vars_from_pat(pat, rhs.clone()));
            }
            return vars.into_iter().collect();
        }
        Pat::Path(path) => {
            let segments = &path.path.segments;
            if segments.len() == 1 {
                let ident = &segments.first().unwrap().ident;
                return vec![Var {
                    name: ident.to_string(),
                    dependencies: rhs,
                    span: ident.span(),
                }];
            }
        }
        Pat::Range(_) => {}
        Pat::Reference(reference) => {
            return vars_from_pat(&reference.pat, rhs);
        }
        Pat::Rest(_) => return vec![],
        Pat::Slice(slice) => {
            let mut fields = Vec::with_capacity(slice.elems.len());
            for elem in slice.elems.iter() {
                fields.push(elem)
            }
            return from_pat_numbered(&fields, rhs);
        }
        Pat::Struct(struct_pat) => {
            let mut fields = Vec::with_capacity(struct_pat.fields.len());

            for field in struct_pat.fields.iter() {
                fields.push(&*field.pat);
            }

            return from_pat_numbered(&fields, rhs);
        }
        Pat::Tuple(tuple) => {
            return from_pat_tuple(tuple, rhs);
        }
        Pat::TupleStruct(tuple_struct) => {
            return from_pat_tuple(&tuple_struct.pat, rhs);
        }
        Pat::Type(type_pat) => {
            return vars_from_pat(&type_pat.pat, rhs);
        }
        Pat::Verbatim(_) => {}
        Pat::Wild(_) => {}
        _ => {
            eprintln!("Warn: unknown Pat type.");
        }
    }

    vec![]
}

fn from_pat_tuple(lhs: &syn::PatTuple, rhs: UnitDeps) -> Vec<Var> {
    let mut vars = Vec::with_capacity(lhs.elems.len());

    for elem in lhs.elems.iter() {
        vars.extend(vars_from_pat(elem, rhs.clone()));
    }

    vars
}

fn from_pat_numbered(lhs: &[&Pat], rhs: UnitDeps) -> Vec<Var> {
    let mut vars = Vec::new();
    for pat in lhs.iter() {
        vars.extend(vars_from_pat(pat, rhs.clone()));
    }
    vars
}

fn from_expr(expr: &Expr, rhs: UnitDeps) -> Vec<Var> {
    //note: we allow the compiler to handle invalid lhs
    match expr {
        Expr::Array(array) => {
            let mut exprs = Vec::with_capacity(array.elems.len());

            for expr in array.elems.iter() {
                exprs.push(expr);
            }
            return from_expr_numbered(&exprs, rhs);
        }
        Expr::Assign(_) => {
            // no-op: assigns return ()
        }
        Expr::AssignOp(_) => {
            // ditto
        }
        Expr::Async(_) => {
            // illegal in lhs pos
        }
        Expr::Await(_) => {
            //illegal in lhs pos
        }
        Expr::Binary(_) => {
            // no-op: expr creates temporary
        }
        Expr::Block(_) => {
            // illegal in lhs pos
        }
        Expr::Box(_) => {
            // no-op: creates temporary
        }
        Expr::Break(_) => {
            // illegal in lhs pos
        }
        Expr::Call(_) => {
            // no vars
        }
        Expr::Cast(_) => {
            // no-op: expr creates temporary
        }
        Expr::Closure(_) => {
            // illegal in lhs pos
        }
        Expr::Continue(_) => {
            // illegal in lhs pos
        }
        Expr::Field(field) => {
            return from_expr(&field.base, rhs);
        }
        Expr::ForLoop(_) => {
            // illegal in lhs pos
        }
        Expr::Group(group) => {
            return from_expr(&group.expr, rhs);
        }
        Expr::If(_) => {
            // illegal in lhs pos
        }
        Expr::Index(index) => {
            if let Expr::Lit(lit) = &*index.index {
                if let Lit::Int(int) = &lit.lit {
                    if int.base10_parse::<usize>().is_ok() {
                        return from_expr(&index.expr, rhs);
                    }
                }
            };
            return from_expr(&index.expr, rhs);
        }
        Expr::Let(_) => {
            // illegal in lhs pos
        }
        Expr::Lit(_) => {
            // illegal in lhs pos
        }
        Expr::Loop(_) => {
            // illegal in lhs pos
        }
        Expr::Macro(_) => {}
        Expr::Match(_) => {
            // illegal in lhs pos
        }
        Expr::MethodCall(_) => {
            // no vars
        }
        Expr::Paren(paren) => {
            return from_expr(&paren.expr, rhs);
        }
        Expr::Path(path) => {
            let segments = &path.path.segments;
            if segments.len() == 1 {
                let ident = &segments.first().unwrap().ident;
                return vec![Var {
                    name: ident.to_string(),
                    dependencies: rhs,
                    span: ident.span(),
                }];
            }
        }
        Expr::Range(_) => {
            // illegal in lhs pos
        }
        Expr::Reference(_) => {
            // illegal in lhs pos
        }
        Expr::Repeat(_) => {
            // illegal in lhs pos
        }
        Expr::Return(_) => {
            // illegal in lhs pos
        }
        Expr::Struct(_) => {
            // illegal in lhs pos
        }
        Expr::Try(_) => {
            // illegal in lhs pos
        }
        Expr::TryBlock(_) => {
            // illegal in lhs pos
        }
        Expr::Tuple(tuple) => {
            let mut exprs = Vec::with_capacity(tuple.elems.len());

            for elem in tuple.elems.iter() {
                exprs.push(elem);
            }
            return from_expr_numbered(&exprs, rhs);
        }
        Expr::Type(_) => {
            // nothing to do
        }
        Expr::Unary(_) => {
            // temp
        }
        Expr::Unsafe(_) => {
            // illegal in lhs pos
        }
        Expr::Verbatim(_) => {
            // nothing can be done
        }
        Expr::While(_) => {
            // illegal in lhs pos
        }
        Expr::Yield(_) => {
            // illegal in lhs pos
        }
        _ => {
            eprintln!("Warn: unknown Expr type.");
        }
    }

    Vec::new()
}

fn from_expr_numbered(lhs: &[&Expr], rhs: UnitDeps) -> Vec<Var> {
    let mut vars = Vec::new();
    for expr in lhs.iter() {
        vars.extend(from_expr(expr, rhs.clone()));
    }
    vars
}

/// Whether the given path and first parameter indicate a function call is a component call.
/// True if the (qualified) identifier is not raw and begins with a capital ASCII character,
fn is_component(call: &syn::ExprCall, path: &Path) -> bool {
    let valid_name = path
        .segments
        .last()
        .and_then(|last| last.ident.to_string().chars().next())
        .map(|first_char| first_char.is_ascii_uppercase())
        .unwrap_or(false);
    let valid_self = call.args.first().map_or(false, |param| match param {
        Expr::Path(path) => path.path.get_ident().map_or(false, |ident| ident == "self"),
        _ => false,
    });
    valid_name && valid_self
}
struct ComponentProp {
    name: Ident,
    value: Expr,
}

fn parse_component_call(call: syn::ExprCall) -> Vec<ComponentProp> {
    let mut props = Vec::with_capacity(call.args.len());
    let args_len = call.args.len();
    // Skip first parameter which is just context `self`
    for (i, arg) in call.args.into_iter().enumerate().skip(1) {
        match arg {
            Expr::Assign(assign) => match *assign.left {
                Expr::Path(path) => match path.path.get_ident() {
                    Some(ident) => props.push(ComponentProp {
                        name: ident.clone(),
                        value: *assign.right,
                    }),
                    None => abort!(
                        path,
                        "expected an identifer referring to a component property"
                    ),
                },
                left => abort!(
                    left,
                    "expected an identifer referring to a component property"
                ),
            },
            trailing_prop if i + 1 == args_len => props.push(ComponentProp {
                name: Ident::new("__last", trailing_prop.span()),
                value: trailing_prop,
            }),
            illegal_prop => abort!(
                illegal_prop,
                "expected a component property in the form of `prop_name=value_expr`";
                note = "an unnamed component property is only allowed as the last argument of a component call"
            ),
        }
    }
    props
}
