#[cfg(test)]
mod tests {
    
    
}

mod macro_expr;

use proc_macro::TokenStream;
use std::collections::HashMap;

use syn::{Item, Block, Stmt, Expr, Pat, Lit, Ident, parse_macro_input, parse_quote, parse::Parse, punctuated::Punctuated};
use quote::{quote, format_ident};
use proc_macro_error::{proc_macro_error, abort, emit_error};

use macro_expr::{ReactiveAssert, ComponentInit, Hooks};

#[derive(Debug, Clone)]
enum DependencyInfo {
    ///The path to the dependency's relevant subpart,
    ///e.g. if a is a value provided by UseState,
    ///a.0 would have sub `Some(vec![0])`
    Hook(HookInfo),
    ///Prop update bit pattern: 
    ///bitwise and of this value with `updates`
    ///determines whether the prop has been updated
    Prop(u64)
}

#[derive(Debug, Clone)]
struct HookInfo {
    sub: Vec<usize>,
    hook_update_ident: Ident
}

type Dependencies = HashMap<Ident, DependencyInfo>;

#[derive(Debug)]
struct Var {
    name: String,
    dependencies: ExprType
}

#[derive(Debug, Clone)]
enum ExprType {
    //represents tuples and arrays with known fields
    NumberedFields(Vec<ExprType>),
    Closure(Closure),
    Opaque(Dependencies)
}

impl ExprType {
    fn unify(&self) -> Dependencies {
        match self {
            ExprType::NumberedFields(fields) => {
                let mut dependencies = Dependencies::new();
                for field in fields.iter() {
                    dependencies.extend(field.unify());
                }

                dependencies
            }
            ExprType::Closure(closure) => {
                closure.value_dependencies.clone()
            }
            ExprType::Opaque(opaque) => {
                opaque.clone()
            }
        }
    }

    fn extend(&mut self, other: ExprType) {
        match self {
            ExprType::NumberedFields(fields) => {
                for field in fields.iter_mut() {
                    field.extend(other.clone());
                }
            }
            ExprType::Closure(_) => {
                //todo: should anything be done?
                //no valid way of modifying a closure except by callig it
                //should a closure be extended by its parameters?
            }
            ExprType::Opaque(opaque) => {
                opaque.extend(other.unify());
            }
        }
    }

    ///Gets the sub-dependency path for a Hook
    ///Returns `None` if it is not opaque
    fn get_hook_sub(&self) -> Option<&Vec<usize>> {
        if let ExprType::Opaque(dependencies) = self {
            if dependencies.len() == 1 {
                match &dependencies.iter().next().unwrap().1 {
                    DependencyInfo::Hook(hook_info) => return Some(&hook_info.sub),
                    _ => return None
                }
            }
        }
        None
    }

    ///Returns whether push succeeded
    fn append_to_sub(&mut self, index: usize) -> bool {
        if let Self::Opaque(opaque) = self {
            if opaque.len() == 1 {
                let (_, dep_info) = opaque.iter_mut().next().unwrap();
                if let DependencyInfo::Hook(hook_info) = dep_info {
                    hook_info.sub.push(index);
                    return true;
                }
            }
        };
        false
    }
}

#[derive(Debug, Clone)]
struct Closure {
    value_dependencies: Dependencies,
    call_dependencies: Dependencies
}

#[derive(Default, Debug)]
struct Scope {
    vars: Vec<Var>
}

impl Scope {
    fn new() -> Self {
        Default::default()
    }
}

#[derive(Default, Debug)]
struct Function {
    scopes: Vec<Scope>
}

impl Function {
    fn new() -> Self {
        Default::default()
    }

    fn get_var(&self, name: &str) -> Option<&Var> {
        for scope in self.scopes.iter().rev() {
            match scope.vars.iter().rev().find(|item| item.name == name) {
                Some(var) => return Some(var),
                None => continue,
            }
        };

        None
    }

    fn get_var_mut(&mut self, name: &str) -> Option<&mut Var> {
        for scope in self.scopes.iter_mut().rev() {
            match scope.vars.iter_mut().rev().find(|item| item.name == name) {
                Some(var) => return Some(var),
                None => continue,
            }
        };

        None
    }

    ///Find a prop or hook value, if it exists.
    fn get_input_var(&self, name: &str) -> Option<&Var> {
        let scope = &self.scopes[0];
        scope.vars.iter().rev().find(|item| item.name == name)
    }

    fn block(&mut self, block: &mut Block) -> ExprType {
        let mut dependencies: Option<ExprType> = None;

        self.scopes.push(Scope::new());
        for stmt in &mut block.stmts {
            if let Some(stmt_dependencies) = self.stmt(stmt) {
                match &mut dependencies {
                    Some(deps) => {
                        deps.extend(stmt_dependencies);
                    }
                    None => {
                        dependencies = Some(stmt_dependencies);
                    }
                }
            }
        }
        self.scopes.pop();


        //todo: add dependencies of last statement
        dependencies.unwrap_or_else(|| ExprType::Opaque(Dependencies::new()))
    }

    fn stmt(&mut self, stmt: &mut Stmt) -> Option<ExprType> {
        match stmt {
            Stmt::Local(local) => {
                let init_dependencies = match &mut local.init {
                    Some(expr) => self.expr(&mut expr.1),
                    None => ExprType::Opaque(Dependencies::new())
                };
                let scope = self.scopes.last_mut().unwrap();
                let ident_pack = IdentPack::from_pat(&local.pat, &init_dependencies);
                let vars = ident_pack.to_vars(init_dependencies);
                scope.vars.extend(vars);
            },
            Stmt::Item(item) => {
                match item {
                    syn::Item::Macro(macro_item) => {
                        self.mac(macro_item.mac.clone(), macro_item);
                    }
                    _ => {}
                }
            },
            Stmt::Expr(expr) => {
                return Some(self.expr(expr));
            },
            Stmt::Semi(expr, _) => {
                let escape_expr = self.escape_expr(expr);
                if escape_expr.escape {
                    return Some(escape_expr.dependencies);
                }
            }
        };

        None
    }

    fn mac<P: Parse>(&mut self, mac: syn::Macro, syntax: &mut P) {
        let name = mac.path.segments.last().unwrap().ident.to_string();
        match &*name {
            "reactive_assert" => {
                if let Ok(reactive_assert) = mac.parse_body::<ReactiveAssert>() { 
                    for assert in reactive_assert.asserts {
                        let dependency_names: Vec<_> = assert.dependencies.iter().map(|d| d.to_string()).collect();
                        let dependent_name = assert.dependent.to_string();

                        let dependent = match self.get_var(&dependent_name) {
                            Some(var) => var,
                            None => {
                                abort!(assert.dependent.span(), "cannot find identifier `{}` in this scope", dependent_name);
                            }
                        };

                        for (dependency_ident, dependency_name) in assert.dependencies.iter().zip(dependency_names.iter()) {
                            if let None = self.get_var(&dependency_name) {
                                abort!(dependency_ident.span(), "cannot find identifier `{}` in this scope", dependency_name);
                            }

                            println!("dependent: {:#?}", dependent);

                            let dependent_dependencies = dependent.dependencies.unify();

                            if let None = dependent_dependencies.get(dependency_ident) {
                                let dependencies: Vec<_> = dependent_dependencies.iter().map(|(ident, _)| ident.to_string()).collect();
                                let dependencies_string: String = dependencies.join(", ");

                                abort!{
                                    assert,
                                    "dependencies for `{}` not satisfied",
                                    dependent.name;
                                        note = "`{}` has dependencies {}", 
                                        dependent.name, 
                                        dependencies_string;
                                }
                            }

                        }
                    }
                }
            }
            _ => {
                if name.chars().next().unwrap().is_ascii_uppercase() {
                    match mac.parse_body::<ComponentInit>() {
                        Ok(mut init) => {
                            let mut field_ident = Vec::with_capacity(init.fields.len());
                            let mut init_expr = Vec::with_capacity(init.fields.len());
                            let mut is_field_updated = Vec::with_capacity(init.fields.len());
                            for field in init.fields.iter_mut() {
                                let ident = match &field.member {
                                    syn::Member::Named(ident) => ident,
                                    syn::Member::Unnamed(u) => abort!(u, "expected named field")
                                };
                                field_ident.push(ident);
                                let dependencies = self.expr(&mut field.expr).unify();
                                init_expr.push(&field.expr);
                                let mut dependency_update = Vec::with_capacity(dependencies.len());
                                for (name, dependency_info) in dependencies.iter() {
                                    match dependency_info {
                                        DependencyInfo::Hook(HookInfo {sub, hook_update_ident}) => {
                                            println!("name: {} sub: {:#?}", name, sub);
                                            dependency_update.push(
                                                quote! {#hook_update_ident.index( &[ #( #sub ),* ] )}
                                            );
                                        }
                                        DependencyInfo::Prop(prop) => {
                                            dependency_update.push(
                                                quote!{self.__internal_updates & #prop}
                                            );
                                        }
                                    } 
                                };
                                is_field_updated.push(
                                    quote!{ false #(|| #dependency_update)* }
                                );
                            }
                            let type_path = &mac.path;
                            
                            println!("{}", quote! {
                                avalanche::__internal_identity! {
                                    <<#type_path as ::avalanche::Component>::Builder>::new()
                                    #(.#field_ident(#init_expr, #is_field_updated))*
                                    .build()
                                    .into()
                                }
                            });
                            println!("{} {} {}", field_ident.len(), init_expr.len(), is_field_updated.len());

                            *syntax = parse_quote! {
                                avalanche::__internal_identity! {
                                    <<#type_path as ::avalanche::Component>::Builder>::new()
                                    #(.#field_ident(#init_expr, #is_field_updated))*
                                    .build()
                                    .into()
                                }
                            };
                        }
                        Err(err) => {
                            abort!(mac, err.to_string());
                        }
                    }
                }
            }
        }
    }

    fn expr(&mut self, expr: &mut Expr) -> ExprType {
        self.escape_expr(expr).dependencies
    }

    fn escape_expr(&mut self, expr: &mut Expr) -> EscapeExprRet {
        let mut dependencies: Option<ExprType> = None;
        let mut escape = false;

        match expr {
            Expr::Array(array) => {
                let mut fields = Vec::with_capacity(array.elems.len());
                for expr in array.elems.iter_mut() {
                    let expr_deps = self.expr(expr);
                    fields.push(expr_deps);
                }

                dependencies = Some(ExprType::NumberedFields(fields));
            }
            Expr::Assign(assign) => {
                let lhs_idents = expr_lhs_ident_pack(&assign.left);
                let rhs = self.expr(&mut assign.right);
                let vars = lhs_idents.to_vars(rhs);

                for var in vars.into_iter() {
                    if let Some(old) = self.get_var_mut(&var.name) {
                        old.dependencies = var.dependencies;
                    }
                }

                //note: no dependencies need be set
                //assignment returns ()
            }
            Expr::AssignOp(assign_op) => {
                let lhs_ident_pack = expr_lhs_ident_pack(&assign_op.left);
                let rhs_dependencies = self.expr(&mut assign_op.right);
                let lhs_vars = lhs_ident_pack.to_vars(rhs_dependencies);
                for lhs_var in lhs_vars.into_iter() {
                    if let Some(var) = self.get_var_mut(&lhs_var.name) {
                        var.dependencies.extend(lhs_var.dependencies);
                    }
                }

                //note: no dependencies need be set
                //assignment returns ()
            }
            Expr::Async(_) => {
                abort!(expr, "async blocks unsupported")
            }
            Expr::Await(_) => {
                abort!(expr, "await expressions unsupported")
            }
            Expr::Binary(binary) => {
                let mut deps = self.expr(&mut binary.left);
                let rhs = self.expr(&mut binary.right);
                deps.extend(rhs);

                dependencies = Some(deps);

            }
            Expr::Block(block) => {
                dependencies = Some(self.block(&mut block.block));
            }
            Expr::Box(expr_box) => {
                dependencies = Some(self.expr(&mut expr_box.expr))
            }
            Expr::Break(break_expr) => {
                match &mut break_expr.expr {
                    Some(expr) => {
                        dependencies = Some(self.expr(expr));
                        escape = true;
                    },
                    None => {}
                }
            }
            Expr::Call(call) => {
                let mut deps = self.expr(&mut call.func);
                for arg in call.args.iter_mut() {
                    let arg_dep = self.expr(arg);
                    deps.extend(arg_dep);
                }

                dependencies = Some(deps);
            }
            Expr::Cast(cast) => {
                dependencies = Some(self.expr(&mut cast.expr));
            }
            Expr::Closure(closure) => {
                let mut closure_scope = Scope::new();

                for input in closure.inputs.iter() {
                    let rhs = ExprType::Opaque(Dependencies::new());
                    let idents = IdentPack::from_pat(&input, &rhs);
                    let vars = idents.to_vars(rhs);
                    closure_scope.vars.extend(vars);
                }

                self.scopes.push(closure_scope);

                let closure = Closure {
                    call_dependencies: self.expr(&mut closure.body).clone().unify(),
                    //TODO: read below
                    //this is currently incorrect
                    value_dependencies: self.expr(&mut closure.body).unify(), 
                };
                
                dependencies = Some(ExprType::Closure(closure));

                self.scopes.pop();

                //TODO: this is wildly incomplete
                //there are two different types of dependencies a closure has:
                //1. the returned value's
                //2. the actions performed by the function
                //currently, this accounts for only 1.
                //however, the actions performed are extremely important for callbacks


            }
            Expr::Continue(_) => {}
            Expr::Field(field) => {
                let mut deps = self.expr(&mut field.base);
                if let syn::Member::Unnamed(unnamed) = &field.member {
                    //if we do not affect sub, we must not return, as if we did,
                    //a render function's return might depend on what index is used,
                    //but we do not add that to dependencies here.
                    if deps.append_to_sub(unnamed.index as usize) {
                        return EscapeExprRet {
                            dependencies: deps,
                            escape: false
                        };
                    };
                    match &deps {
                        ExprType::NumberedFields(fields) => {
                            if let Some(field_deps) = fields.get(unnamed.index as usize) {
                                return EscapeExprRet{
                                    dependencies: field_deps.clone(),
                                    escape: false
                                };
                            }
                        }
                        _ => {}
                    }
                };
                dependencies = Some(deps);
            }
            Expr::ForLoop(for_expr) => {
                //create scope for the variables created by 
                //for pat in expr {}
                let mut scope = Scope::new();

                let expr = self.expr(&mut for_expr.expr);

                //create variables for those created by pat
                let ident_pack = IdentPack::from_pat(&for_expr.pat, &expr);
                let vars = ident_pack.to_vars(expr);
                scope.vars = vars;
                self.scopes.push(scope);

                //get dependencies of the for expr
                dependencies = Some(self.block(&mut for_expr.body));

                //variables created by pat no longer present
                self.scopes.pop();
            }
            Expr::Group(group) => {
                dependencies = Some(self.expr(&mut group.expr));
            }
            Expr::If(if_expr) => {
                //TODO: handle conditional dependency updates within block

                //the value of an if else branch doess NOT directly depend on the condition
                //as the actual dependencies are derived from the bodies, which are currently
                //unified
                //however, we must process it in order to account for let guards.

                //this scope is for variables created via let.
                let mut if_scope = Scope::new();
                let cond_dependencies = self.expr(&mut if_expr.cond);
                match &mut *if_expr.cond {
                    Expr::Let(let_expr) => {
                        let let_dependencies = self.expr(&mut let_expr.expr);
                        let ident_pack = IdentPack::from_pat(&let_expr.pat, &let_dependencies);
                        if_scope.vars = ident_pack.to_vars(let_dependencies);
                    },
                    _ => {}
                }

                //allow vars created by let guard to be accessed in block
                self.scopes.push(if_scope);

                let mut deps = self.block(&mut if_expr.then_branch);
                deps.extend(cond_dependencies);
                match &mut if_expr.else_branch {
                    Some(else_branch) => {
                        deps.extend(self.expr(&mut else_branch.1))
                    }
                    None => {}
                }

                //remove vars created by if let
                self.scopes.pop();

                dependencies = Some(deps);
            }
            Expr::Index(index) => {
                let mut deps = self.expr(&mut index.expr);
                if let Expr::Lit(lit) = &*index.index {
                    if let Lit::Int(int) = &lit.lit {
                        if let Ok(index) = int.base10_parse::<usize>() {
                            //if we do not affect sub, we must not return, as if we did,
                            //a render function's return might depend on what index is used,
                            //but we do not add that to dependencies here.
                            if deps.append_to_sub(index) {
                                return EscapeExprRet {
                                    dependencies: deps,
                                    escape: false
                                };
                            };
                            match &deps {
                                ExprType::NumberedFields(fields) => {
                                    if let Some(field_deps) = fields.get(index) {
                                        return EscapeExprRet{
                                            dependencies: field_deps.clone(),
                                            escape: false
                                        };
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
                //catch-all if specific cases above aren't applicable
                let index = self.expr(&mut index.index);
                deps.extend(index);
                dependencies = Some(deps);
            }
            Expr::Let(let_expr) => {
                //note: this should not be called,
                //but instead is special-cased by If
                //as variables may be created within its block
                //however, this is implemented here for completion's sake

                dependencies = Some(self.expr(&mut let_expr.expr));
            }
            Expr::Lit(_) => {}
            Expr::Loop(loop_expr) => {
                dependencies = Some(self.block(&mut loop_expr.body));
            }
            Expr::Macro(macro_expr) => {
                self.mac(macro_expr.mac.clone(), expr);
            }
            Expr::Match(match_expr) => {
                let mut deps = self.expr(&mut match_expr.expr);
                for arm in match_expr.arms.iter_mut() {
                    //note: identifiers in match patterns are not processed
                    //because those only contain dependencies already added 
                    deps.extend(self.expr(&mut arm.body));
                }
                dependencies = Some(deps);
            }
            Expr::MethodCall(method) => {
                let mut deps = self.expr(&mut method.receiver);
                for arg in method.args.iter_mut() { 
                    deps.extend(self.expr(arg));
                }
                dependencies = Some(deps)

                //TODO: handle lhs dependency updates?
            }
            Expr::Paren(paren) => {
                dependencies = Some(self.expr(&mut paren.expr));
            }
            Expr::Path(path) => {
                let segments = &path.path.segments;
                if segments.len() == 1 {
                    let ident_name = segments.first().unwrap().ident.to_string();
                    match self.get_var(&ident_name) {
                        Some(var) => {
                            dependencies = Some(var.dependencies.clone());
                        }
                        None => {}
                    }
                }
            }
            Expr::Range(range) => {
                // range.from.as_ref().map(|r| dependencies.extend(self.expr(&r).drain()));
                // range.to.as_ref().map(|r| dependencies.extend(self.expr(&r).drain()));
                //let range = range.clone();
                let mut deps = range.from.as_mut().map(|r| self.expr(r));
                if let Some(to) = &mut range.to {
                    match &mut deps {
                        Some(deps) => {
                            deps.extend(self.expr(to));
                        }
                        None => {
                            deps = Some(self.expr(to));
                        }
                    }
                }
                dependencies = deps
            }
            Expr::Reference(reference) => {
                dependencies = Some(self.expr(&mut reference.expr));
            }
            Expr::Repeat(repeat) => {
                dependencies = Some(self.expr(&mut repeat.expr));
            }
            Expr::Return(ret) => {
                match &mut ret.expr {
                    Some(expr) => {
                        dependencies = Some(self.expr(expr));
                        escape = true;
                    },
                    None => {}
                }
            }
            Expr::Struct(struct_expr) => {
                let mut deps: Option<ExprType> = None;
                for field in struct_expr.fields.iter_mut() {
                    match &mut deps {
                        Some(deps) => {
                            deps.extend(self.expr(&mut field.expr));
                        }
                        None => {
                            deps = Some(self.expr(&mut field.expr));
                        }
                    }
                }
                dependencies = deps;
            }
            Expr::Try(try_expr) => {
                dependencies = Some(self.expr(&mut try_expr.expr));
            }
            Expr::TryBlock(try_block) => {
                dependencies = Some(self.block(&mut try_block.block));
            }
            Expr::Tuple(tuple) => {
                let mut elems = Vec::with_capacity(tuple.elems.len());
                for expr in tuple.elems.iter_mut() {
                    let expr_deps = self.expr(expr);
                    elems.push(expr_deps);
                }

                dependencies = Some(ExprType::NumberedFields(elems));
            }
            Expr::Type(_) => {}
            Expr::Unary(unary) => {
                dependencies = Some(self.expr(&mut unary.expr));
            }
            Expr::Unsafe(unsafe_block) => {
                dependencies = Some(self.block(&mut unsafe_block.block));
            }
            Expr::Verbatim(_) => {
            }
            Expr::While(while_expr) => {
                //while loops have value ()
                //() has no dependencies, so none are returned here
                dependencies = Some(self.block(&mut while_expr.body));
            }
            Expr::Yield(_) => {
                abort!(expr, "yield unsupported")
            }
            _ => {
                eprintln!("Warn: unexpected Expr type encountered.");
            }
        }

        EscapeExprRet {
            dependencies: dependencies.unwrap_or_else(|| ExprType::Opaque(Dependencies::new())),
            escape
        }
    }
}

struct EscapeExprRet {
    dependencies: ExprType,
    //whether this expression may result in a jump from its
    //enclosing block
    //examples: return, break, yield in generators (although unsupported)
    escape: bool
}

#[derive(Debug, Clone)]
struct Atom {
    name: String,
    sub: Option<Vec<usize>>
}

impl From<String> for Atom {
    fn from(name: String) -> Self {
        Self {
            name,
            sub: None
        }
    }
    
}

#[derive(Debug, Clone)]
enum IdentPack {
    //indivisible ident
    //note: dependency contains name and potentially sub
    //but the actual variable referred to by the name may have other dependencies
    Atom(Atom),
    Nested(Vec<IdentPack>),
    //for the .. in tuples and arrays
    Rest,
    //when we don't handle a category
    None
}


impl IdentPack {
    fn to_vars(self, rhs: ExprType) -> Vec<Var> {
        match self {
            IdentPack::Atom(atom) => {
                let var = Var {
                    name: atom.name,
                    dependencies: rhs
                };
                vec![var]
            }
            IdentPack::Nested(nested) => {
                match rhs {
                    ExprType::NumberedFields(fields) => {
                        //tracks whether the .. pattern has been encountered in nested
                        //multiple .. in one pattern are illegal, and would lead to this code panicking
                        let mut rest_encountered = false;
                        let fields_len = fields.len();
                        let mut fields_iter = fields.into_iter();
                        let mut vars = Vec::new();

                        for (i, ident) in nested.into_iter().enumerate() {
                            match ident {
                                IdentPack::Rest => {
                                    if rest_encountered {
                                        //if .. already encountered, skip code will panic
                                        //as multiple .. is illegal, we quit early,
                                        //allowing the compiler to catch the error
                                        return Vec::new();
                                    };
                                    rest_encountered = true;

                                    //this skips the fields covered by ..
                                    //the `- 2` component does two things:
                                    //it converts fields_len to the index of the last elem,
                                    //and saves the desired field for processing
                                    //in the next loop iteration.
                                    fields_iter.nth(fields_len - i - 2);
                                }
                                _ => {
                                    let current_field = match fields_iter.next() {
                                        Some(field) => field,
                                        //mismatch in lhs to rhs
                                        //let compiler handle the error
                                        None => {
                                            return vars;
                                        }
                                    };
                                    vars.extend(ident.to_vars(current_field));
                                }
                            }
                        }
                        vars
                    }
                    _ => {
                        let mut vars = Vec::new();
                        let ident_dependencies = ExprType::Opaque(rhs.unify());
                        for ident in nested.into_iter() {
                            vars.extend(ident.to_vars(ident_dependencies.clone()))
                        };
                        vars
                    }
                }
            }
            IdentPack::Rest => {
                Vec::new()
            }
            IdentPack::None => {
                Vec::new()
            }
        }
    }

    fn from_pat(lhs: &Pat, rhs: &ExprType) -> IdentPack {
        match lhs {
            Pat::Box(_) => {
                //invalid lhs?
            }
            Pat::Ident(ident) => {
                let sub = rhs.get_hook_sub().cloned();
                return IdentPack::Atom(
                    Atom {
                        name: ident.ident.to_string(),
                        sub
                    }
                );
            }
            Pat::Lit(_) => {}
            Pat::Macro(_) => {}
            Pat::Or(_) => {}
            Pat::Path(path) => {
                let segments = &path.path.segments;
                if segments.len() == 1 {
                    let ident_name = segments.first().unwrap().ident.to_string();
                    let sub = rhs.get_hook_sub().cloned();
                    return IdentPack::Atom(
                        Atom {
                            name: ident_name,
                            sub
                        }
                    );
                }
            }
            Pat::Range(_) => {}
            Pat::Reference(_) => {
                //TODO: needs to be handled?
                todo!("reference")
            }
            Pat::Rest(_) => {
                return IdentPack::Rest;
            }
            Pat::Slice(slice) => {
                let mut idents = Vec::with_capacity(slice.elems.len());
    
                for elem in slice.elems.iter() {
                    idents.push(Self::from_pat(&elem, rhs));
                }
                return IdentPack::Nested(idents);
            }
            Pat::Struct(struct_pat) => {
                let mut idents = Vec::with_capacity(struct_pat.fields.len());
    
                for field in struct_pat.fields.iter() {
                    idents.push(IdentPack::from_pat(&field.pat, &rhs));
                }
                //note: while we return as nested, all idents are always given the
                //same dependencies when handling structs.
                return IdentPack::Nested(idents);
            }
            Pat::Tuple(tuple) => {
                return Self::from_pat_tuple(&tuple, rhs);
            }
            Pat::TupleStruct(tuple_struct) => {
                return Self::from_pat_tuple(&tuple_struct.pat, rhs);
            }
            Pat::Type(type_pat) => {
                return IdentPack::from_pat(&type_pat.pat, &rhs);
            }
            Pat::Verbatim(_) => {}
            Pat::Wild(_) => {}
            _ => {
                eprintln!("Warn: unknown Pat type.");
            }
        }
    
        IdentPack::None
    }

    fn from_pat_tuple(lhs: &syn::PatTuple, rhs: &ExprType) -> IdentPack {
        //TODO: handle per-element info
        let mut idents = Vec::with_capacity(lhs.elems.len());
    
        for (i, elem) in lhs.elems.iter().enumerate() {
            let mut rhs = rhs.clone();
            rhs.append_to_sub(i);
            idents.push(IdentPack::from_pat(elem, &rhs));
        }
        
        IdentPack::Nested(idents)
    }
}

fn expr_lhs_ident_pack(expr: &Expr) -> IdentPack {
    //note: we allow the compiler to handle invalid lhs
    match expr {
        Expr::Array(array) => {
            let mut idents = Vec::with_capacity(array.elems.len());

            for expr in array.elems.iter() {
                idents.push(expr_lhs_ident_pack(&expr));
            }
            return IdentPack::Nested(idents);
        }
        Expr::Assign(_) => {
            //no-op: assigns return ()
        }
        Expr::AssignOp(_) => {
            //ditto
        }
        Expr::Async(_) => {
            //illegal in lhs pos
        }
        Expr::Await(_) => {
            //illegal in lhs pos
        }
        Expr::Binary(_) => {
            //no-op: expr creates temporary
        }
        Expr::Block(_) => {
            //illegal in lhs pos
        }
        Expr::Box(_) => {
            //no-op: creates temporary
        }
        Expr::Break(_) => {
            //illegal in lhs pos
        }
        Expr::Call(_) => {
            //undetermined how to handle
            todo!()
        }
        Expr::Cast(_) => {
            //no-op: expr creates temporary
        }
        Expr::Closure(_) => {
            //illegal in lhs pos
        }
        Expr::Continue(_) => {
            //illegal in lhs pos
        }
        Expr::Field(field) => {
            return expr_lhs_ident_pack(&field.base);
        }
        Expr::ForLoop(_) => {
            //illegal in lhs pos
        }
        Expr::Group(group) => {
            return expr_lhs_ident_pack(&group.expr);
        }
        Expr::If(_) => {
            //illegal in lhs pos
        }
        Expr::Index(index) => {
            //TODO: index-specific stuff
            return expr_lhs_ident_pack(&index.expr);
        }
        Expr::Let(_) => {
            //illegal in lhs pos
        }
        Expr::Lit(_) => {
            //illegal in lhs pos
        }
        Expr::Loop(_) => {
            //illegal in lhs pos
        }
        Expr::Macro(_) => {}
        Expr::Match(_) => {
            //illegal in lhs pos
        }
        Expr::MethodCall(_) => {
            todo!()
        }
        Expr::Paren(paren) => {
            return expr_lhs_ident_pack(&paren.expr);
        }
        Expr::Path(path) => {
            let segments = &path.path.segments;
            if segments.len() == 1 {
                let ident_name = segments.first().unwrap().ident.to_string();
                return IdentPack::Atom(ident_name.into());
            }
        }
        Expr::Range(_) => {
            //illegal in lhs pos
        }
        Expr::Reference(_) => {
            //illegal in lhs pos
        }
        Expr::Repeat(_) => {
            //illegal in lhs pos
        }
        Expr::Return(_) => {
            //illegal in lhs pos
        }
        Expr::Struct(_) => {
            //illegal in lhs pos
        }
        Expr::Try(_) => {
            //illegal in lhs pos
        }
        Expr::TryBlock(_) => {
            //illegal in lhs pos
        }
        Expr::Tuple(tuple) => {
            let mut idents = Vec::with_capacity(tuple.elems.len());

            for elem in tuple.elems.iter() {
                idents.push(expr_lhs_ident_pack(&elem));
            }
            return IdentPack::Nested(idents);
        }
        Expr::Type(_) => {
            //nothing to do
        }
        Expr::Unary(_) => {
            //temp
        }
        Expr::Unsafe(_) => {
            //illegal in lhs pos
        }
        Expr::Verbatim(_) => {
            //nothing can be done
        }
        Expr::While(_) => {
            //illegal in lhs pos
        }
        Expr::Yield(_) => {
            //illegal in lhs pos
        }
        _ => {
            eprintln!("Warn: unknown Expr type.");
        }
    }
    
    IdentPack::None
}

#[proc_macro_error]
#[proc_macro_attribute]
pub fn component(metadata: TokenStream, input: TokenStream) -> TokenStream {
    let mut item = parse_macro_input!(input as Item);

    let item_fn = match item {
        Item::Fn(ref mut fun) => fun,
        _ => abort!(item, "component requires function input")
    };

    let mut function = Function::new();
    let mut param_scope = Scope::new();

    if item_fn.sig.inputs.len() > 64 {
        emit_error!(item_fn.sig.inputs, "more than 64 properties are unsupported");
    }

    if let Some(token) = &item_fn.sig.asyncness {
        abort!(token, "async components currently unsupported");
    };

    if let Some(token) = &item_fn.sig.unsafety {
        abort!(
            token,
            "unsafe components unsupported"; 
            note = "to write unsafe code, use an unsafe block"
        );
    };

    if let Some(token) = &item_fn.sig.variadic {
        abort!(token, "variadic components unsupported");
    };

    let hooks = if !metadata.is_empty() {
        let hooks = parse_macro_input!(metadata as Hooks);
        hooks.hooks
    }
    else {
        Punctuated::new()
    };

    let mut hook_name = Vec::with_capacity(hooks.len());
    let mut hook_type = Vec::with_capacity(hooks.len());
    let mut hook_get_fn_name = Vec::with_capacity(hooks.len());
    let mut hook_updates_name = Vec::with_capacity(hooks.len());

    for (i, hook) in hooks.iter().enumerate() {
        println!("Within hook loop");
        hook_name.push(&hook.name);
        hook_type.push(&hook.ty);
        hook_get_fn_name.push(format_ident!("get_{}_{}", hook.name, i));
        let hook_update_ident = format_ident!("__avalanche_internal_{}_update", i);
        hook_updates_name.push(hook_update_ident.clone());

        let var = Var {
            name: hook.name.to_string(),
            dependencies: {
                let mut deps = Dependencies::new();
                deps.insert(
                    hook.name.to_owned(),
                    DependencyInfo::Hook(HookInfo {
                        sub: Vec::new(),
                        hook_update_ident
                    }),
                );
                ExprType::Opaque(deps)
            }
        };
        param_scope.vars.push(var);
    };

    let mut param_ident = Vec::with_capacity(hooks.len());
    let mut param_type = Vec::with_capacity(hooks.len());
    let mut param_attributes = Vec::with_capacity(hooks.len());
    let mut flag = Vec::with_capacity(hooks.len());

    //process render code
    for (i, param) in item_fn.sig.inputs.iter().enumerate() {
        let update_bit_pattern = 2u64.pow(i as u32);
        match param {
            syn::FnArg::Receiver(rec) => {
                abort!(rec, "receiver not allowed");
            },
            syn::FnArg::Typed(param) => {
                let ident = if let Pat::Ident(ident) = &*param.pat {
                    let mut dependencies = Dependencies::default();
                    dependencies.insert(
                        ident.ident.to_owned(), 
                        DependencyInfo::Prop(update_bit_pattern)
                    );
                    let dependencies = ExprType::Opaque(dependencies);
                    param_scope.vars.push(Var {
                        name: ident.ident.to_string(),
                        dependencies
                    });
                    ident
                }
                else {
                    abort!(param.pat, "expected identifier");
                };

                param_ident.push(ident);
                param_type.push(&param.ty);
                param_attributes.push(&ident.attrs);
                flag.push(update_bit_pattern);
            }
        }
    }

    function.scopes.push(param_scope);

    function.block(&mut item_fn.block);

    let name = &item_fn.sig.ident;
    let builder_name = format_ident!("{}Builder", name);
    let state_name = format_ident!("{}State", name);

    let render_body = &item_fn.block;
    let visibility = &item_fn.vis;

    let component = quote!{
        #[derive(std::default::Default)]
        #visibility struct #builder_name {
            __internal_updates: u64,
            #(#param_ident: std::option::Option<#param_type>),*
        }

        impl #builder_name {
            fn new() -> Self {
                std::default::Default::default()
            }

            fn build(self) -> #name {
                #name {
                    __internal_updates: self.__internal_updates,
                    #(#param_ident: self.#param_ident.unwrap()),*
                }
            }

            #(
                #(#param_attributes)*
                fn #param_ident(mut self, val: #param_type, updated: bool) -> Self {
                    if updated {
                        self.__internal_updates |= #flag;
                    }
                    self.#param_ident = std::option::Option::Some(val);
                    self
                }
            )*
        }

        #[derive(std::default::Default)]
        #visibility struct #state_name {
            #( #hook_name: #hook_type ),*
        }

        #visibility struct #name {
            __internal_updates: u64,
            #(#param_ident: #param_type),*
        }

        impl avalanche::Component for #name {
            type Builder = #builder_name;

            fn init_state(&self) -> std::boxed::Box<dyn std::any::Any> {
                std::boxed::Box::new(<#state_name as std::default::Default>::default())
            }

            fn render(&self, context: avalanche::InternalContext) -> avalanche::View {
                #(
                    fn #hook_get_fn_name(state: &mut std::boxed::Box<std::any::Any>) -> &mut #hook_type {
                        let state = std::option::Option::unwrap(std::any::Any::downcast_mut::<#state_name>(&mut **state));
                        &mut state.#hook_name
                    };
                )*
                let state = std::option::Option::unwrap(std::any::Any::downcast_mut::<#state_name>(&mut **context.state));
                #( let (#hook_name, #hook_updates_name) = state.#hook_name.hook(context.vnode.clone(), #hook_get_fn_name); );*

                let _ = state;
                let _ = context;

                let #name { #(#param_ident,)* .. } = self;
                
                #render_body
            }

            fn updates(&self) -> u64 {
                self.__internal_updates
            }

        }
    };

    component.into()
}
