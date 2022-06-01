mod macro_expr;
mod transform;

use proc_macro::TokenStream;

use proc_macro2::Span;
use proc_macro_error::{abort, emit_error, proc_macro_error};
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, GenericParam, Item, Lifetime, Pat, Path, PathArguments, ReturnType, Type,
    TypeParamBound,
};

use transform::{Dependencies, Function, Scope, Var};

#[proc_macro_error]
#[proc_macro_attribute]
pub fn component(_metadata: TokenStream, input: TokenStream) -> TokenStream {
    let mut item = parse_macro_input!(input as Item);

    let item_fn = match item {
        Item::Fn(ref mut fun) => fun,
        _ => abort!(item, "component requires function input"),
    };

    let mut function = Function::new();
    let mut param_scope = Scope::new();

    if item_fn.sig.inputs.len() > 64 {
        emit_error!(
            item_fn.sig.inputs,
            "more than 64 properties are unsupported"
        );
    }

    let return_type = match &item_fn.sig.output {
        ReturnType::Type(_, ty) => ty,
        ReturnType::Default => {
            abort!(
                item_fn.sig,
                "input function must have return type avalanche::View"
            );
        }
    };

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

    let mut lifetime = None;

    for param in item_fn.sig.generics.params.iter() {
        if let GenericParam::Lifetime(lifetime_def) = param {
            if let None = lifetime {
                lifetime = Some(&lifetime_def.lifetime);
            } else {
                abort!(lifetime_def, "only one lifetime parameter allowed in a component signature");
            };
        } else {
            abort!(
                param, 
                "only single lifetime parameter allowed"; 
                note = "currently generic components are unsupported"
            );
        }
    }

    let mut lifetime = lifetime.cloned();

    let inputs_len = item_fn.sig.inputs.len();

    let mut param_ident = Vec::with_capacity(inputs_len);
    let mut param_type = Vec::with_capacity(inputs_len);
    let mut param_attributes = Vec::with_capacity(inputs_len);
    let mut flag = Vec::with_capacity(inputs_len);

    // process render code
    for (i, param) in item_fn.sig.inputs.iter_mut().enumerate() {
        let update_bit_pattern = 2u64.pow(i as u32);
        match param {
            syn::FnArg::Receiver(rec) => {
                abort!(rec, "receiver not allowed");
            }
            syn::FnArg::Typed(param) => {
                add_explicit_lifetime(&mut lifetime, &mut param.ty);
                let ident = if let Pat::Ident(ident) = &*param.pat {
                    let mut dependencies = Dependencies::default();
                    dependencies.insert(ident.ident.to_owned());
                    param_scope.vars.push(Var {
                        name: ident.ident.to_string(),
                        dependencies: dependencies.into(),
                    });
                    ident
                } else {
                    abort!(param.pat, "expected identifier");
                };

                param_ident.push(ident);
                param_type.push(&param.ty);
                param_attributes.push(&ident.attrs);
                flag.push(update_bit_pattern);
            }
        }
    }

    // info for generating __last setter for last prop
    let last_param_ident = param_ident.last().into_iter();
    let last_param_type = param_type.last().into_iter();
    let last_param_attributes = param_attributes.last().into_iter();
    let last_flag = flag.last();

    function.scopes.push(param_scope);

    // dependencies unneeded: we only want to process the block
    let _ = function.block(&mut item_fn.block);

    let name = &item_fn.sig.ident;
    let builder_name = format_ident!("{}Builder", name);

    let render_body = &item_fn.block;
    let render_body_attributes = &item_fn.attrs;
    let visibility = &item_fn.vis;

    let component_default_impl = if inputs_len == 0 {
        lifetime = None;
        Some(quote! {
            impl ::std::default::Default for #name {
                fn default() -> Self {
                    Self {
                        __internal_updates: 0,
                        __key: None,
                        __location: (0, 0)
                    }
                }
            }
        })
    } else {
        None
    };

    let any_ref_impl = match &lifetime {
        Some(lifetime) => quote! { ::avalanche::impl_any_ref!(#name<#lifetime>); },
        None => quote! { ::avalanche::impl_any_ref!(#name); },
    };

    // a lifetime that always exists for the Component<'a> imp;
    let component_lifetime = lifetime
        .clone()
        .unwrap_or_else(|| Lifetime::new("'a", Span::call_site()));
    let lifetime_iter: Vec<_> = lifetime.as_ref().into_iter().collect();

    let component = quote! {
        #visibility struct #builder_name<#lifetime> {
            __internal_updates: ::std::primitive::u64,
            __key: ::std::option::Option<::std::string::String>,
            #(#param_ident: ::std::option::Option<#param_type>),*
        }

        impl<#lifetime> ::std::default::Default for #builder_name<#lifetime> {
            fn default() -> Self {
                Self {
                    __internal_updates: 0,
                    __key: ::std::option::Option::None,
                    #(#param_ident: ::std::option::Option::None),*
                }
            }
        }

        impl<#lifetime> #builder_name<#lifetime> where #( #param_type: ::std::clone::Clone ),* {
            fn new() -> Self {
                ::std::default::Default::default()
            }

            fn build(self, location: (::std::primitive::u32, ::std::primitive::u32)) -> #name<#lifetime> {
                #name::<#lifetime> {
                    __internal_updates: self.__internal_updates,
                    __key: self.__key,
                    __location: location,
                    #(#param_ident: ::std::option::Option::unwrap(self.#param_ident)),*
                }
            }

            fn key<T: ::std::string::ToString>(mut self, key: T, _updated: ::std::primitive::bool) -> Self {
                //TODO: should updated be used?
                self.__key = ::std::option::Option::Some(::std::string::ToString::to_string(&key));
                self
            }

            #(
                #(#param_attributes)*
                fn #param_ident(mut self, val: #param_type, updated: ::std::primitive::bool) -> Self {
                    if updated {
                        self.__internal_updates |= #flag;
                    }
                    self.#param_ident = ::std::option::Option::Some(val);
                    self
                }
            )*

            #(
                #(#last_param_attributes)*
                fn __last(mut self, val: #last_param_type, updated: ::std::primitive::bool) -> Self {
                    if updated {
                        self.__internal_updates |= #last_flag;
                    }
                    self.#last_param_ident = ::std::option::Option::Some(val);
                    self
                }
            )*
        }

        #[derive(::std::clone::Clone)]
        #visibility struct #name #(<#lifetime_iter>)* {
            __internal_updates: ::std::primitive::u64,
            __key: std::option::Option<::std::string::String>,
            __location: (::std::primitive::u32, ::std::primitive::u32),
            #(#param_ident: #param_type),*
        }

        #component_default_impl

        #any_ref_impl

        impl<#component_lifetime> ::avalanche::Component<#component_lifetime> for #name<#lifetime> {
            type Builder = #builder_name<#lifetime>;

            #( #render_body_attributes )*
            #[allow(clippy::eval_order_dependence, clippy::unit_arg)]
            fn render(self, mut __avalanche_render_context: ::avalanche::RenderContext, __avalanche_hook_context: ::avalanche::HookContext) -> #return_type {
                // let state = ::std::option::Option::unwrap(::std::any::Any::downcast_mut::<#state_name>(&mut **__avalanche_context.state));

                #(let #param_ident = ::avalanche::Tracked::new(self.#param_ident, (&self.__internal_updates & #flag) != 0);)*

                let mut __avalanche_internal_updated = false;

                #render_body
            }

            fn updated(&self) -> ::std::primitive::bool {
                self.__internal_updates != 0
            }

            fn key(&self) -> ::std::option::Option<String> {
                std::clone::Clone::clone(&self.__key)
            }

        }
    };

    // println!("{}", component);

    component.into()
}

fn add_explicit_lifetime(lifetime: &mut Option<Lifetime>, ty: &mut Type) {
    match ty {
        Type::Array(array) => {
            add_explicit_lifetime(lifetime, &mut *array.elem);
        }
        Type::BareFn(_) => {}
        Type::Group(group) => add_explicit_lifetime(lifetime, &mut *group.elem),
        Type::ImplTrait(impl_trait) => {
            for bound in impl_trait.bounds.iter_mut() {
                add_explicit_lifetime_type_bound(lifetime, bound);
            }
        }
        Type::Infer(_) => {}
        Type::Macro(_) => {}
        Type::Never(_) => {}
        Type::Paren(paren) => add_explicit_lifetime(lifetime, &mut *paren.elem),
        Type::Path(path) => {
            if let Some(qself) = &mut path.qself {
                add_explicit_lifetime(lifetime, &mut qself.ty);
            };
            add_explicit_lifetime_path(lifetime, &mut path.path);
        }
        Type::Ptr(ptr) => add_explicit_lifetime(lifetime, &mut *ptr.elem),
        Type::Reference(reference) => {
            // TODO: handle inference
            if let Some(l) = &reference.lifetime {
                if l.ident.to_string() == "_" {
                    reference.lifetime = None;
                }
            }
            reference.lifetime.get_or_insert_with(|| {
                lifetime
                    .get_or_insert_with(|| Lifetime::new("'a", Span::call_site()))
                    .clone()
            });
            add_explicit_lifetime(lifetime, &mut reference.elem);
        }
        Type::Slice(slice) => add_explicit_lifetime(lifetime, &mut *slice.elem),
        Type::TraitObject(trait_object) => {
            for bound in trait_object.bounds.iter_mut() {
                add_explicit_lifetime_type_bound(lifetime, bound);
            }
        }
        Type::Tuple(tuple) => {
            for elem in tuple.elems.iter_mut() {
                add_explicit_lifetime(lifetime, &mut *elem);
            }
        }
        Type::Verbatim(_token_stream) => {}
        _ => {}
    };
}

fn add_explicit_lifetime_path(lifetime: &mut Option<Lifetime>, path: &mut Path) {
    for segment in path.segments.iter_mut() {
        if let PathArguments::AngleBracketed(generics) = &mut segment.arguments {
            for arg in generics.args.iter_mut() {
                match arg {
                    syn::GenericArgument::Lifetime(_) => {}
                    syn::GenericArgument::Type(ty) => add_explicit_lifetime(lifetime, ty),
                    syn::GenericArgument::Binding(binding) => {
                        add_explicit_lifetime(lifetime, &mut binding.ty)
                    }
                    syn::GenericArgument::Constraint(constraint) => {
                        for bound in constraint.bounds.iter_mut() {
                            add_explicit_lifetime_type_bound(lifetime, bound);
                        }
                    }
                    syn::GenericArgument::Const(_) => {}
                }
            }
        }
    }
}

fn add_explicit_lifetime_type_bound(
    lifetime: &mut Option<Lifetime>,
    type_bound: &mut TypeParamBound,
) {
    if let TypeParamBound::Trait(trait_bound) = type_bound {
        add_explicit_lifetime_path(lifetime, &mut trait_bound.path)
    }
}
