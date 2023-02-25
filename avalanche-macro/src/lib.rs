mod avalanche_path;
mod macro_expr;
mod transform;

use avalanche_path::get_avalanche_path;
use proc_macro::TokenStream;

use proc_macro2::{Ident, Span};
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

    let avalanche_path = get_avalanche_path();

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
            if lifetime.is_none() {
                lifetime = Some(&lifetime_def.lifetime);
            } else {
                abort!(
                    lifetime_def,
                    "only one lifetime parameter allowed in a component signature"
                );
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
    let mut index = Vec::with_capacity(inputs_len);

    // process render code
    for (i, param) in item_fn.sig.inputs.iter_mut().enumerate() {
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
                        span: ident.ident.span(),
                    });
                    ident
                } else {
                    abort!(param.pat, "expected identifier");
                };

                param_ident.push(ident);
                param_type.push(&param.ty);
                param_attributes.push(&ident.attrs);
                index.push(i);
            }
        }
    }

    // info for generating __last setter for last prop
    let last_param_ident = param_ident.last().into_iter();
    let last_param_type = param_type.last().into_iter();
    let last_param_attributes = param_attributes.last().into_iter();
    let last_index = index.last();

    function.scopes.push(param_scope);

    // dependencies unneeded: we only want to process the block
    let _ = function.block(&mut item_fn.block);

    let builder_name = Ident::new(&item_fn.sig.ident.to_string(), item_fn.sig.ident.span());
    let name = format_ident!("{}Impl", builder_name, span = Span::call_site());

    let render_body = &item_fn.block;
    let render_body_attributes = &item_fn.attrs;
    let visibility = &item_fn.vis;

    // a lifetime that always exists for the Component<'a> impl
    let component_lifetime = lifetime.unwrap_or_else(|| Lifetime::new("'a", Span::call_site()));

    let component_default_impl = if inputs_len == 0 {
        Some(quote! {
            impl<#component_lifetime> #avalanche_path::DefaultComponent<#component_lifetime> for #builder_name<#component_lifetime> {
                type Impl = #name<#component_lifetime>;

                fn new() -> Self::Impl {
                    Self::Impl {
                        __internal_gens: [],
                        __key: None,
                        __location: (0, 0),
                        __phantom: ::std::marker::PhantomData,
                    }
                }
            }
        })
    } else {
        None
    };

    let component = quote! {
        #visibility struct #builder_name<#component_lifetime> {
            __internal_gens: [#avalanche_path::tracked::Gen<#component_lifetime>; #inputs_len],
            __key: ::std::option::Option<::std::string::String>,
            #(#param_ident: ::std::option::Option<#param_type>),*
        }

        impl<#component_lifetime> #builder_name<#component_lifetime> where #( #param_type: ::std::clone::Clone ),* {
            pub fn new() -> Self {
                Self {
                    __internal_gens: [#avalanche_path::tracked::Gen::escape_hatch_new(false); #inputs_len],
                    __key: ::std::option::Option::None,
                    #(#param_ident: ::std::option::Option::None),*
                }
            }

            pub fn build(self, location: (::std::primitive::u32, ::std::primitive::u32)) -> #name<#component_lifetime> {
                #name::<#component_lifetime> {
                    __internal_gens: self.__internal_gens,
                    __key: self.__key,
                    __location: location,
                    __phantom: ::std::marker::PhantomData,
                    #(#param_ident: ::std::option::Option::unwrap(self.#param_ident)),*
                }
            }

            pub fn key<T: ::std::string::ToString>(mut self, key: T, _gen: #avalanche_path::tracked::Gen<#component_lifetime>) -> Self {
                self.__key = ::std::option::Option::Some(::std::string::ToString::to_string(&key));
                self
            }

            #(
                #(#param_attributes)*
                pub fn #param_ident(mut self, val: #param_type, gen: #avalanche_path::tracked::Gen<#component_lifetime>) -> Self {
                    self.__internal_gens[#index] = gen;
                    self.#param_ident = ::std::option::Option::Some(val);
                    self
                }
            )*

            #(
                #(#last_param_attributes)*
                pub fn __last(mut self, val: #last_param_type, gen: #avalanche_path::tracked::Gen<#component_lifetime>) -> Self {
                    self.__internal_gens[#last_index] = gen;
                    self.#last_param_ident = ::std::option::Option::Some(val);
                    self
                }
            )*
        }

        #[derive(::std::clone::Clone)]
        #visibility struct #name<#component_lifetime> {
            __internal_gens: [#avalanche_path::tracked::Gen<#component_lifetime>; #inputs_len],
            __key: std::option::Option<::std::string::String>,
            __location: (::std::primitive::u32, ::std::primitive::u32),
            __phantom: ::std::marker::PhantomData<&#component_lifetime ()>,
            #(#param_ident: #param_type),*
        }

        #component_default_impl

        impl<#component_lifetime> #avalanche_path::Component<#component_lifetime> for #name<#component_lifetime> {
            #( #render_body_attributes )*
            #[allow(clippy::eval_order_dependence, clippy::unit_arg)]
            fn render(self, mut __avalanche_render_context: #avalanche_path::hooks::RenderContext, __avalanche_hook_context: #avalanche_path::hooks::HookContext) -> #return_type {
                #(let #param_ident = #avalanche_path::tracked::Tracked::new(self.#param_ident, self.__internal_gens[#index]);)*

                let mut __avalanche_internal_gen = #avalanche_path::tracked::Gen::escape_hatch_new(false);

                #render_body
            }

            fn updated(&self, curr_gen: #avalanche_path::tracked::Gen) -> ::std::primitive::bool {
                for gen in self.__internal_gens {
                    if gen >= curr_gen {
                        return true;
                    }
                }
                return false;
            }

            fn key(&self) -> ::std::option::Option<String> {
                std::clone::Clone::clone(&self.__key)
            }
        }
    };

    // eprintln!("{}", component);

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
                if l.ident == "_" {
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
