mod avalanche_path;
mod macro_expr;
mod transform;

use avalanche_path::get_avalanche_path;
use macro_expr::DefaultAttr;
use proc_macro::TokenStream;

use proc_macro2::{Ident, Span};
use proc_macro_error::{abort, emit_error, proc_macro_error};
use quote::{format_ident, quote};
use syn::{
    parse2, parse_macro_input, punctuated::Punctuated, Attribute, Expr, GenericParam, Item,
    Lifetime, Pat, Path, PathArguments, ReturnType, Token, Type, TypeParamBound, parse_quote,
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

    let mut generic_params = Vec::new();
    let mut generic_idents = Vec::new();
    let mut generic_types = Vec::new();

    for param in item_fn.sig.generics.params.iter() {
        match param {
            GenericParam::Lifetime(lifetime_def) => {
                if lifetime.is_none() {
                    lifetime = Some(&lifetime_def.lifetime);
                } else {
                    abort!(
                        lifetime_def,
                        "only one lifetime parameter allowed in a component signature"
                    );
                };
            }
            GenericParam::Type(ty) => {
                generic_params.push(param);
                generic_idents.push(&ty.ident);
                generic_types.push(&ty.ident);
            }
            GenericParam::Const(c) => {
                generic_params.push(param);
                generic_idents.push(&c.ident);
            }
        }
    }
    let generic_params = Punctuated::<_, Token![,]>::from_iter(generic_params);
    // Allow multiple interpolations into macro.
    let generic_params = &generic_params;

    let generic_idents = Punctuated::<_, Token![,]>::from_iter(generic_idents);
    // Allow multiple interpolations into macro.
    let generic_idents = &generic_idents;

    let generic_types = Punctuated::<_, Token![,]>::from_iter(generic_types);
    // Allow multiple interpolations into macro.
    let generic_types = &generic_types;

    let inputs_len = item_fn.sig.inputs.len();

    let mut param_ident = Vec::with_capacity(inputs_len);
    let mut param_type = Vec::with_capacity(inputs_len);
    // The types that go within the type that implements `Component`
    // rather than in the builder. Useful for the `#[optional]` attribute.
    let mut impl_param_type = Vec::with_capacity(inputs_len);
    let mut param_docs = Vec::with_capacity(inputs_len);
    let mut index = Vec::with_capacity(inputs_len);
    let mut impl_init_expr = Vec::with_capacity(inputs_len);

    // a lifetime that always exists for the Component<'a> impl
    let component_lifetime = lifetime
        .cloned()
        .unwrap_or_else(|| Lifetime::new("'a", Span::call_site()));

    // process render code
    for (i, param) in item_fn.sig.inputs.iter_mut().enumerate() {
        match param {
            syn::FnArg::Receiver(rec) => {
                abort!(rec, "receiver not allowed");
            }
            syn::FnArg::Typed(param) => {
                // Ensure users don't pass impl Trait or _
                // as component type parameters
                match &*param.ty {
                    Type::ImplTrait(impl_trait) => {
                        abort!(
                            impl_trait,
                            "impl trait not supported";
                            note = "consider using generics instead"
                        );
                    }
                    Type::Infer(infer) => {
                        abort!(infer, "explicit types required for component properties");
                    }
                    _ => {}
                };
                add_explicit_lifetime(&component_lifetime, &mut param.ty);
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

                let prop_attributes = process_prop_attributes(&param.attrs);
                let docs = prop_attributes.docs;
                param_docs.push(docs);
                
                let impl_ty = if let Some(PropInit::Optional) = &prop_attributes.init {
                    let ty = &param.ty;
                    parse_quote!{ ::std::option::Option<#ty> }
                } else {
                    (*param.ty).clone()
                };
                impl_param_type.push(impl_ty);
                
                let impl_init = match prop_attributes.init {
                    Some(PropInit::Optional) => quote!{self.#ident},
                    Some(PropInit::Default) => quote!{
                        ::std::option::Option::unwrap_or_default(self.#ident)
                    },
                    Some(PropInit::WithDefault(default)) => quote!{
                        ::std::option::Option::unwrap_or_else(self.#ident, || #default)
                    },
                    None => quote!{
                        ::std::option::Option::unwrap(self.#ident)
                    },
                };
                
                impl_init_expr.push(impl_init);

                param_ident.push(ident);
                param_type.push(&param.ty);
                index.push(i);
            }
        }
    }

    // info for generating __last setter for last prop
    let last_param_ident = param_ident.last().into_iter();
    let last_param_type = param_type.last().into_iter();
    let last_param_docs = param_docs.last().into_iter();
    let last_index = index.last();

    function.scopes.push(param_scope);

    // dependencies unneeded: we only want to process the block
    let _ = function.block(&mut item_fn.block);

    let builder_name = Ident::new(&item_fn.sig.ident.to_string(), item_fn.sig.ident.span());
    let name = format_ident!("{}Impl", builder_name, span = Span::call_site());

    let render_body = &item_fn.block;
    let render_body_attributes = &item_fn.attrs;
    let visibility = &item_fn.vis;
    let where_clause = &item_fn.sig.generics.where_clause;

    let component_default_impl = if inputs_len == 0 {
        Some(quote! {
            impl<#component_lifetime, #generic_params> #avalanche_path::DefaultComponent<#component_lifetime> for #builder_name<#component_lifetime, #generic_idents> #where_clause {
                type Impl = #name<#component_lifetime, #generic_idents>;

                fn new() -> Self::Impl {
                    Self::Impl {
                        __internal_gens: [],
                        __key: ::std::option::Option::None,
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
        #visibility struct #builder_name<#component_lifetime, #generic_params> #where_clause {
            __internal_gens: [#avalanche_path::tracked::Gen<#component_lifetime>; #inputs_len],
            __key: ::std::option::Option<::std::string::String>,
            #(#param_ident: ::std::option::Option<#param_type>),*
        }

        impl<#component_lifetime, #generic_params> ::std::default::Default for #builder_name<#component_lifetime, #generic_idents> #where_clause {
            fn default() -> Self {
                Self {
                    __internal_gens: [#avalanche_path::tracked::Gen::escape_hatch_new(false); #inputs_len],
                    __key: ::std::option::Option::None,
                    #(#param_ident: ::std::option::Option::None),*
                }
            }
        }

        impl<#component_lifetime, #generic_params> #builder_name<#component_lifetime, #generic_idents> #where_clause {
            pub fn new() -> Self {
                ::std::default::Default::default()
            }

            pub fn build(self, location: (::std::primitive::u32, ::std::primitive::u32)) -> #name<#component_lifetime, #generic_idents> {
                #name {
                    __internal_gens: self.__internal_gens,
                    __key: self.__key,
                    __location: location,
                    __phantom: ::std::marker::PhantomData,
                    #(#param_ident: #impl_init_expr),*
                }
            }

            pub fn key<__T: ::std::string::ToString>(mut self, key: __T, _gen: #avalanche_path::tracked::Gen<#component_lifetime>) -> Self {
                self.__key = ::std::option::Option::Some(::std::string::ToString::to_string(&key));
                self
            }

            #(
                #(#param_docs)*
                pub fn #param_ident(mut self, val: #param_type, gen: #avalanche_path::tracked::Gen<#component_lifetime>) -> Self {
                    self.__internal_gens[#index] = gen;
                    self.#param_ident = ::std::option::Option::Some(val);
                    self
                }
            )*

            #(
                #(#last_param_docs)*
                pub fn __last(mut self, val: #last_param_type, gen: #avalanche_path::tracked::Gen<#component_lifetime>) -> Self {
                    self.__internal_gens[#last_index] = gen;
                    self.#last_param_ident = ::std::option::Option::Some(val);
                    self
                }
            )*
        }

        #[derive(::std::clone::Clone)]
        #visibility struct #name<#component_lifetime, #generic_params> #where_clause {
            __internal_gens: [#avalanche_path::tracked::Gen<#component_lifetime>; #inputs_len],
            __key: std::option::Option<::std::string::String>,
            __location: (::std::primitive::u32, ::std::primitive::u32),
            __phantom: ::std::marker::PhantomData<&#component_lifetime (#generic_types)>,
            #(#param_ident: #impl_param_type),*
        }

        #component_default_impl

        impl<#component_lifetime, #generic_params> #avalanche_path::Component<#component_lifetime> for #name<#component_lifetime, #generic_idents> #where_clause {
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
                ::std::clone::Clone::clone(&self.__key)
            }
            
            fn location(&self) -> ::std::option::Option<(::std::primitive::u32, ::std::primitive::u32)> {
                ::std::option::Option::Some(self.__location)
            }
        }
    };
    
    // eprintln!("{}", component);

    component.into()
}

fn add_explicit_lifetime(lifetime: &Lifetime, ty: &mut Type) {
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
            reference.lifetime.get_or_insert_with(|| lifetime.clone());
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

fn add_explicit_lifetime_path(lifetime: &Lifetime, path: &mut Path) {
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

fn add_explicit_lifetime_type_bound(lifetime: &Lifetime, type_bound: &mut TypeParamBound) {
    if let TypeParamBound::Trait(trait_bound) = type_bound {
        add_explicit_lifetime_path(lifetime, &mut trait_bound.path)
    }
}

/// How a prop not being provider is handled
enum PropInit {
    /// `#[default]`
    /// Use prop type's `Default` implementation if it isn't
    /// provided explicitly
    Default,
    /// `#[default(with_init)]`
    /// Use the given TokenStream as an initialization expression
    /// if the caller doesn't provide one
    WithDefault(Expr),
    /// `#[option]`
    /// Allow the caller not to specify a prop, presenting it to the component
    /// as `None`.
    Optional,
}

/// Contains processed prop attribute data on docs and initialization.
struct PropAttributes {
    /// What kind of initialization is used for a prop.
    init: Option<PropInit>,
    /// `#[doc]` attributes given to a prop.
    docs: Vec<Attribute>,
}

fn process_prop_attributes(attributes: &Vec<Attribute>) -> PropAttributes {
    let mut docs = Vec::new();
    let mut init = None;
    for attr in attributes {
        let Some(ident) = attr.path.get_ident() else {
            abort!(attr.path, "attribute not recognized");
        };

        match &*ident.to_string() {
            "doc" => docs.push(attr.clone()),
            "default" => {
                if init.is_some() {
                    emit_error!(
                        attr,
                        "only one of `default` or `optional` is allowed on a property"
                    );
                }
                init = if attr.tokens.is_empty() {
                    Some(PropInit::Default)
                } else {
                    let expr = match parse2::<DefaultAttr>(attr.tokens.clone()) {
                        Ok(default_attr) => default_attr.expr,
                        Err(err) => abort!(err.span(), err.to_string()),
                    };
                    Some(PropInit::WithDefault(expr))
                };
            }
            "optional" => {
                if init.is_some() {
                    emit_error!(
                        attr,
                        "only one of `default` or `optional` is allowed on a property"
                    );
                }
                init = Some(PropInit::Optional)
            }
            _ => abort!(attr.path, "attribute not recognized"),
        }
    }

    PropAttributes { init, docs }
}
