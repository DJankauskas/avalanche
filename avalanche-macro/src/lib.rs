mod transform;
mod macro_expr;

use proc_macro::TokenStream;

use proc_macro_error::{abort, emit_error, proc_macro_error};
use quote::{format_ident, quote};
use syn::{parse_macro_input, punctuated::Punctuated, Item, Pat, ReturnType};

use transform::{Dependencies, DependencyInfo, ExprType, Function, HookInfo, Scope, Var};
use macro_expr::Hooks;

#[proc_macro_error]
#[proc_macro_attribute]
pub fn component(metadata: TokenStream, input: TokenStream) -> TokenStream {
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

    let hooks = if !metadata.is_empty() {
        let hooks = parse_macro_input!(metadata as Hooks);
        hooks.hooks
    } else {
        Punctuated::new()
    };

    let mut hook_name = Vec::with_capacity(hooks.len());
    let mut hook_type = Vec::with_capacity(hooks.len());
    let mut hook_get_fn_name = Vec::with_capacity(hooks.len());
    let mut hook_updates_name = Vec::with_capacity(hooks.len());

    for (i, hook) in hooks.iter().enumerate() {
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
                        hook_update_ident,
                    }),
                );
                ExprType::Opaque(deps)
            },
        };
        param_scope.vars.push(var);
    }

    let inputs_len = item_fn.sig.inputs.len();

    let mut param_ident = Vec::with_capacity(inputs_len);
    let mut param_type = Vec::with_capacity(inputs_len);
    let mut param_attributes = Vec::with_capacity(inputs_len);
    let mut flag = Vec::with_capacity(inputs_len);

    //process render code
    for (i, param) in item_fn.sig.inputs.iter().enumerate() {
        let update_bit_pattern = 2u64.pow(i as u32);
        match param {
            syn::FnArg::Receiver(rec) => {
                abort!(rec, "receiver not allowed");
            }
            syn::FnArg::Typed(param) => {
                let ident = if let Pat::Ident(ident) = &*param.pat {
                    let mut dependencies = Dependencies::default();
                    dependencies.insert(
                        ident.ident.to_owned(),
                        DependencyInfo::Prop(update_bit_pattern),
                    );
                    let dependencies = ExprType::Opaque(dependencies);
                    param_scope.vars.push(Var {
                        name: ident.ident.to_string(),
                        dependencies,
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

    function.scopes.push(param_scope);

    //dependencies unneeded: we only want to process the block
    let _ = function.block(&mut item_fn.block);

    let name = &item_fn.sig.ident;
    let builder_name = format_ident!("{}Builder", name);
    let state_name = format_ident!("{}State", name);

    let render_body = &item_fn.block;
    let render_body_attributes = &item_fn.attrs;
    let visibility = &item_fn.vis;

    let component_default_impl = if inputs_len == 0 {
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

    let component = quote! {
        #[derive(::std::default::Default)]
        #visibility struct #builder_name {
            __internal_updates: ::std::primitive::u64,
            __key: ::std::option::Option<::std::string::String>,
            #(#param_ident: ::std::option::Option<#param_type>),*
        }

        impl #builder_name {
            fn new() -> Self {
                ::std::default::Default::default()
            }

            fn build(self, location: (::std::primitive::u32, ::std::primitive::u32)) -> #name {
                #name {
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
        }

        #[derive(::std::default::Default)]
        #visibility struct #state_name {
            #( #hook_name: #hook_type ),*
        }

        #visibility struct #name {
            __internal_updates: ::std::primitive::u64,
            __key: std::option::Option<::std::string::String>,
            __location: (::std::primitive::u32, ::std::primitive::u32),
            #(#param_ident: #param_type),*
        }

        #component_default_impl

        impl avalanche::Component for #name {
            type Builder = #builder_name;

            fn init_state(&self) -> ::std::boxed::Box<dyn std::any::Any> {
                std::boxed::Box::new(<#state_name as ::std::default::Default>::default())
            }

            #( #render_body_attributes )*
            fn render(&self, context: avalanche::InternalContext) -> #return_type {
                #(
                    fn #hook_get_fn_name(state: &mut ::std::boxed::Box<::std::any::Any>) -> &mut #hook_type {
                        let state = ::std::option::Option::unwrap(::std::any::Any::downcast_mut::<#state_name>(&mut **state));
                        &mut state.#hook_name
                    };
                )*
                let state = ::std::option::Option::unwrap(::std::any::Any::downcast_mut::<#state_name>(&mut **context.state));
                #( let (#hook_name, #hook_updates_name) = <#hook_type>::hook(
                    &mut state.#hook_name,
                    ::std::clone::Clone::clone(&context.component_pos),
                    context.scheduler,
                    #hook_get_fn_name
                ); );*

                let _ = state;
                let _ = context;

                let #name { #(#param_ident,)* .. } = self;

                #render_body
            }

            fn updated(&self) -> ::std::primitive::bool {
                self.__internal_updates != 0
            }

            fn key(&self) -> ::std::option::Option<&str> {
                self.__key.as_deref()
            }

        }
    };

    // println!("{}", component);

    component.into()
}
