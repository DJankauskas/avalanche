use proc_macro2::{Span, TokenStream};
use proc_macro_crate::{crate_name, FoundCrate};
use quote::{quote, ToTokens};
use syn::Ident;

thread_local!{
    static AVALANCHE_PATH: AvalanchePath = init_avalanche_path();
}

fn init_avalanche_path() -> AvalanchePath {
    let found_crate = crate_name("avalanche").expect("crate avalanche found in `Cargo.toml`");
    match found_crate {
        FoundCrate::Itself => AvalanchePath::Itself,
        FoundCrate::Name(name) => AvalanchePath::Named(name),
    }
}

/// Returns a type that can produce a token stream for the path of the
/// avalanche crate. This can be either `crate` or `::name`, where `name`
/// is the name the user has given the avalanche crate.
pub(crate) fn get_avalanche_path() -> impl ToTokens {
    AVALANCHE_PATH.with(|path| path.clone())
}

/// A representation of the avalanche crate's path.
#[derive(Clone)]
enum AvalanchePath {
    Itself,
    Named(String),
}

impl ToTokens for AvalanchePath {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            AvalanchePath::Itself => tokens.extend(quote! { crate }),
            AvalanchePath::Named(name) => {
                let ident = Ident::new(name, Span::call_site());
                tokens.extend(quote! { :: #ident })
            }
        }
    }
}
