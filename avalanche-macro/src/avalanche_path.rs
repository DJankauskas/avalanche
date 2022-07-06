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
        FoundCrate::Itself => {
            // Doc tests expect avalanche items to be in avalanche, not in crate, even if they're within the avalanche crate
            // Note that this solution is unstable but it's the best we can easily do at the moment
            // TODO: more robust solution?
            if std::env::var("UNSTABLE_RUSTDOC_TEST_LINE").is_ok() || std::env::var("UNSTABLE_RUSTDOC_TEST_PATH").is_ok() {
                AvalanchePath::Name("avalanche".to_string())
            } else {
                AvalanchePath::Itself
            }
        }
        FoundCrate::Name(name) => AvalanchePath::Name(name),
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
    Name(String),
}

impl ToTokens for AvalanchePath {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            AvalanchePath::Itself => tokens.extend(quote! { crate }),
            AvalanchePath::Name(name) => {
                let ident = Ident::new(name, Span::call_site());
                tokens.extend(quote! { :: #ident })
            }
        }
    }
}
