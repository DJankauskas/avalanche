use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};
use syn::{punctuated::Punctuated, Type};
use syn::{Expr, FieldValue, Ident, Result, Token};

pub(crate) struct ReactiveAssert {
    pub asserts: Punctuated<Assert, Token![;]>,
}

impl Parse for ReactiveAssert {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut asserts = Punctuated::new();
        while !input.is_empty() {
            let assert = input.parse::<Assert>()?;
            asserts.push_value(assert);

            //remove separator, except if the stream has no more to process
            if !input.is_empty() {
                asserts.push_punct(input.parse::<Token![;]>()?);
            }
        }

        Ok(ReactiveAssert { asserts })
    }
}

pub(crate) struct Assert {
    pub dependencies: Punctuated<Ident, Token![,]>,
    pub dependent: Ident,
    pub fat_arrow_token: Token![=>],
}

impl Parse for Assert {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut dependencies = Punctuated::new();

        while !input.lookahead1().peek(Token![=>]) {
            let ident = input.parse::<Ident>()?;
            dependencies.push_value(ident);

            //remove separator, except if the stream has no more to process
            if !input.lookahead1().peek(Token![=>]) {
                dependencies.push_punct(input.parse::<Token![,]>()?);
            }
        }

        let fat_arrow_token = input.parse()?;

        let dependent = input.parse::<Ident>()?;

        Ok(Assert {
            dependencies,
            dependent,
            fat_arrow_token,
        })
    }
}

impl ToTokens for Assert {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.dependencies.to_tokens(tokens);
        self.fat_arrow_token.to_tokens(tokens);
        self.dependent.to_tokens(tokens);
    }
}

pub(crate) struct ComponentInit {
    pub(crate) fields: Punctuated<FieldValue, Token![,]>,
}

impl Parse for ComponentInit {
    fn parse(input: ParseStream) -> Result<Self> {
        let fields = Punctuated::parse_terminated(input)?;
        let comp = ComponentInit { fields };

        Ok(comp)
    }
}

pub(crate) struct Hooks {
    pub hooks: Punctuated<Hook, Token![,]>,
}

impl Parse for Hooks {
    fn parse(input: ParseStream) -> Result<Self> {
        let hooks = Punctuated::parse_terminated(input)?;
        let ret = Hooks { hooks };

        Ok(ret)
    }
}

pub(crate) struct Hook {
    pub name: Ident,
    pub ty: Type,
    pub equal_token: Token![=],
}

impl Parse for Hook {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        let equal_token = input.parse()?;
        let ty = input.parse()?;
        let hook = Hook {
            name,
            ty,
            equal_token,
        };

        Ok(hook)
    }
}

pub(crate) struct Enclose {
    pub idents: Punctuated<Ident, Token![,]>,
    pub semicolon: Token![;],
    pub expr: Expr,
}

impl Parse for Enclose {
    fn parse(input: ParseStream) -> Result<Self> {

        let idents: Punctuated<Ident, Token![,]> = Punctuated::parse_separated_nonempty(input)?;
        let semicolon = input.parse()?;
        let expr = input.parse()?;

        let enclose = Enclose {
            idents,
            semicolon,
            expr
        };

        Ok(enclose)
    }
}
