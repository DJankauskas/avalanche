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

pub(crate) struct EncloseBody {
    pub idents: Punctuated<Ident, Token![,]>,
    pub semicolon: Token![;],
    pub expr: Expr,
}

impl Parse for EncloseBody {
    fn parse(input: ParseStream) -> Result<Self> {
        let idents: Punctuated<Ident, Token![,]> = Punctuated::parse_separated_nonempty(input)?;
        let semicolon = input.parse()?;
        let expr = input.parse()?;

        let enclose = EncloseBody {
            idents,
            semicolon,
            expr,
        };

        Ok(enclose)
    }
}

/// A type representing a comma-delineated series of expressions.
pub(crate) struct ExprList {
    pub exprs: Punctuated<Expr, Token![,]>,
}

impl Parse for ExprList {
    fn parse(input: ParseStream) -> Result<Self> {
        let exprs = Punctuated::parse_terminated(input)?;

        let dbg = ExprList { exprs };

        Ok(dbg)
    }
}

pub(crate) struct MatchesBody {
    pub expr: Expr,
    pub comma_token: Token![,],
    pub pats: Punctuated<syn::Pat, Token![|]>,
    pub if_token: Option<Token![if]>,
    pub if_expr: Option<Expr>,
    pub trailing_comma_token: Option<Token![,]>,
}

impl Parse for MatchesBody {
    fn parse(input: ParseStream) -> Result<Self> {
        let expr = input.parse()?;
        let comma_token = input.parse()?;
        let pats = Punctuated::parse_separated_nonempty(input)?;
        let if_token = input.parse().ok();
        let if_expr = input.parse().ok();
        let trailing_comma_token = input.parse().ok();

        let matches = MatchesBody {
            expr,
            comma_token,
            pats,
            if_token,
            if_expr,
            trailing_comma_token,
        };

        Ok(matches)
    }
}

/// This is an incomplete representation of the body of
/// a [`vec!`] macro, consisting of its expressions.
pub(crate) enum VecBody {
    Repeat(VecRepeat),
    Literal(Punctuated<Expr, Token![,]>),
}

pub(crate) struct VecRepeat {
    pub expr: Expr,
    pub semicolon_token: Token![;],
    pub n_expr: Expr,
}

impl Parse for VecBody {
    fn parse(input: ParseStream) -> Result<Self> {
        let expr = input.parse()?;
        let semicolon_token = match input.parse() {
            Ok(token) => token,
            Err(_) => {
                let mut exprs = Punctuated::new();
                exprs.push_value(expr);
                if let Ok(comma_token) = input.parse() {
                    exprs.push_punct(comma_token);
                    exprs.extend(Punctuated::parse_terminated(input)?.into_pairs());
                };

                let vec = VecBody::Literal(exprs);

                return Ok(vec);
            }
        };
        let n_expr = input.parse()?;

        let vec = VecBody::Repeat(VecRepeat {
            expr,
            semicolon_token,
            n_expr,
        });

        Ok(vec)
    }
}

pub(crate) struct Try {
    pub expr: Expr,
    pub trailing_comma_token: Option<Token![,]>,
}

impl Parse for Try {
    fn parse(input: ParseStream) -> Result<Self> {
        let expr = input.parse()?;
        let trailing_comma_token = input.parse().ok();

        let try_ = Try {
            expr,
            trailing_comma_token,
        };

        Ok(try_)
    }
}
