use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::{discouraged::Speculative, Parse, ParseStream};
use syn::{punctuated::Punctuated};
use syn::{Expr, Ident, Result, Token};

pub(crate) struct EncloseBody {
    pub idents: Punctuated<Ident, Token![,]>,
    pub semicolon: Token![;],
    pub expr: Expr,
}

impl Parse for EncloseBody {
    fn parse(input: ParseStream) -> Result<Self> {
        let idents: Punctuated<Ident, Token![,]> = Punctuated::parse_separated_nonempty(input).unwrap_or_default();
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

impl ToTokens for EncloseBody {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.idents.to_tokens(tokens);
        self.semicolon.to_tokens(tokens);
        self.expr.to_tokens(tokens);
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

impl ToTokens for ExprList {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.exprs.to_tokens(tokens);
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

impl ToTokens for MatchesBody {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.expr.to_tokens(tokens);
        self.comma_token.to_tokens(tokens);
        self.pats.to_tokens(tokens);
        self.if_token.to_tokens(tokens);
        self.if_expr.to_tokens(tokens);
        self.trailing_comma_token.to_tokens(tokens);
    }
}

/// This is an incomplete representation of the body of
/// a [`vec!`] macro, consisting of its expressions.
pub(crate) enum VecBody {
    Repeat(Box<VecRepeat>),
    Literal(Punctuated<Expr, Token![,]>),
}

pub(crate) struct VecRepeat {
    pub expr: Expr,
    pub semicolon_token: Token![;],
    pub n_expr: Expr,
}

impl ToTokens for VecRepeat {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.expr.to_tokens(tokens);
        self.semicolon_token.to_tokens(tokens);
        self.n_expr.to_tokens(tokens);
    }
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

        let vec = VecBody::Repeat(Box::new(VecRepeat {
            expr,
            semicolon_token,
            n_expr,
        }));

        Ok(vec)
    }
}

impl ToTokens for VecBody {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            VecBody::Repeat(repeat) => repeat.to_tokens(tokens),
            VecBody::Literal(literal) => literal.to_tokens(tokens),
        }
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

impl ToTokens for Try {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.expr.to_tokens(tokens);
        self.trailing_comma_token.to_tokens(tokens);
    }
}

pub(crate) struct ComponentFieldValue {
    pub name: Ident,
    pub colon_token: Token![:],
    pub value: Expr,
}

impl Parse for ComponentFieldValue {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(ComponentFieldValue {
            name: input.parse()?,
            colon_token: input.parse()?,
            value: input.parse()?,
        })
    }
}

impl ToTokens for ComponentFieldValue {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.name.to_tokens(tokens);
        self.colon_token.to_tokens(tokens);
        self.value.to_tokens(tokens);
    }
}

pub(crate) struct ComponentBuilder {
    pub named_init: Punctuated<ComponentFieldValue, Token![,]>,
    pub trailing_init: Option<Expr>,
    _trailing_comma_token: Option<Token![,]>,
}

impl Parse for ComponentBuilder {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut named_init = Punctuated::new();
        while !input.is_empty() {
            let fork = input.fork();
            let value = fork.parse::<ComponentFieldValue>();
            if let Ok(value) = value {
                input.advance_to(&fork);
                named_init.push(value);
                match input.parse() {
                    Ok(comma_token) => named_init.push_punct(comma_token),
                    Err(_) => {
                        if input.is_empty() {
                            return Ok(ComponentBuilder {
                                named_init,
                                trailing_init: None,
                                _trailing_comma_token: None,
                            });
                        } else {
                            return Err(input.error("expected , or )"));
                        }
                    }
                }
            } else {
                break;
            }
        }
        let trailing_init = input.parse().ok();
        let trailing_comma_token = input.parse().ok();

        if !input.is_empty() {
            return Err(input.error("expected end of input"));
        }

        Ok(ComponentBuilder {
            named_init,
            trailing_init,
            _trailing_comma_token: trailing_comma_token,
        })
    }
}

#[allow(clippy::large_enum_variant)]
pub(crate) enum Tracked {
    Named(Ident),
    Unnamed(Expr),
}

impl Parse for Tracked {
    fn parse(input: ParseStream) -> Result<Self> {
        let fork = input.fork();
        if fork.parse::<Ident>().is_ok() && fork.is_empty() {
            let ident = input.parse()?;
            let tracked = Tracked::Named(ident);
            return Ok(tracked);
        }
        let expr = input.parse()?;

        let tracked = Tracked::Unnamed(expr);
        Ok(tracked)
    }
}
