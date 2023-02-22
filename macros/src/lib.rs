use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    token::{Brace, Bracket, Paren},
    Ident, LitBool, LitFloat, LitInt, Token,
};

struct Mapper {
    token_tree: proc_macro2::TokenStream,
}

impl Parse for Mapper {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut tokens = Vec::new();

        let vec = quote! {let mut tokens = Vec::new();};

        while !input.is_empty() {
            let span = input.span();

            let mut res = None;

            macro_rules! try_basic {
                ($token:tt, $kind:ident) => {
                    if res.is_none() && input.peek(Token![$token]) {
                        let _token: Token![$token] = input.parse()?;
                        res = Some(quote! { JitTokenKind::Basic(JitBasicToken::$kind) });
                    }
                };
            }

            try_basic!(pub, Pub);
            try_basic!(fn, Fn);
            try_basic!(struct, Struct);
            try_basic!(const, Const);
            try_basic!(as, As);
            try_basic!(if, If);
            try_basic!(else, Else);
            try_basic!(impl, Impl);
            try_basic!(return, Return);
            try_basic!(loop, Loop);
            try_basic!(while, While);
            try_basic!(break, Break);
            try_basic!(let, Let);
            try_basic!(->, Arrow);
            try_basic!(<=, LessEqual);
            try_basic!(>=, GreaterEqual);
            try_basic!(!=, NotEqual);
            try_basic!(==, DoubleEqual);
            try_basic!(::, DoubleColon);
            try_basic!(:, Colon);
            try_basic!(;, Semicolon);
            try_basic!(,, Comma);
            try_basic!(., Dot);
            try_basic!(=, Equal);
            try_basic!(+, Plus);
            try_basic!(-, Minus);
            try_basic!(*, Star);
            try_basic!(/, Slash);
            try_basic!(^, Caret);
            try_basic!(%, Percent);
            try_basic!(|, Pipe);
            try_basic!(&, Ampersand);
            try_basic!(<, LeftAngBracket);
            try_basic!(>, RightAngBracket);

            if res.is_none() {
                if input.peek(Token![#]) {
                    res = Some(parse_insertion(input)?);
                } else if input.peek(LitInt) {
                    res = Some(parse_lit_int(input)?);
                } else if input.peek(LitFloat) {
                    res = Some(parse_lit_float(input)?);
                } else if input.peek(LitBool) {
                    res = Some(parse_lit_bool(input)?);
                } else if input.peek(Ident) {
                    res = Some(parse_ident(input)?);
                } else if input.peek(Brace) {
                    let content;
                    let _ = syn::braced!(content in input);
                    let token_tree = Mapper::parse(&content)?.token_tree;
                    res = Some(quote! { JitTokenKind::Grouped {
                        kind: JitGroupKind::Braces,
                        tree: #token_tree,
                    } });
                } else if input.peek(Bracket) {
                    let content;
                    let _ = syn::bracketed!(content in input);
                    let token_tree = Mapper::parse(&content)?.token_tree;
                    res = Some(quote! { JitTokenKind::Grouped {
                        kind: JitGroupKind::Brackets,
                        tree: #token_tree,
                    } });
                } else if input.peek(Paren) {
                    let content;
                    let _ = syn::parenthesized!(content in input);
                    let token_tree = Mapper::parse(&content)?.token_tree;
                    res = Some(quote! { JitTokenKind::Grouped {
                        kind: JitGroupKind::Parentheses,
                        tree: #token_tree,
                    } });
                }
            }

            let Some(res) = res else {
                return Err(syn::Error::new(
                    span,
                    "Expected a valid token",
                ));
            };

            let span = quote! { JitSpan{} };
            let token = quote! { JitTokenWritable::write(#res, &mut tokens, #span) };

            tokens.push(token);
        }

        Ok(Self {
            token_tree: quote! {
                {
                    #vec
                    #(#tokens);*;
                    JitTokenTree {
                        tokens: tokens,
                    }
                }
            },
        })
    }
}

#[proc_macro]
pub fn jit_quote(input: TokenStream) -> TokenStream {
    let mapper = parse_macro_input!(input as Mapper);
    mapper.token_tree.into()
}

fn parse_lit_int(input: ParseStream) -> syn::Result<proc_macro2::TokenStream> {
    let span = input.span();
    let token: LitInt = input.parse()?;
    if token.suffix() == "" {
        // Can't parse without a suffix
        return Err(syn::Error::new(
            span,
            "Expected a type suffix on the number",
        ));
    }

    let kind = match token.suffix() {
        "u8" => quote!(U8),
        "u16" => quote!(U16),
        "u32" => quote!(U32),
        "u64" => quote!(U64),
        "usize" => quote!(USize),
        "i8" => quote!(I8),
        "i16" => quote!(I16),
        "i32" => quote!(I32),
        "i64" => quote!(I64),
        "isize" => quote!(ISize),
        "f32" => quote!(F32),
        "f64" => quote!(F64),
        _ => return Err(syn::Error::new(span, "Expected a valid integer bit size")),
    };

    let value = quote!(NumberValue::#kind(#token));

    Ok(quote! {
        JitTokenKind::Number(#value)
    })
}

fn parse_lit_float(input: ParseStream) -> syn::Result<proc_macro2::TokenStream> {
    let span = input.span();
    let token: LitFloat = input.parse()?;
    if token.suffix() == "" {
        // Can't parse without a suffix
        return Err(syn::Error::new(
            span,
            "Expected a type suffix on the number",
        ));
    }

    let kind = match token.suffix() {
        "f32" => quote!(F32),
        "f64" => quote!(F64),
        _ => return Err(syn::Error::new(span, "Expected a valid integer bit size")),
    };

    let value = quote!(NumberValue::#kind(#token));

    Ok(quote! {
        JitTokenKind::Number(#value)
    })
}

fn parse_lit_bool(input: ParseStream) -> syn::Result<proc_macro2::TokenStream> {
    let token: LitBool = input.parse()?;

    let value = token.value;

    Ok(quote! {
        JitTokenKind::Bool(#value)
    })
}

fn parse_ident(input: ParseStream) -> syn::Result<proc_macro2::TokenStream> {
    let token: Ident = input.parse()?;
    let value = token.to_string();

    Ok(quote! {
        JitTokenKind::Ident(#value.into())
    })
}

fn parse_insertion(input: ParseStream) -> syn::Result<proc_macro2::TokenStream> {
    let _: Token![#] = input.parse()?;

    let ident: Ident = input.parse()?;

    Ok(quote! { #ident })
}
