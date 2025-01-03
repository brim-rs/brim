use proc_macro::TokenStream;

use quote::quote;
use syn::{DeriveInput, parse_macro_input, Expr};
use thiserror::Error;

use std::iter::Iterator;
use syn::spanned::Spanned;

#[derive(Debug, Error)]
pub enum MacroFunctionError {
    #[error("Parse error: {0}")]
    ParseError(#[from] syn::Error),
}

macro_rules! macro_compile_error {
    ($span:expr, $($arg:tt)*) => {
        return TokenStream::from(syn::Error::new(
            $span,
            format!($($arg)*),
        ).to_compile_error())
    };

    () => {};
}

pub fn get_string_literal(expr: &Expr) -> String {
    match expr {
        Expr::Lit(expr_lit) => {
            if let syn::Lit::Str(lit_str) = &expr_lit.lit {
                lit_str.value()
            } else {
                panic!("Message must be a string literal");
            }
        }
        _ => panic!("Message must be a string literal"),
    }
}

pub fn macro_derive_impl(item: TokenStream) -> TokenStream {
    {
        let ast = parse_macro_input!(item as DeriveInput);
        let attrs = &ast.attrs;
        let struct_name = &ast.ident;

        let diagnostic_attrs = attrs.iter()
            .find(|attr| {
                let ident = attr.path().segments.last().unwrap().ident.to_string().to_lowercase();
                matches!(ident.as_str(), "help" | "note" | "warning" | "error" | "bug")
            });

        if diagnostic_attrs.is_none() {
            macro_compile_error!(ast.span(), "One of help, note, warning, error, or bug attribute is required");
        }

        let attr = diagnostic_attrs.unwrap();
        let severity = attr.path().segments.last().unwrap().ident.to_string().to_lowercase();
        let severity_variant = severity.chars().next().unwrap().to_uppercase().collect::<String>() + &severity[1..];

        let expr = attr.parse_args::<Expr>().unwrap();
        let error_string = get_string_literal(&expr);

        let error_code = attrs.iter()
            .find(|attr| attr.path().segments.last().unwrap().ident == "code")
            .map(|attr| {
                let expr = attr.parse_args::<Expr>().unwrap();
                get_string_literal(&expr)
            });

        let code_expr = if let Some(code) = error_code {
            quote! { Some(#code.to_string()) }
        } else {
            quote! { None }
        };

        let fields = match &ast.data {
            syn::Data::Struct(data) => {
                if let syn::Fields::Named(fields) = &data.fields {
                    fields
                } else {
                    macro_compile_error!(ast.span(), "Only named fields are supported");
                }
            }
            _ => macro_compile_error!(ast.span(), "Only structs are supported"),
        };

        let field_names = fields.named.iter()
            .map(|field| &field.ident)
            .collect::<Vec<_>>()
            .iter()
            .map(|ident| quote! { #ident })
            .collect::<Vec<_>>();

        let severity_ident = syn::Ident::new(&severity_variant, proc_macro2::Span::call_site());

        Ok::<TokenStream, TokenStream>(Into::into(quote! {
            impl<'a> ToDiagnostic<'a> for #struct_name {
                fn message(&self) -> String {
                    let #(#field_names = self.#field_names;)*
                    format!(#error_string)
                }

                fn labels(&self) -> Vec<brim::diagnostic::Label<'a, usize>> {
                    vec![]
                }

                fn severity(&self) -> Severity {
                    Severity::#severity_ident
                }

                fn code(&self) -> Option<String> {
                    #code_expr
                }

                fn notes(&self) -> Vec<String> {
                    vec![]
                }
            }
        }))
    }.unwrap_or_else(|err| err)
}
