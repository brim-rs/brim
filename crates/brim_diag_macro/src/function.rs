use proc_macro::TokenStream;
use quote::quote;
use syn::{DeriveInput, Expr, parse_macro_input, spanned::Spanned};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum MacroFunctionError {
    #[error("Parse error: {0}")]
    ParseError(#[from] syn::Error),
    #[error("Invalid attribute: {0}")]
    InvalidAttribute(String),
    #[error("Missing required attribute")]
    MissingAttribute,
}

const VALID_SEVERITIES: &[&str] = &["help", "warning", "error", "bug", "notel"];

fn get_string_literal(expr: &Expr) -> Result<String, MacroFunctionError> {
    match expr {
        Expr::Lit(expr_lit) => {
            if let syn::Lit::Str(lit_str) = &expr_lit.lit {
                Ok(lit_str.value())
            } else {
                Err(MacroFunctionError::InvalidAttribute(
                    "Message must be a string literal".to_string(),
                ))
            }
        }
        _ => Err(MacroFunctionError::InvalidAttribute(
            "Message must be a string literal".to_string(),
        )),
    }
}

fn is_valid_severity(ident: &str) -> bool {
    VALID_SEVERITIES.contains(&ident.to_lowercase().as_str())
}

fn get_label_style(severity: &str) -> proc_macro2::TokenStream {
    match severity.to_lowercase().as_str() {
        "error" => quote! { LabelStyle::Error },
        "warning" => quote! { LabelStyle::Warning },
        "notel" => quote! { LabelStyle::Note },
        _ => quote! { LabelStyle::Primary },
    }
}

pub fn macro_derive_impl(item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item as DeriveInput);
    let result = impl_diagnostic_derive(&ast);

    match result {
        Ok(token_stream) => token_stream,
        Err(error) => {
            TokenStream::from(syn::Error::new(ast.span(), error.to_string()).to_compile_error())
        }
    }
}

fn impl_diagnostic_derive(ast: &DeriveInput) -> Result<TokenStream, MacroFunctionError> {
    let attrs = &ast.attrs;
    let struct_name = &ast.ident;

    let diagnostic_attr = attrs
        .iter()
        .find(|attr| {
            let ident = attr
                .path()
                .segments
                .last()
                .map(|seg| seg.ident.to_string().to_lowercase())
                .unwrap_or_default();
            is_valid_severity(&ident)
        })
        .ok_or(MacroFunctionError::MissingAttribute)?;

    let severity = diagnostic_attr
        .path()
        .segments
        .last()
        .unwrap()
        .ident
        .to_string()
        .to_lowercase();

    let severity_variant = format!(
        "{}{}",
        severity.chars().next().unwrap().to_uppercase(),
        &severity[1..]
    );

    let expr = diagnostic_attr.parse_args::<Expr>()?;
    let error_string = get_string_literal(&expr)?;

    let fields = match &ast.data {
        syn::Data::Struct(data) => match &data.fields {
            syn::Fields::Named(fields) => fields,
            _ => {
                return Err(MacroFunctionError::InvalidAttribute(
                    "Only named fields are supported".to_string(),
                ));
            }
        },
        _ => {
            return Err(MacroFunctionError::InvalidAttribute(
                "Only structs are supported".to_string(),
            ));
        }
    };

    let message_fields: Vec<_> = fields
        .named
        .iter()
        .filter_map(|field| {
            if field.attrs.iter().all(|attr| {
                attr.path()
                    .segments
                    .last()
                    .map_or(true, |seg| !is_valid_severity(&seg.ident.to_string()))
            }) {
                field.ident.as_ref()
            } else {
                None
            }
        })
        .collect();

    let note_fields: Vec<_> = fields
        .named
        .iter()
        .filter_map(|field| {
            field.attrs.iter().find(|attr| {
                let ident = attr
                    .path()
                    .segments
                    .last()
                    .unwrap()
                    .ident
                    .to_string()
                    .to_lowercase();
                ident == "note"
            })?;

            field.ident.as_ref()
        })
        .collect();

    let span_fields: Vec<_> = fields
        .named
        .iter()
        .filter_map(|field| {
            let attr = field.attrs.iter().find(|attr| {
                let ident = attr
                    .path()
                    .segments
                    .last()
                    .unwrap()
                    .ident
                    .to_string()
                    .to_lowercase();
                is_valid_severity(&ident)
            })?;

            let severity = attr
                .path()
                .segments
                .last()
                .unwrap()
                .ident
                .to_string()
                .to_lowercase();

            let message = attr
                .parse_args::<Expr>()
                .ok()
                .and_then(|expr| get_string_literal(&expr).ok())
                .unwrap_or_default();

            Some((field.ident.clone(), message, severity))
        })
        .collect();

    let message_field_refs = message_fields.iter().map(|&ident| {
        quote! { let #ident = &self.#ident; }
    });

    let label_implementations = span_fields.iter().map(|(field_ident, message, severity)| {
        if let Some(ident) = field_ident {
            let label_style = get_label_style(severity);
            let message_field_refs_clone = message_fields.iter().map(|&ident| {
                quote! { let #ident = &self.#ident; }
            });

            quote! {
                {
                    #(#message_field_refs_clone)*
                    let message = format!(#message);

                    Label::new(
                        #label_style,
                        self.#ident.1,
                        self.#ident.0.range()
                    )
                    .with_message(message)
                }
            }
        } else {
            quote! {}
        }
    });

    let notes = note_fields.iter().map(|&ident| {
        quote! {
            let #ident = &self.#ident;
        }
    });

    let severity_ident = syn::Ident::new(&severity_variant, proc_macro2::Span::call_site());

    Ok(TokenStream::from(quote! {
        #[allow(unused)]
        impl ToDiagnostic for #struct_name {
            fn message(&self) -> String {
                #(#message_field_refs)*
                format!(#error_string)
            }

            fn labels(&self) -> Vec<Label<usize>> {
                vec![
                    #(#label_implementations,)*
                ]
            }

            fn severity(&self) -> Severity {
                Severity::#severity_ident
            }

            fn code(&self) -> Option<String> {
                None
            }

            fn notes(&self) -> Vec<String> {
                #(#notes)*

                vec![
                    #(#note_fields.to_string(),)*
                ]
            }
        }
    }))
}
