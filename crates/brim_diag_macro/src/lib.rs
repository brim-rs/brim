mod function;

use crate::function::macro_derive_impl;
use proc_macro::TokenStream;

#[proc_macro_derive(Diagnostic, attributes(error, note, warning, help, bug))]
pub fn derive_diagnostic(item: TokenStream) -> TokenStream {
    macro_derive_impl(item)
}
