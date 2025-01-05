mod generator;

use proc_macro::TokenStream;
use crate::generator::generate;

#[proc_macro]
pub fn generate_symbols(input: TokenStream) -> TokenStream {
    generate(input.into()).into()
}