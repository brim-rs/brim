mod generator;

use crate::generator::generate;
use proc_macro::TokenStream;

#[proc_macro]
pub fn generate_symbols(input: TokenStream) -> TokenStream {
    generate(input.into()).into()
}
