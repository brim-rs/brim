use proc_macro2::{TokenStream, Span};
use quote::{quote, format_ident};
use syn::{parse2, parse::Parse, parse::ParseStream, Token, Ident, LitStr, Result};
use indexmap::IndexMap;

struct SymbolDef {
    name: Ident,
    equals: Token![=],
    value: LitStr,
}

struct SymbolDefs {
    symbols: Vec<SymbolDef>,
}

impl Parse for SymbolDef {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(SymbolDef {
            name: input.parse()?,
            equals: input.parse()?,
            value: input.parse()?,
        })
    }
}

impl Parse for SymbolDefs {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut symbols = Vec::new();

        while !input.is_empty() {
            symbols.push(input.parse()?);
            if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
        }

        Ok(SymbolDefs { symbols })
    }
}

pub fn generate(tokens: TokenStream) -> TokenStream {
    // Parse the input tokens into our symbol definitions
    let SymbolDefs { symbols } = parse2(tokens).unwrap();

    // Generate constant definitions and map initialization code
    let mut index = 0;
    let const_definitions = symbols.iter().map(|symbol| {
        let name = &symbol.name;
        let value = &symbol.value;
        let current_index: usize = index;
        index += 1;

        quote! {
            pub const #name: Symbol = Symbol(SymbolIndex::from_usize(#current_index));
        }
    });

    let map_initialization = symbols.iter().map(|symbol| {
        let name = &symbol.name;
        let value = &symbol.value;

        quote! {
            map.insert(#name.0, #value.to_string());
        }
    });

    // Generate the final TokenStream with both constants and map
    quote! {
        // Constant definitions
        #(#const_definitions)*

        // Initialize the symbol table
        lazy_static! {
            pub static ref SYMBOL_STRINGS: IndexMap<SymbolIndex, String> = {
                let mut map = IndexMap::new();
                #(#map_initialization)*
                map
            };
        }
    }
}