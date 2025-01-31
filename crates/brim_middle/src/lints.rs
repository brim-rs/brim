use brim_diag_macro::Diagnostic;
use brim_diagnostics::diagnostic::{Diagnostic as Diag, Label, LabelStyle, Severity, ToDiagnostic};
use brim_span::span::Span;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

pub struct Lint {
    pub enabled: bool,
    pub diag: Box<dyn ToDiagnostic>,
}

#[derive(Debug, Clone)]
pub struct Lints {
    enabled_lints: HashMap<String, bool>,
}

impl Lints {
    pub fn new() -> Self {
        Self {
            enabled_lints: HashMap::new(),
        }
    }

    pub fn enable(&mut self, name: &str) {
        self.enabled_lints.insert(name.to_string(), true);
    }

    pub fn disable(&mut self, name: &str) {
        self.enabled_lints.insert(name.to_string(), false);
    }

    pub fn is_enabled(&self, name: &str) -> bool {
        *self.enabled_lints.get(name).unwrap_or(&false)
    }
}

macro_rules! define_lints {
    (
        $(
            $( #[$meta:meta] )*
            $vis:vis struct $name:ident => $method_name:ident {
                $( $( #[$field_meta:meta] )* $field:ident : $ty:ty ),* $(,)?
            }
        )*
    ) => {
        $(
            $( #[$meta] )*
            $vis struct $name {
                $( $( #[$field_meta] )* pub $field: $ty ),*
            }

            impl $name {
                pub fn new($( $field: $ty ),*) -> Self {
                    Self { $( $field ),* }
                }
            }
        )*

        impl Lints {
            $(
                pub fn $method_name(&self, $( $field: $ty ),*) -> Lint {
                    let enabled = self.is_enabled(stringify!($name));
                    Lint {
                        enabled,
                        diag: Box::new($name { $( $field ),* }),
                    }
                }
            )*

            pub fn configure(config: &LintsConfig) -> Self {
                let mut l = Self::new();

                $(
                    l.enabled_lints.insert(stringify!($name).to_string(), config.$method_name.unwrap_or(true));
                )*

                l
            }
        }

        #[derive(Deserialize, Debug, Clone, Serialize)]
        pub struct LintsConfig {
            $( pub $method_name: Option<bool>, )*
        }

        impl Default for LintsConfig {
            fn default() -> Self {
                Self {
                    $( $method_name: Some(true), )*
                }
            }
        }
    };
}

define_lints! {
    #[derive(Diagnostic)]
    #[warning("variable '{name}' should be snake case")]
    pub struct VariableNotSnakeCase => variable_not_snake_case {
        name: String,
        new_name: String,
        #[warning("rename to '{new_name}'")]
        span: (Span, usize),
    }

    #[derive(Diagnostic)]
    #[warning("function '{name}' should be camel case")]
    pub struct FunctionNotCamelCase => function_not_camel_case {
        name: String,
        new_name: String,
        #[warning("rename to '{new_name}'")]
        span: (Span, usize),
    }
}
