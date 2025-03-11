use lazy_static::lazy_static;
use std::collections::HashMap;

#[derive(Debug)]
pub struct FunctionData {
    pub description: &'static str,
    pub expected_args: usize,
}

lazy_static! {
    pub static ref BUILTIN_FUNCTIONS: HashMap<String, FunctionData> = {
        let mut m = HashMap::new();
        m.insert("os".to_string(), FunctionData {
            description: "Returns the operating system",
            expected_args: 0,
        });
        m.insert("cast".to_string(), FunctionData {
            description: "Casts an any type to another type",
            expected_args: 2,
        });
        m
    };
}
