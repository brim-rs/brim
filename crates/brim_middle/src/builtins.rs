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
        m.insert("ok".to_string(), FunctionData {
            description: "Returns an Ok variant with the given value",
            expected_args: 1,
        });
        m.insert("err".to_string(), FunctionData {
            description: "Returns an Err variant with the given value",
            expected_args: 1,
        });
        m
    };
}
