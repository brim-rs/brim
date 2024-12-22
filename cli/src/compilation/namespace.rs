// This function converts any string into a valid namespace.
pub fn generate_safe_namespace(path: impl Into<String>) -> String {
    let path = path.into();
    path.replace(|c: char| !c.is_alphanumeric(), "_")
}