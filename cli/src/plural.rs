pub fn plural(num: usize, singular: &str, plural: &str) -> String {
    if num == 1 { singular.to_string() } else { plural.to_string() }
}
