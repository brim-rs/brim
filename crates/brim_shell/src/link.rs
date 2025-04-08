use std::fmt;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Link<'a> {
    pub text: &'a str,
    pub url: &'a str,
}

impl<'a> Link<'a> {
    pub fn new(text: &'a str, url: &'a str) -> Self {
        Self { text, url }
    }
}

impl fmt::Display for Link<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\u{1b}]8;;{}\u{1b}\\{}\u{1b}]8;;\u{1b}\\", self.url, self.text)
    }
}
