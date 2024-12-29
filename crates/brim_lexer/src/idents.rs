pub fn is_identifier_start(c: char) -> bool {
    matches!(c as u32, 0x0024 /* $ */ | 0x005F /* _ */) || c.is_alphabetic()
}

pub fn is_valid_ident_continue(c: char) -> bool {
    c.is_alphanumeric() || c as u32 == 0x005F /* _ */
}
