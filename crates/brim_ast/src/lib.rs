pub mod token;

/// A struct that represents already emitted diagnostic
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct ErrorEmitted(());

impl ErrorEmitted {
    pub fn new() -> Self {
        Self(())
    }
}
