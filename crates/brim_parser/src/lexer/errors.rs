use brim::Diagnostic;
use brim::diagnostic::Severity;
use brim::diagnostic::ToDiagnostic;
use brim::symbol::Symbol;

#[derive(Diagnostic)]
#[error("Emojis can't be used in identifiers: `{ident}`")]
pub struct EmojiIdentifier {
    pub ident: Symbol,
}
