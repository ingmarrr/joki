#[derive(Debug, thiserror::Error)]
pub enum LexError {
    #[error("Invalid token at line {line}, col {col}")]
    InvalidToken { line: usize, col: usize },

    #[error("Invalid character at (line :: {line} | col :: {col})")]
    InvalidChar { line: usize, col: usize },

    #[error("Not an init character :: {0}")]
    NotInit(String),
}
