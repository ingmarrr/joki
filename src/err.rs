#[derive(Debug, thiserror::Error)]
pub enum LexError {
    #[error("Invalid token at line {line}, col {col}")]
    InvalidToken { line: usize, col: usize },

    #[error("Invalid character at (line :: {line} | col :: {col})")]
    InvalidChar { line: usize, col: usize },

    #[error("Not an init character :: {0}")]
    NotInit(String),
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Invalid token at (line :: {line}, col :: {col})")]
    InvalidToken { line: usize, col: usize },

    #[error("Expected identifier ({ident}) at (line :: {line}, col :: {col})")]
    ExpectedIdent {
        ident: String,
        line: usize,
        col: usize,
    },

    #[error("Expected token ({tok}) at (line :: {line}, col :: {col})")]
    ExpectedToken {
        tok: String,
        line: usize,
        col: usize,
    },

    #[error("Expected expression at (line :: {line}, col :: {col})")]
    ExpectedExpr { line: usize, col: usize },
}
