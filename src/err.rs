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

    #[error("Unexpected EOF at (line :: {line}, col :: {col})")]
    UnexpectedEOF { line: usize, col: usize },

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

    #[error("Expected expression ({expr}) at (line :: {line}, col :: {col})")]
    ExpectedExpr {
        expr: String,
        line: usize,
        col: usize,
    },

    #[error("Expected keyword ({kw}) at (line :: {line}, col :: {col})")]
    ExpectedKeyword { kw: String, line: usize, col: usize },

    #[error("Expected type ({ty}) at (line :: {line}, col :: {col})")]
    ExpectedType { ty: String, line: usize, col: usize },
}
