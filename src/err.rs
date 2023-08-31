#[derive(Debug, thiserror::Error, PartialEq)]
pub enum LexError {
    #[error("Invalid token at line {line}, col {col}")]
    InvalidToken { line: usize, col: usize },

    #[error("Unterminated string literal at line {line}, col {col}")]
    UnterminatedString { line: usize, col: usize },

    #[error("Unterminated comment at line {line}, col {col}")]
    UnterminatedComment { line: usize, col: usize },

    #[error("Invalid number format at line {line}, col {col}")]
    InvalidNumberFormat { line: usize, col: usize },

    #[error("Number too large at line {line}, col {col}")]
    NumberTooLarge { line: usize, col: usize },

    #[error("Unrecognized escape sequence at line {line}, col {col}")]
    InvalidEscapeSequence { line: usize, col: usize },

    #[error("Invalid identifier start at line {line}, col {col}")]
    InvalidIdentifierStart { line: usize, col: usize },

    #[error("Reserved keyword used as identifier at line {line}, col {col}")]
    ReservedKeyword { line: usize, col: usize },

    #[error("Unmatched delimiter at line {line}, col {col}")]
    UnmatchedDelimiter { line: usize, col: usize },

    #[error("Incomplete character literal at line {line}, col {col}")]
    IncompleteCharLiteral { line: usize, col: usize },

    #[error("Not an init character {0}")]
    NotInit(String),

    #[error("Unexpected EOF at line {line}, col {col}")]
    UnexpectedEOF { line: usize, col: usize },
}

#[derive(Debug, thiserror::Error, PartialEq)]
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

    #[error(transparent)]
    LexError(#[from] LexError),
}
