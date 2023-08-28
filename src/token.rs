pub enum Tok {
    Ident(String),
    Int(String),
    Alpha(char),
    Num(char),

    Add,
    Sub,
    Mul,
    Div,

    Eq,
    Gt,
    Lt,

    Neq,
    Deq,
    GEq,
    LEq,
    AddEq,
    SubEq,
    MulEq,
    DivEq,

    LParen,
    RParen,
    LSquare,
    RSquare,
    LBrace,
    RBrace,

    Semicolon,
    Colon,
    Comma,
    Bang,
    Pound,
    Under,
    Dot,
    SQ,
    DQ,

    T,
    Type,
    Let,
    Fn,
    Asm,
    Struct,
    Trait,
    Comptime,
    Package,
    Pub,
    Mut,
    Use,

    If,
    Else,
    Match,
    Return,

    Invalid,
    EOF,
}

impl From<char> for Tok {
    fn from(value: char) -> Self {
        match value {
            '+' => Tok::Add,
            '-' => Tok::Sub,
            '*' => Tok::Mul,
            '/' => Tok::Div,
            '(' => Tok::LParen,
            ')' => Tok::RParen,
            '[' => Tok::LSquare,
            ']' => Tok::RSquare,
            '{' => Tok::LBrace,
            '}' => Tok::RBrace,
            ';' => Tok::Semicolon,
            ':' => Tok::Colon,
            ',' => Tok::Comma,
            '=' => Tok::Eq,
            '>' => Tok::Gt,
            '<' => Tok::Lt,
            '!' => Tok::Bang,
            '#' => Tok::Pound,
            '_' => Tok::Under,
            '.' => Tok::Dot,
            '\'' => Tok::SQ,
            '"' => Tok::DQ,
            _ if value.is_alphabetic() => Tok::Alpha(value),
            _ if value.is_numeric() => Tok::Num(value),
            _ => Tok::Invalid,
        }
    }
}

impl From<&str> for Tok {
    fn from(value: &str) -> Self {
        match value {
            "T" => Tok::T,
            "type" => Tok::Type,
            "let" => Tok::Let,
            "fn" => Tok::Fn,
            "asm" => Tok::Asm,
            "struct" => Tok::Struct,
            "trait" => Tok::Trait,
            "comptime" => Tok::Comptime,
            "pub" => Tok::Pub,
            "package" => Tok::Package,
            "mut" => Tok::Mut,
            "use" => Tok::Use,
            "if" => Tok::If,
            "else" => Tok::Else,
            "match" => Tok::Match,
            "return" => Tok::Return,
            _ => Tok::Ident(value.to_string()),
        }
    }
}
