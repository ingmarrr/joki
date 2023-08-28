use crate::token::Tok;

pub struct Lex {
    pub tokens: Vec<Tok>,
    pub pos: usize,
}
