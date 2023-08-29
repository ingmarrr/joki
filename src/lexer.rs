use std::{iter::Peekable, str::Chars};

use crate::err;
use crate::token::{InitTok, Tok};

pub struct Lexer<'a> {
    pub chs: Peekable<Chars<'a>>,
    pub errs: Vec<err::LexError>,
    pub line: usize,
    pub col: usize,
}

macro_rules! check {
    ($self:ident, $($ch:literal => $tok:expr),*; $else:expr) => {
        match $self.peek() {
            Some(ch) => {
                match ch {
                    $($ch => {
                        $self.take();
                        return Some($tok);
                    })*
                    _ => return Some($else),
                }
            }
            None => return None,
        }
    };
}
impl<'a> Lexer<'a> {
    pub fn new(inp: &'a str) -> Lexer<'a> {
        Lexer {
            chs: inp.chars().peekable(),
            errs: Vec::new(),
            line: 0,
            col: 0,
        }
    }

    pub fn lex(&mut self) -> Vec<Tok> {
        let mut toks = Vec::new();
        while let Some(tok) = self.next_tok() {
            toks.push(tok);
        }
        toks
    }

    pub fn next_tok(&mut self) -> Option<Tok> {
        self.skip_ws();
        while let Some(ch) = self.take() {
            let tok = Tok::from(ch);
            if let Tok::Invalid = tok {
                self.errs.push(err::LexError::InvalidToken {
                    line: self.line,
                    col: self.col,
                });
                tracing::error!("Invalid token at line {}, col {}", self.line, self.col);
            };

            let init_tok = InitTok::try_from(&tok);

            if let Err(_) = init_tok {
                return Some(tok);
            }

            let init_tok = init_tok.unwrap();
            match init_tok {
                InitTok::SQ => return Some(Tok::Char(self.take_until('\''))),
                InitTok::DQ => return Some(Tok::String(self.take_until('"'))),
                InitTok::Add => check!(self, '=' => Tok::AddEq; Tok::Add),
                InitTok::Sub => check!(self, '=' => Tok::SubEq; Tok::Sub),
                InitTok::Mul => check!(self, '=' => Tok::MulEq; Tok::Mul),
                InitTok::Div => check!(self, '=' => Tok::DivEq, '/' => Tok::Comment; Tok::Div),
                InitTok::Eq => check!(self, '=' => Tok::Deq; Tok::Eq),
                InitTok::Lt => check!(self, '=' => Tok::Leq, '<' => Tok::Lsl; Tok::Lt),
                InitTok::Gt => check!(self, '=' => Tok::Geq, '>' => Tok::Lsr; Tok::Gt),
                InitTok::Bang => check!(self, '=' => Tok::Neq; Tok::Bang),
                InitTok::Num(_) => {
                    let num = self.take_while(ch, |tok| match tok {
                        Tok::Num(_) | Tok::Under => true,
                        _ => false,
                    });
                    return Some(Tok::Scalar(num));
                }
                InitTok::Alpha(_) => {
                    let ident = self.take_while(ch, |tok| match tok {
                        Tok::Alpha(_) | Tok::Num(_) | Tok::Under => true,
                        _ => false,
                    });
                    let kw = Tok::from(ident.as_str());
                    if let Tok::Invalid = kw {
                        return Some(Tok::Ident(ident));
                    }
                    return Some(kw);
                }
                InitTok::Under => {
                    let ident = self.take_while(ch, |tok| match tok {
                        Tok::Alpha(_) | Tok::Num(_) | Tok::Under => true,
                        _ => false,
                    });
                    for ch in ident.chars() {
                        if ch != '_' {
                            return Some(Tok::Ident(ident));
                        }
                    }
                    return Some(Tok::Under);
                }
            }
        }
        None
    }

    fn take(&mut self) -> Option<char> {
        let ch = self.chs.next();
        if ch == Some('\n') {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
        ch
    }

    fn take_until(&mut self, ch: char) -> String {
        let mut s = String::new();
        while let Some(c) = self.take() {
            if c == ch {
                break;
            }
            s.push(c);
        }
        s
    }

    fn take_while(&mut self, starting: char, f: impl Fn(Tok) -> bool) -> String {
        let mut buf = format!("{}", starting);
        while let Some(c) = self.peek() {
            if !f(Tok::from(c)) {
                break;
            }
            self.take();
            buf.push(c);
        }
        buf.to_owned()
    }

    fn peek(&mut self) -> Option<char> {
        self.chs.peek().map(|ch| *ch)
    }

    fn skip_ws(&mut self) {
        while let Some(tok) = self.peek().map(Tok::try_from) {
            match tok {
                Ok(Tok::Ws) | Ok(Tok::Nl) => {
                    self.take();
                }
                _ => break,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbols() {
        let txt = "+-*/()[]{};:,= ><&|^~!@#?_.\\ += -= *= /= == != >= <= << >> // ! & | ^ ~ ";
        let mut lexer = Lexer::new(txt);
        let toks = lexer.lex();
        assert_eq!(
            toks,
            vec![
                Tok::Add,
                Tok::Sub,
                Tok::Mul,
                Tok::Div,
                Tok::LParen,
                Tok::RParen,
                Tok::LSquare,
                Tok::RSquare,
                Tok::LBrace,
                Tok::RBrace,
                Tok::Semi,
                Tok::Colon,
                Tok::Comma,
                Tok::Eq,
                Tok::Gt,
                Tok::Lt,
                Tok::And,
                Tok::Or,
                Tok::Caret,
                Tok::Wave,
                Tok::Bang,
                Tok::At,
                Tok::Pound,
                Tok::Ques,
                Tok::Under,
                Tok::Dot,
                Tok::Esc,
                Tok::AddEq,
                Tok::SubEq,
                Tok::MulEq,
                Tok::DivEq,
                Tok::Deq,
                Tok::Neq,
                Tok::Geq,
                Tok::Leq,
                Tok::Lsl,
                Tok::Lsr,
                Tok::Comment,
                Tok::Bang,
                Tok::And,
                Tok::Or,
                Tok::Caret,
                Tok::Wave,
            ]
        );
    }

    #[test]
    fn test_kws() {
        let txt =
            "T type let fn asm struct trait comp pub pack mut use if else match return true false for";
        let mut lexer = Lexer::new(txt);
        let toks = lexer.lex();
        assert_eq!(
            toks,
            vec![
                Tok::T,
                Tok::Type,
                Tok::Let,
                Tok::Fn,
                Tok::Asm,
                Tok::Struct,
                Tok::Trait,
                Tok::Comptime,
                Tok::Pub,
                Tok::Package,
                Tok::Mut,
                Tok::Use,
                Tok::If,
                Tok::Else,
                Tok::Match,
                Tok::Return,
                Tok::True,
                Tok::False,
                Tok::For,
            ]
        );
    }

    #[test]
    fn test_str_char() {
        let txt = r#"
        'abc' "abc"
        "#;
        let mut lexer = Lexer::new(txt);
        let toks = lexer.lex();
        assert_eq!(
            toks,
            vec![Tok::Char("abc".to_string()), Tok::String("abc".to_string()),]
        );
    }

    #[test]
    fn test_idents() {
        let txt = r#"
        abc abc123 _abc _123 ___
        "#;
        let mut lexer = Lexer::new(txt);
        let toks = lexer.lex();
        assert_eq!(
            toks,
            vec![
                Tok::Ident("abc".to_string()),
                Tok::Ident("abc123".to_string()),
                Tok::Ident("_abc".to_string()),
                Tok::Ident("_123".to_string()),
                Tok::Under,
            ]
        );
    }

    #[test]
    fn test_num() {
        let txt = r#"
        123 123abc 123_123
        "#;
        let mut lexer = Lexer::new(txt);
        let toks = lexer.lex();
        assert_eq!(
            toks,
            vec![
                Tok::Scalar("123".to_string()),
                Tok::Scalar("123".to_string()),
                Tok::Ident("abc".to_string()),
                Tok::Scalar("123_123".to_string()),
            ]
        );
    }

    #[test]
    fn test_floats() {
        let txt = r#"
        123.123
        "#;
        let mut lexer = Lexer::new(txt);
        let toks = lexer.lex();
        assert_eq!(
            toks,
            vec![
                Tok::Scalar("123".to_string()),
                Tok::Dot,
                Tok::Scalar("123".to_string()),
            ]
        );
    }
}
