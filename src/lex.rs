use crate::ast::IntBase;
use crate::err::{self, LexError};
use crate::token::{InitTok, Tok};

macro_rules! check {
    ($self:ident, $($ch:literal => $tok:expr),*; $else:expr) => {
        match $self.peek() {
            Some(ch) => {
                match ch {
                    $($ch => {
                        $self.take();
                        tracing::info!("Found valid token :: {}", $tok);
                        return Ok($tok);
                    })*
                    _ => {
                        tracing::info!("Found valid token :: {}", $else);
                        return Ok($else);
                    }
                }
            }
            None => Err(LexError::UnexpectedEOF {
                line: $self.cx.line,
                col: $self.cx.col,
            }),
        }
    };
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cx {
    ix: usize,
    ch: char,
    fchar: bool,
    pub line: usize,
    pub col: usize,
}

pub struct Lexer {
    pub errs: Vec<err::LexError>,
    pub toks: Vec<Tok>,
    pub cx: Cx,
    pub tmpcx: Option<Cx>,
    chars: Vec<char>,
}

impl Lexer {
    pub fn new<'a>(inp: &'a str) -> Lexer {
        Lexer {
            chars: inp.chars().collect(),
            errs: Vec::new(),
            toks: Vec::new(),
            cx: Cx {
                ix: 0,
                ch: '\0',
                line: 0,
                col: 0,
                fchar: true,
            },
            tmpcx: None,
        }
    }

    pub fn lex(&mut self) -> Vec<Tok> {
        let mut toks = Vec::new();
        loop {
            let tok = self.next_token();
            match tok {
                Ok(Tok::EOF) => break,
                Ok(tok) => toks.push(tok),
                Err(err) => self.errs.push(err),
            }
        }
        toks
    }

    pub fn next_token(&mut self) -> Result<Tok, LexError> {
        if let Some(cx) = &self.tmpcx {
            self.cx = cx.clone();
            self.tmpcx = None;
        }
        self._next_token()
    }

    /// Peeks the next token by using a temporary context
    /// This function can be called as many times in a row as there
    /// are tokens in the input. It will not advance the context.
    /// Once the `next_token` function is called, the temporary context will
    /// be reset to the current context.
    pub fn lookahead(&mut self) -> Result<Tok, LexError> {
        if let None = self.tmpcx {
            self.tmpcx = Some(self.cx.clone());
        }
        self._next_token()
    }

    fn _next_token(&mut self) -> Result<Tok, LexError> {
        self.skip_ws();
        loop {
            self.cx.ch = self.take().unwrap_or('\0');
            let tok = Tok::from(self.cx.ch);
            if let Tok::EOF = tok {
                return Ok(Tok::EOF);
            }
            if let Tok::Invalid = tok {
                // self.errs.push(err::LexError::InvalidToken {
                //     line: self.cx.line,
                //     col: self.cx.col,
                // });
                tracing::error!(
                    "Invalid token at line {}, col {}",
                    self.cx.line,
                    self.cx.col
                );
            };

            let itok = match InitTok::try_from(&tok) {
                Ok(itok) => itok,
                Err(_) => {
                    tracing::info!("Found valid token :: {}", tok);
                    return Ok(tok);
                }
            };

            return match itok {
                InitTok::Alpha | InitTok::Under => self.lx_ident(),
                InitTok::Num => self.lx_num(),
                InitTok::SQ => self.lx_char(),
                InitTok::DQ => self.lx_str(),
                InitTok::Dot => self.lx_dot(),
                InitTok::Slash => self.lx_slash(),
                InitTok::Plus => check!(self,  '=' => Tok::AddEq; Tok::Plus),
                InitTok::Minus => check!(self,  '=' => Tok::SubEq; Tok::Minus),
                InitTok::Star => check!(self,  '=' => Tok::MulEq; Tok::Star),
                InitTok::Eq => check!(self,  '=' => Tok::Deq; Tok::Eq),
                InitTok::Bang => check!(self, '=' => Tok::Neq; Tok::Bang),
                InitTok::Lt => check!(self, '=' => Tok::Leq, '<' => Tok::Lsl; Tok::Lt),
                InitTok::Gt => check!(self, '=' => Tok::Geq, '>' => Tok::Lsr; Tok::Gt),
                InitTok::Colon => check!(self, ':' => Tok::DoubleColon; Tok::Colon),
            };
        }
    }

    // TODO! Add tests
    fn lx_num(&mut self) -> Result<Tok, LexError> {
        match self.cx.ch {
            '0' => {
                let peek = self.peek();
                match peek {
                    Some('x') | Some('b') | Some('o') => {
                        self.take();
                        let base = match peek {
                            Some('x') => 16,
                            Some('b') => 2,
                            Some('o') => 8,
                            _ => unreachable!(),
                        };
                        let mut num: String = '0'.into();
                        num.push(self.cx.ch);
                        num.push(self.take().unwrap());
                        while let Some(ch) = self.peek() {
                            if ch.is_digit(base) {
                                num.push(self.take().unwrap());
                            } else {
                                break;
                            }
                        }

                        if num.len() == 2 {
                            return Err(LexError::MissingIntegerLiteral {
                                line: self.cx.line,
                                col: self.cx.col,
                            });
                        }

                        let size = num.len();
                        tracing::info!("Found valid int :: {}", num);
                        let tok = Ok(Tok::LitInt {
                            buf: num,
                            size,
                            base: peek.unwrap().into(),
                        });
                        return tok;
                    }
                    _ => {
                        return Err(LexError::InvalidNumberFormat {
                            line: self.cx.line,
                            col: self.cx.col,
                        })
                    }
                };
            }
            _ => {}
        };

        let mut buf = format!("{}", self.cx.ch);
        let base = 10;
        let mut size = 1;

        while let Some(ch) = self.peek() {
            if ch.is_digit(base) {
                buf.push(self.take().unwrap());
                size += 1;
            } else {
                if ch == '_' {
                    self.take();
                    continue;
                }
                break;
            }
        }
        tracing::info!("Found valid int :: {}", buf);

        if let Some(ch) = self.peek_nth(0) {
            if ch == '.' {
                match self.peek_nth(1) {
                    Some('.') => {
                        return Ok(Tok::LitInt {
                            buf,
                            size,
                            base: IntBase::Dec,
                        })
                    }
                    Some(c) if !c.is_digit(10) => {
                        return Ok(Tok::LitInt {
                            buf,
                            size,
                            base: IntBase::Dec,
                        })
                    }
                    None => {
                        return Ok(Tok::LitInt {
                            buf,
                            size,
                            base: IntBase::Dec,
                        })
                    }
                    Some(_) => {}
                };
                buf.push(self.take().unwrap());
                size += 1;
                while let Some(ch) = self.peek() {
                    if ch.is_digit(base) {
                        buf.push(self.take().unwrap());
                        size += 1;
                    } else {
                        break;
                    }
                }
                let tok = Ok(Tok::LitFloat { buf, size });
                tracing::info!("Found valid float :: {:#?}", tok);
                return tok;
            }
        }

        tracing::info!("Found valid int :: {}", buf);
        Ok(Tok::LitInt {
            buf,
            size,
            base: IntBase::Dec,
        })
    }

    fn lx_ident(&mut self) -> Result<Tok, LexError> {
        let ident = self.take_while(|tok| match tok {
            Tok::Alpha(_) | Tok::Num(_) | Tok::Under => true,
            _ => false,
        });
        let kw = Tok::from(ident.as_str());
        if let Tok::Ident(_) = kw {
            tracing::info!("Found valid ident :: {}", ident);
            if ident == "_" {
                return Ok(Tok::Under);
            }
            return Ok(Tok::Ident(ident));
        }
        tracing::info!("Found valid keyword :: {}", ident);
        return Ok(kw);
    }

    fn lx_char(&mut self) -> Result<Tok, LexError> {
        let buf = self.take_until('\'');
        if buf.len() > 1 || buf.len() == 0 {
            return Err(LexError::InvalidCharLiteral {
                line: self.cx.line,
                col: self.cx.col,
            });
        }
        let ch = buf.chars().next().unwrap();
        let tok = Ok(Tok::LitChar(ch));
        tracing::info!("Found valid char :: {}", ch);
        tok
    }

    fn lx_str(&mut self) -> Result<Tok, LexError> {
        let buf = self.take_until('"');
        tracing::info!("Found valid string :: {}", buf);
        let tok = Ok(Tok::LitString {
            size: buf.len(),
            buf,
        });
        tok
    }

    fn lx_dot(&mut self) -> Result<Tok, LexError> {
        let peek = self.peek();
        tracing::info!("Found :: {:#?}", peek);
        match peek {
            Some('.') => {
                tracing::info!("Found range");
                self.take();
                match self.peek() {
                    Some('=') => return Ok(Tok::RangeInc),
                    _ => return Ok(Tok::RangeEx),
                }
            }
            Some(c) if c.is_digit(10) => {
                let mut num = self.take_while(|tok| match tok {
                    Tok::Num(_) => true,
                    _ => false,
                });
                tracing::info!("Found valid float :: {}", num);
                if num.len() == 0 {
                    num = "0".into();
                }
                return Ok(Tok::LitFloat {
                    buf: format!("0.{}", num),
                    size: num.len() + 2,
                });
            }
            _ => Ok(Tok::Dot),
        }
    }

    fn lx_slash(&mut self) -> Result<Tok, LexError> {
        let peek = self.peek();
        tracing::info!("Found :: {:#?}", peek);
        match peek {
            Some('/') => {
                self.take();
                let buf = self.take_until('\n');
                tracing::info!("Found valid comment :: {}", buf);
                let tok = Ok(Tok::Comment(buf));
                tok
            }
            Some('=') => {
                self.take();
                Ok(Tok::DivEq)
            }
            _ => Ok(Tok::Slash),
        }
    }

    fn take(&mut self) -> Option<char> {
        if self.cx.ix >= self.chars.len() {
            return None;
        }
        let ch = self.chars[self.cx.ix];
        if ch == '\n' || ch == '\r' {
            self.cx.line += 1;
            self.cx.col = 0;
            self.cx.fchar = true;
        } else {
            if self.cx.fchar {
                self.cx.fchar = false
            } else {
                self.cx.col += 1;
            }
        }
        self.cx.ix += 1;
        Some(ch)
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

    fn take_while(&mut self, f: impl Fn(Tok) -> bool) -> String {
        let mut buf = format!("{}", self.cx.ch);
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
        if self.cx.ix >= self.chars.len() {
            return None;
        }
        Some(self.chars[self.cx.ix])
    }

    /// Peeks the nth character ahead
    /// 0 is actually the next character
    /// 1 is the second next character
    /// And so on
    ///
    /// Since ix is initally 0, peeking 0 will return the first character
    /// peeking 1 will return the second character. From that ix will always
    /// be increased when taking a character, so the offset continues
    fn peek_nth(&mut self, n: usize) -> Option<char> {
        if self.cx.ix + n >= self.chars.len() {
            return None;
        }
        Some(self.chars[self.cx.ix + n])
    }

    fn skip_ws(&mut self) {
        while let Some(tok) = self.peek().map(Tok::try_from) {
            match tok {
                Ok(Tok::Ws) | Ok(Tok::Nl) | Ok(Tok::Tab) => {
                    self.take();
                }
                _ => break,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use super::*;

    macro_rules! lex_test {
        ($name:ident, $txt:expr, $toks:expr) => {
            #[traced_test]
            #[test]
            fn $name() {
                tracing::info!("Testing lexing of :: {}", $txt);
                let mut lexer = Lexer::new($txt);
                let toks = lexer.lex();
                assert_eq!(toks, $toks);
            }
        };
    }

    lex_test!(test_empty, "", vec![]);
    lex_test!(test_ws, " \t\n\r", vec![]);
    lex_test!(
        test_comment,
        "// Hello, world!",
        vec![Tok::Comment(" Hello, world!".into())]
    );
    lex_test!(
        test_comment_ws,
        "// Hello, world!\n",
        vec![Tok::Comment(" Hello, world!".into())]
    );

    #[traced_test]
    #[test]
    fn test_symbols() {
        let txt = "+-*/()[]{};:,= ><&|^~!@#?_.\\ += -= *= /= == != >= <= << >> ! & | ^ ~ ";
        let mut lexer = Lexer::new(txt);
        let toks = lexer.lex();
        assert_eq!(
            toks,
            vec![
                Tok::Plus,
                Tok::Minus,
                Tok::Star,
                Tok::Slash,
                Tok::LParen,
                Tok::RParen,
                Tok::LSquare,
                Tok::RSquare,
                Tok::LBrace,
                Tok::RBrace,
                Tok::SemiColon,
                Tok::Colon,
                Tok::Comma,
                Tok::Eq,
                Tok::Gt,
                Tok::Lt,
                Tok::And,
                Tok::Or,
                Tok::Caret,
                Tok::Tilde,
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
                Tok::Bang,
                Tok::And,
                Tok::Or,
                Tok::Caret,
                Tok::Tilde,
            ]
        );
    }

    #[traced_test]
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

    #[traced_test]
    #[test]
    fn test_str_char() {
        let txt = r#"'a' 'abc' "abc""#;
        let mut lexer = Lexer::new(txt);
        let toks = lexer.lex();
        assert_eq!(
            toks,
            vec![
                Tok::LitChar('a'),
                Tok::LitString {
                    buf: "abc".to_string(),
                    size: 3
                }
            ]
        );
        assert_eq!(
            lexer.errs,
            vec![LexError::InvalidCharLiteral { line: 0, col: 8 }]
        );
    }

    #[traced_test]
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
                Tok::Ident("___".to_string()),
            ]
        );
    }

    #[traced_test]
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
                Tok::LitInt {
                    buf: "123".to_string(),
                    size: 3,
                    base: IntBase::Dec
                },
                Tok::LitInt {
                    buf: "123".to_string(),
                    size: 3,
                    base: IntBase::Dec
                },
                Tok::Ident("abc".to_string()),
                Tok::LitInt {
                    buf: "123123".to_string(),
                    size: 6,
                    base: IntBase::Dec
                }
            ]
        );
    }

    #[traced_test]
    #[test]
    fn test_floats() {
        let txt = r#"
        123.123 123... 123.hello 123.0.hello
        "#;
        let mut lexer = Lexer::new(txt);
        let toks = lexer.lex();
        assert_eq!(
            toks,
            vec![
                Tok::LitFloat {
                    buf: "123.123".to_string(),
                    size: 7
                },
                Tok::LitInt {
                    buf: "123".to_string(),
                    size: 3,
                    base: IntBase::Dec
                },
                Tok::RangeEx,
                Tok::Dot,
                Tok::LitInt {
                    buf: "123".to_string(),
                    size: 3,
                    base: IntBase::Dec
                },
                Tok::Dot,
                Tok::Ident("hello".to_string()),
                Tok::LitFloat {
                    buf: "123.0".to_string(),
                    size: 5
                },
                Tok::Dot,
                Tok::Ident("hello".to_string()),
            ]
        );
    }

    lex_test!(
        test_range,
        "1..2",
        vec![
            Tok::LitInt {
                buf: "1".to_string(),
                size: 1,
                base: IntBase::Dec
            },
            Tok::RangeEx,
            Tok::LitInt {
                buf: "2".to_string(),
                size: 1,
                base: IntBase::Dec
            }
        ]
    );
}
