use crate::{
    ast::{BuiltIn, Decl, Expr, Type},
    err::ParseError,
    token::Tok,
};

pub struct Parser<'a> {
    lx: crate::lexer::Lexer<'a>,
    _errs: Vec<crate::err::ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(lx: crate::lexer::Lexer<'a>) -> Parser<'a> {
        Parser {
            lx,
            _errs: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Vec<Decl> {
        let mut decls = Vec::new();
        while let Some(decl) = self.next_decl() {
            decls.push(decl);
        }
        decls
    }

    fn next_decl(&mut self) -> Option<Decl> {
        let tok = self.lx.next_tok();

        if let None = tok {
            return None;
        }
        let tok = tok.unwrap();

        let _ = match tok {
            Tok::Let => self.parse_let(),
            _ => todo!(),
        };

        todo!()
    }

    fn parse_let(&mut self) -> Result<Decl, ParseError> {
        let name = match self.lx.next_tok() {
            Some(Tok::Ident(s)) => s,
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: "identifier".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                })
            }
        };

        match self.lx.next_tok() {
            Some(Tok::Colon) => {}
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: ":".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                })
            }
        };

        let ty = match self.lx.next_tok() {
            Some(Tok::Ident(s)) => Type::from(s),
            Some(Tok::LParen) => {
                let mut types = Vec::new();
                loop {
                    match self.lx.next_tok() {
                        Some(Tok::Ident(s)) => types.push(Type::from(s)),
                        Some(Tok::RParen) => break,
                        Some(Tok::Comma) => continue,
                        _ => {
                            return Err(ParseError::ExpectedToken {
                                tok: ")".to_string(),
                                line: self.lx.line,
                                col: self.lx.col,
                            })
                        }
                    };
                }
                if types.len() == 0 {
                    Type::BuiltIn(BuiltIn::Unit)
                } else {
                    Type::BuiltIn(BuiltIn::Tuple(types))
                }
            }
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: "identifier".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                })
            }
        };

        match self.lx.next_tok() {
            Some(Tok::Eq) => {}
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: "=".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                })
            }
        };

        let val = self.parse_expr();

        Ok(Decl::Var { name, ty, val })
    }

    fn parse_expr(&mut self) -> Expr {
        match self.lx.next_tok() {
            Some(Tok::Scalar(i)) => Expr::Int(i),
            _ => Expr::Unit,
        }
    }
}
