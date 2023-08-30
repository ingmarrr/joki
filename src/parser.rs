use crate::{
    ast::{BinaryOp, BuiltIn, Decl, Expr, Stmt, Type, UnaryOp},
    err::ParseError,
    token::Tok,
};

pub struct Parser<'a> {
    lx: crate::lex::Lexer<'a>,
    _errs: Vec<crate::err::ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(lx: crate::lex::Lexer<'a>) -> Parser<'a> {
        Parser {
            lx,
            _errs: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut decls = Vec::new();
        while let Some(decl) = self.next_decl() {
            decls.push(Stmt::Decl(decl));
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

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        let tok = self.lx.next_tok().ok_or(ParseError::UnexpectedEOF {
            line: self.lx.line,
            col: self.lx.col,
        })?;
        match tok {
            Tok::Scalar(i) => match i.contains('.') {
                true => Ok(Expr::LitFloat(i)), // Todo ! check if valid float
                false => Ok(Expr::LitInt(i)),
            },
            Tok::String(s) => Ok(Expr::LitString(s)),
            Tok::Char(c) => Ok(Expr::LitChar(c)),
            Tok::True => Ok(Expr::LitBool(true)),
            Tok::False => Ok(Expr::LitBool(false)),
            Tok::If => self.parse_if(),
            _ => Ok(Expr::Unit),
        }
    }

    fn parse_if(&mut self) -> Result<Expr, ParseError> {
        tracing::info!("Parsing if expr");
        let cond = self.parse_expr();

        match cond {
            Ok(Expr::LitBool(_)) => {}
            Ok(Expr::Binary { ref op, .. }) => match op {
                BinaryOp::Eq
                | BinaryOp::Neq
                | BinaryOp::Gt
                | BinaryOp::Lt
                | BinaryOp::Geq
                | BinaryOp::Leq => {}
                _ => {
                    return Err(ParseError::ExpectedExpr {
                        expr: "boolean".to_string(),
                        line: self.lx.line,
                        col: self.lx.col,
                    })
                }
            },
            Ok(Expr::Unary { ref op, .. }) => match op {
                UnaryOp::Not => {}
                _ => {
                    return Err(ParseError::ExpectedExpr {
                        expr: "boolean".to_string(),
                        line: self.lx.line,
                        col: self.lx.col,
                    })
                }
            },
            Ok(Expr::FnCall) => {}
            _ => {
                return Err(ParseError::ExpectedExpr {
                    expr: "boolean".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                })
            }
        };
        tracing::info!("Parsed if condition");

        match self.lx.next_tok() {
            Some(Tok::LBrace) => {}
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: "{".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                })
            }
        };

        let then = self.parse_expr();

        match self.lx.next_tok() {
            Some(Tok::RBrace) => {}
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: "}".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                })
            }
        };

        let els = match self.lx.next_tok() {
            Some(Tok::Else) => match self.lx.next_tok() {
                Some(Tok::LBrace) => {
                    let els = match self.parse_expr() {
                        Ok(e) => e,
                        Err(e) => return Err(e),
                    };

                    match self.lx.next_tok() {
                        Some(Tok::RBrace) => {}
                        _ => {
                            return Err(ParseError::ExpectedToken {
                                tok: "}".to_string(),
                                line: self.lx.line,
                                col: self.lx.col,
                            })
                        }
                    };

                    Some(els)
                }
                _ => {
                    return Err(ParseError::ExpectedToken {
                        tok: "{".to_string(),
                        line: self.lx.line,
                        col: self.lx.col,
                    })
                }
            },
            _ => None,
        };

        Ok(Expr::If {
            cond: Box::new(cond.unwrap()),
            then: Box::new(then.unwrap()),
            els: match els {
                Some(e) => Some(Box::new(e)),
                None => None,
            },
        })
    }

    fn parse_let(&mut self) -> Result<Decl, ParseError> {
        tracing::info!("Parsing let decl");
        match self.lx.next_tok() {
            Some(Tok::Let) => {}
            _ => {
                tracing::error!("Expected `let`");
                return Err(ParseError::ExpectedToken {
                    tok: "let".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                });
            }
        };

        let name = match self.lx.next_tok() {
            Some(Tok::Ident(s)) => s,
            _ => {
                tracing::error!("Expected identifier");
                return Err(ParseError::ExpectedToken {
                    tok: "identifier".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                });
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

        let val = self.parse_expr()?;

        match self.lx.next_tok() {
            Some(Tok::Semi) => {}
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: ";".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                })
            }
        };

        Ok(Stmt::Decl(Decl::Var { name, ty, val }))
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use crate::ast::IntKind;

    use super::*;

    #[traced_test]
    #[test]
    fn test_parse_let() {
        let src = "let x: i32 = 5;";
        let lx = crate::lex::Lexer::new(src);
        let mut parser = Parser::new(lx);
        let decl = parser.parse_let();
        assert!(decl.is_ok());
        let decl = decl.unwrap();
        assert_eq!(
            decl,
            Decl::Var {
                name: "x".to_string(),
                ty: Type::BuiltIn(BuiltIn::Int(IntKind::I32)),
                val: Expr::LitInt("5".to_string()),
            }
        );
    }

    #[traced_test]
    #[test]
    fn test_parse_if() {
        let src = "if true { 5 } else { 6 }";
        let lx = crate::lex::Lexer::new(src);
        let mut parser = Parser::new(lx);
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::If {
                cond: Box::new(Expr::LitBool(true)),
                then: Box::new(Expr::LitInt("5".to_string())),
                els: Some(Box::new(Expr::LitInt("6".to_string()))),
            }
        );
    }
}
