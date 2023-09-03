use crate::{
    ast::{BinaryOp, BuiltIn, CmpOp, Decl, Expr, Stmt, Type, UnaryOp},
    err::ParseError,
    token::Tok,
};

pub struct Parser {
    lx: crate::lex::Lexer,
    _errs: Vec<crate::err::ParseError>,
}

impl Parser {
    pub fn new(lx: crate::lex::Lexer) -> Parser {
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

        if let Err(_) = tok {
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
        let tok = self.lx.next_tok()?;

        let pot_lhs = match tok {
            Tok::Scalar(i) => match i.contains('.') {
                true => Ok(Expr::LitFloat(i)), // Todo ! check if valid float
                false => Ok(Expr::LitInt(i)),
            },
            Tok::String(s) => Ok(Expr::LitString(s)),
            Tok::Char(c) => Ok(Expr::LitChar(c)),
            Tok::True => Ok(Expr::LitBool(true)),
            Tok::False => Ok(Expr::LitBool(false)),
            Tok::If => self.parse_if(),
            Tok::Ident(i) => self.parse_ident(i),
            _ if UnaryOp::try_from(&tok).is_ok() => {
                let expr = self.parse_expr()?;
                Ok(Expr::Unary {
                    op: UnaryOp::try_from(&tok).unwrap(),
                    expr: Box::new(expr),
                })
            }
            _ => {
                tracing::error!("Expected expression");
                return Err(ParseError::ExpectedExpr {
                    expr: "expression".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                });
            }
        }?;
        tracing::info!("Found pot lhs :: {:?}", pot_lhs);

        match pot_lhs {
            Expr::Var(_)
            | Expr::LitInt(_)
            | Expr::LitBool(_)
            | Expr::LitFloat(_)
            | Expr::LitChar(_)
            | Expr::LitString(_)
            | Expr::FnCall { .. } => {
                tracing::info!("Parsing binary expr");
                tracing::info!("Found binary lhs :: {:?}", pot_lhs);
                let (v1, v2) = self.lx.peek_n_no_ws::<2>().splat();
                tracing::info!("Found binary peek :: {:?}", (&v1));

                let op = CmpOp::from((v1, v2));
                tracing::info!("Found cmp op :: {:?}", op);
                let binop = match op {
                    CmpOp::Invalid => return Ok(pot_lhs),
                    _ => self.lx.next_tok().map(BinaryOp::from)?,
                };
                tracing::info!("Found binary op :: {:?}", binop);
                let rhs = self.parse_expr()?;
                tracing::info!("Found binary rhs :: {:?}", rhs);

                Ok(Expr::Binary {
                    op: binop,
                    lhs: Box::new(pot_lhs),
                    rhs: Box::new(rhs),
                })
            }
            _ => Ok(pot_lhs),
        }
    }

    fn parse_if(&mut self) -> Result<Expr, ParseError> {
        tracing::info!("Parsing if expr");
        let cond = self.parse_expr()?;

        match cond {
            Expr::LitBool(_) => {}
            Expr::Binary { ref op, .. } => match op {
                BinaryOp::Eq
                | BinaryOp::Neq
                | BinaryOp::Gt
                | BinaryOp::Lt
                | BinaryOp::Geq
                | BinaryOp::Leq => tracing::info!("Found valid comparison operator"),
                _ => {
                    tracing::error!("Expected comparison operator");
                    return Err(ParseError::ExpectedExpr {
                        expr: "boolean".to_string(),
                        line: self.lx.line,
                        col: self.lx.col,
                    });
                }
            },
            Expr::Unary { ref op, .. } => match op {
                UnaryOp::Not => {}
                _ => {
                    return Err(ParseError::ExpectedExpr {
                        expr: "boolean".to_string(),
                        line: self.lx.line,
                        col: self.lx.col,
                    })
                }
            },
            Expr::FnCall { .. } => {}
            _ => {
                return Err(ParseError::ExpectedExpr {
                    expr: "boolean".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                })
            }
        };
        tracing::info!("Found if condition :: {:?}", cond);

        match self.lx.next_tok() {
            Ok(Tok::LBrace) => {}
            _ => {
                tracing::error!("Expected `{{`");
                return Err(ParseError::ExpectedToken {
                    tok: "{".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                });
            }
        };
        tracing::info!("Found if LBrace");

        let (pot_rbrace,) = self.lx.peek_n_no_ws::<1>().splat();

        let then = if let Some(Tok::RBrace) = pot_rbrace {
            Expr::Unit
        } else {
            let e = self.parse_expr()?;
            e
        };
        tracing::info!("Parsed if then :: {:?}", then);

        let nxt = self.lx.next_tok();
        match nxt {
            Ok(Tok::RBrace) => {}
            _ => {
                tracing::info!("Found if else :: {:?}", nxt);
                tracing::error!("Expected expression");
                return Err(ParseError::ExpectedToken {
                    tok: "}".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                });
            }
        };
        tracing::info!("Found if RBrace");

        let els = match self.lx.next_tok() {
            Ok(Tok::Else) => match self.lx.next_tok() {
                Ok(Tok::LBrace) => {
                    tracing::info!("Found else LBrace");
                    let els = match self.parse_expr() {
                        Ok(Expr::Unit) => {
                            return Err(ParseError::ExpectedExpr {
                                expr: "expression".to_string(),
                                line: self.lx.line,
                                col: self.lx.col,
                            });
                        }
                        Ok(e) => e,
                        Err(e) => return Err(e),
                    };

                    match self.lx.next_tok() {
                        Ok(Tok::RBrace) => tracing::info!("Found else RBrace"),
                        _ => {
                            return Err(ParseError::ExpectedToken {
                                tok: "}".to_string(),
                                line: self.lx.line,
                                col: self.lx.col,
                            });
                        }
                    };

                    Some(els)
                }
                _ => {
                    return Err(ParseError::ExpectedToken {
                        tok: "{".to_string(),
                        line: self.lx.line,
                        col: self.lx.col,
                    });
                }
            },
            Ok(Tok::LBrace) => {
                return Err(ParseError::ExpectedKeyword {
                    kw: "else".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                });
            }
            _ => None,
        };
        tracing::info!("Found if else :: {:?}", els);

        Ok(Expr::If {
            cond: Box::new(cond),
            then: Box::new(then),
            els: match els {
                Some(e) => Some(Box::new(e)),
                None => None,
            },
        })
    }

    fn parse_let(&mut self) -> Result<Decl, ParseError> {
        tracing::info!("Parsing let decl");
        match self.lx.next_tok() {
            Ok(Tok::Let) => {}
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
            Ok(Tok::Ident(s)) => s,
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
            Ok(Tok::Colon) => {}
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: ":".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                })
            }
        };

        let ty = match self.lx.next_tok() {
            Ok(Tok::Ident(s)) => Type::from(s),
            Ok(Tok::LParen) => {
                let mut types = Vec::new();
                loop {
                    match self.lx.next_tok() {
                        Ok(Tok::Ident(s)) => types.push(Type::from(s)),
                        Ok(Tok::RParen) => break,
                        Ok(Tok::Comma) => continue,
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
            Ok(Tok::Eq) => {}
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: "=".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                })
            }
        };

        let val = self.parse_expr()?;
        tracing::info!("Found let val :: {:?}", val);

        match self.lx.next_tok() {
            Ok(Tok::SemiColon) => {}
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: ";".to_string(),
                    line: self.lx.line,
                    col: self.lx.col,
                })
            }
        };

        Ok(Decl::Var { name, ty, val })
    }

    fn parse_ident(&mut self, ident: String) -> Result<Expr, ParseError> {
        tracing::info!("Parsing ident");

        match self.lx.next_tok() {
            Ok(Tok::LParen) => {
                let mut args = Vec::new();
                loop {
                    match self.lx.next_tok() {
                        Ok(Tok::RParen) => break,
                        Ok(Tok::Comma) => continue,
                        _ => {
                            args.push(self.parse_expr()?);
                        }
                    };
                }
                Ok(Expr::FnCall { name: ident, args })
            }
            _ => Ok(Expr::Var(ident)),
        }
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use crate::ast::IntKind;

    use super::*;

    macro_rules! parse_test {
        (Expr, must, $name:ident, $src:expr, $expected:expr) => {
            #[traced_test]
            #[test]
            fn $name() {
                tracing::info!("Testing expr :: {}", $src);
                let lx = crate::lex::Lexer::new($src);
                let mut parser = Parser::new(lx);
                let expr = parser.parse_expr();
                assert!(expr.is_ok());
                let expr = expr.unwrap();
                assert_eq!(expr, $expected);
            }
        };
        (Expr, must, $( $name:ident: $src:expr, $expected:expr; )*) => {
            $(
                parse_test!(Expr, must, $name, $src, $expected);
            )*
        };
        (Expr, fail, $name:ident, $src:expr, $expected:expr) => {
            #[traced_test]
            #[test]
            fn $name() {
                tracing::info!("Testing expr :: {}", $src);
                let lx = crate::lex::Lexer::new($src);
                let mut parser = Parser::new(lx);
                let expr = parser.parse_expr();
                assert!(expr.is_err());
                let expr = expr.unwrap_err();
                assert_eq!(expr, $expected);
            }
        };
        (Expr, fail, $( $name:ident: $src:expr, $expected:expr; )*) => {
            $(
                parse_test!(Expr, fail, $name, $src, $expected);
            )*
        };


        (Decl, must, $name:ident, $src:expr, $expected:expr) => {
            #[traced_test]
            #[test]
            fn $name() {
                tracing::info!("Testing decl :: {}", $src);
                let lx = crate::lex::Lexer::new($src);
                let mut parser = Parser::new(lx);
                let decl = parser.parse_let();
                assert!(decl.is_ok());
                let decl = decl.unwrap();
                assert_eq!(decl, $expected);
            }
        };
        (Decl, $( must, $name:ident: $src:expr, $expected:expr; )*) => {
            $(
                parse_test!(Decl, must, $name, $src, $expected);
            )*
        };
        (Decl, fail, $name:ident, $src:expr, $expected:expr) => {
            #[traced_test]
            #[test]
            fn $name() {
                tracing::info!("Testing decl :: {}", $src);
                let lx = crate::lex::Lexer::new($src);
                let mut parser = Parser::new(lx);
                let decl = parser.parse_let();
                assert!(decl.is_err());
                let decl = decl.unwrap_err();
                assert_eq!(decl, $expected);
            }
        };
        (Decl, fail, $( $name:ident: $src:expr, $expected:expr; )*) => {
            $(
                parse_test!(Decl, fail, $name, $src, $expected);
            )*
        };
    }

    parse_test!(
        Expr, must,
        i_lit: "5", Expr::LitInt("5".to_string());
        f_lit: "5.0", Expr::LitFloat("5.0".to_string());
        string_lit: "\"hello\"", Expr::LitString("hello".to_string());
        char_lit: "'c'", Expr::LitChar("c".to_string());
        true_lit: "true", Expr::LitBool(true);
        false_lit: "false", Expr::LitBool(false);
        if_expr: "if true { 5 } else { 6 }", Expr::If {
            cond: Box::new(Expr::LitBool(true)),
            then: Box::new(Expr::LitInt("5".to_string())),
            els: Some(Box::new(Expr::LitInt("6".to_string()))),
        };
        if_expr_no_else: "if true { 5 }", Expr::If {
            cond: Box::new(Expr::LitBool(true)),
            then: Box::new(Expr::LitInt("5".to_string())),
            els: None,
        };
        if_no_then: "if true { } else { 6 }", Expr::If {
            cond: Box::new(Expr::LitBool(true)),
            then: Box::new(Expr::Unit),
            els: Some(Box::new(Expr::LitInt("6".to_string()))),
        };
        if_expr_with_cmp: "if 5 == 5 { 'a' } else { 'b' }", Expr::If {
            cond: Box::new(Expr::Binary {
                op: BinaryOp::Eq,
                lhs: Box::new(Expr::LitInt("5".to_string())),
                rhs: Box::new(Expr::LitInt("5".to_string())),
            }),
            then: Box::new(Expr::LitChar("a".to_string())),
            els: Some(Box::new(Expr::LitChar("b".to_string()))),
        };
        if_expr_with_cmp_and_unary: "if !5 == 5 { 5 } else { 6 }", Expr::If {
            cond: Box::new(Expr::Unary {
                op: UnaryOp::Not,
                expr: Box::new(Expr::Binary {
                    op: BinaryOp::Eq,
                    lhs: Box::new(Expr::LitInt("5".to_string())),
                    rhs: Box::new(Expr::LitInt("5".to_string())),
                }),
            }),
            then: Box::new(Expr::LitInt("5".to_string())),
            els: Some(Box::new(Expr::LitInt("6".to_string()))),
        };
        if_expr_with_fn_call: "if foo() { 5 } else { 6 }", Expr::If {
            cond: Box::new(Expr::FnCall { name: "foo".to_string(), args: Vec::new() }),
            then: Box::new(Expr::LitInt("5".to_string())),
            els: Some(Box::new(Expr::LitInt("6".to_string()))),
        };
        if_expr_with_fn_call_and_cmp: "if foo() == 5 { 5 } else { 6 }", Expr::If {
            cond: Box::new(Expr::Binary {
                op: BinaryOp::Eq,
                lhs: Box::new(Expr::FnCall {
                    name: "foo".to_string(),
                    args: Vec::new(),
                }),
                rhs: Box::new(Expr::LitInt("5".to_string())),
            }),
            then: Box::new(Expr::LitInt("5".to_string())),
            els: Some(Box::new(Expr::LitInt("6".to_string()))),
        };
        if_expr_with_fn_call_and_cmp_and_unary: "if !foo() == 5 { 5 } else { 6 }", Expr::If {
            cond: Box::new(Expr::Unary {
                op: UnaryOp::Not,
                expr: Box::new(Expr::Binary {
                op: BinaryOp::Eq,
                lhs: Box::new(Expr::FnCall {
                    name: "foo".to_string(),
                    args: Vec::new(),
                }),
                rhs: Box::new(Expr::LitInt("5".to_string())),
            })}),
            then: Box::new(Expr::LitInt("5".to_string())),
            els: Some(Box::new(Expr::LitInt("6".to_string()))),
        };
    );
    parse_test!(
        Expr, fail,
        fail_if_no_cond: "if { 5 } else { 6 }", ParseError::ExpectedExpr {
            expr: "expression".to_string(),
            line: 0,
            col: 3,
        };
        fail_if_no_lbrace: "if true 5 } else { 6 }", ParseError::ExpectedToken {
            tok: "{".to_string(),
            line: 0,
            col: 8,
        };
        fail_if_no_rbrace: "if true { 5 else { 6 }", ParseError::ExpectedToken {
            tok: "}".to_string(),
            line: 0,
            col: 15, // Ending of the `else` keyword, not sure if this is requires fixing
        };
        fail_if_no_else_kw: "if true { 5 } { 6 }", ParseError::ExpectedKeyword {
            kw: "else".to_string(),
            line: 0,
            col: 14,
        };
        fail_if_no_else: "if true { 5 } else { }", ParseError::ExpectedExpr {
            expr: "expression".to_string(),
            line: 0,
            col: 21,
        };
        fail_if_no_else_lbrace: "if true { 5 } else 6 }", ParseError::ExpectedToken {
            tok: "{".to_string(),
            line: 0,
            col: 19,
        };
        fail_if_no_else_rbrace: "if true { 5 } else { 6 ", ParseError::ExpectedToken {
            tok: "}".to_string(),
            line: 0,
            col: 22,
        };
    );
    parse_test!(
        Decl, must,
        let_decl: "let x: i32 = 5;", Decl::Var {
            name: "x".to_string(),
            ty: Type::BuiltIn(BuiltIn::Int(IntKind::I32)),
            val: Expr::LitInt("5".to_string()),
        };
    );
    parse_test!(
        Decl, fail,
        fail_let_decl: "let x: i32 = 5", ParseError::ExpectedToken {
            tok: ";".to_string(),
            line: 0,
            col: 13,
        };
        fail_let_no_colon: "let x = 5;", ParseError::ExpectedToken {
            tok: ":".to_string(),
            line: 0,
            col: 6,
        };
        fail_let_no_type: "let x: = 5;", ParseError::ExpectedToken {
            tok: "identifier".to_string(),
            line: 0,
            col: 7,
        };
        fail_let_no_eq: "let x: i32 5;", ParseError::ExpectedToken {
            tok: "=".to_string(),
            line: 0,
            col: 11,
        };
        fail_let_no_val: "let x: i32 = ;", ParseError::ExpectedExpr {
            expr: "expression".to_string(),
            line: 0,
            col: 13,
        };
    );
}
