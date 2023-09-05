use crate::{
    ast::{BinaryOp, BuiltIn, Decl, Expr, Stmt, Type, UnaryOp},
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
        while let Ok(decl) = self.parse_decl() {
            decls.push(Stmt::Decl(decl));
        }
        decls
    }

    fn parse_decl(&mut self) -> Result<Decl, ParseError> {
        match self.lx.lookahead() {
            Ok(Tok::Let) => self.parse_let(),
            Ok(Tok::Fn) => self.parse_fn(),
            _ => Err(ParseError::InvalidDeclToken {
                line: self.lx.cx.line,
                col: self.lx.cx.col,
            }),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        let tok = self.lx.next_token()?;

        let pot_lhs = match tok {
            Tok::LitInt { buf, size, base } => Ok(Expr::LitInt { buf, size, base }),
            Tok::LitFloat { buf, size } => Ok(Expr::LitFloat { buf, size }),
            Tok::LitString { buf, size } => Ok(Expr::LitString { buf, size }),
            Tok::LitChar(c) => Ok(Expr::LitChar(c)),
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
                return Err(ParseError::ExpectedExpr {
                    expr: "expression".to_string(),
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
                });
            }
        }?;
        tracing::info!("Found potential lhs :: {:?}", pot_lhs);

        match pot_lhs {
            Expr::Var(_)
            | Expr::LitInt { .. }
            | Expr::LitBool(_)
            | Expr::LitFloat { .. }
            | Expr::LitChar(_)
            | Expr::LitString { .. }
            | Expr::FnCall { .. } => {
                tracing::info!("Parsing binary expr");
                tracing::info!("Found binary lhs :: {:?}", pot_lhs);
                let binop = match BinaryOp::from(self.lx.lookahead()?) {
                    BinaryOp::Invalid => return Ok(pot_lhs),
                    _ => BinaryOp::from(self.lx.next_token()?),
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
                        line: self.lx.cx.line,
                        col: self.lx.cx.col,
                    });
                }
            },
            Expr::Unary { ref op, .. } => match op {
                UnaryOp::Not => {}
                _ => {
                    return Err(ParseError::ExpectedExpr {
                        expr: "boolean".to_string(),
                        line: self.lx.cx.line,
                        col: self.lx.cx.col,
                    })
                }
            },
            Expr::FnCall { .. } => {}
            _ => {
                return Err(ParseError::ExpectedExpr {
                    expr: "boolean".to_string(),
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
                })
            }
        };
        tracing::info!("Found if condition :: {:?}", cond);

        match self.lx.next_token() {
            Ok(Tok::LBrace) => {}
            _ => {
                tracing::error!("Expected `{{`");
                return Err(ParseError::ExpectedToken {
                    tok: "{".to_string(),
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
                });
            }
        };
        tracing::info!("Found if LBrace");

        let pot_rbrace = self.lx.lookahead()?;

        let then = if let Tok::RBrace = pot_rbrace {
            Expr::Unit
        } else {
            let e = self.parse_expr()?;
            e
        };
        tracing::info!("Parsed if then :: {:?}", then);

        let nxt = self.lx.next_token();
        match nxt {
            Ok(Tok::RBrace) => {}
            _ => {
                tracing::info!("Found if else :: {:?}", nxt);
                tracing::error!("Expected expression");
                return Err(ParseError::ExpectedToken {
                    tok: "}".to_string(),
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
                });
            }
        };
        tracing::info!("Found if RBrace");

        let els = match self.lx.next_token() {
            Ok(Tok::Else) => match self.lx.next_token() {
                Ok(Tok::LBrace) => {
                    tracing::info!("Found else LBrace");
                    let els = match self.parse_expr() {
                        Ok(Expr::Unit) => {
                            return Err(ParseError::ExpectedExpr {
                                expr: "expression".to_string(),
                                line: self.lx.cx.line,
                                col: self.lx.cx.col,
                            });
                        }
                        Ok(e) => e,
                        Err(e) => return Err(e),
                    };

                    match self.lx.next_token() {
                        Ok(Tok::RBrace) => tracing::info!("Found else RBrace"),
                        _ => {
                            return Err(ParseError::ExpectedToken {
                                tok: "}".to_string(),
                                line: self.lx.cx.line,
                                col: self.lx.cx.col,
                            });
                        }
                    };

                    Some(els)
                }
                _ => {
                    return Err(ParseError::ExpectedToken {
                        tok: "{".to_string(),
                        line: self.lx.cx.line,
                        col: self.lx.cx.col,
                    });
                }
            },
            Ok(Tok::LBrace) => {
                return Err(ParseError::ExpectedKeyword {
                    kw: "else".to_string(),
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
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
        match self.lx.next_token() {
            Ok(Tok::Let) => {}
            _ => {
                tracing::error!("Expected `let`");
                return Err(ParseError::ExpectedToken {
                    tok: "let".to_string(),
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
                });
            }
        };

        let name = match self.lx.next_token() {
            Ok(Tok::Ident(s)) => s,
            _ => {
                tracing::error!("Expected identifier");
                return Err(ParseError::ExpectedToken {
                    tok: "identifier".to_string(),
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
                });
            }
        };

        match self.lx.next_token() {
            Ok(Tok::Colon) => {}
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: ":".to_string(),
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
                })
            }
        };

        let ty = match self.lx.next_token() {
            Ok(Tok::Ident(s)) => Type::from(s),
            Ok(Tok::LParen) => {
                let mut types = Vec::new();
                loop {
                    match self.lx.next_token() {
                        Ok(Tok::Ident(s)) => types.push(Type::from(s)),
                        Ok(Tok::RParen) => break,
                        Ok(Tok::Comma) => continue,
                        _ => {
                            return Err(ParseError::ExpectedToken {
                                tok: ")".to_string(),
                                line: self.lx.cx.line,
                                col: self.lx.cx.col,
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
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
                })
            }
        };

        match self.lx.next_token() {
            Ok(Tok::Eq) => {}
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: "=".to_string(),
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
                })
            }
        };

        let val = self.parse_expr()?;
        tracing::info!("Found let val :: {:?}", val);

        match self.lx.next_token() {
            Ok(Tok::SemiColon) => {}
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: ";".to_string(),
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
                })
            }
        };

        Ok(Decl::Var { name, ty, val })
    }

    fn parse_fn(&mut self) -> Result<Decl, ParseError> {
        tracing::info!("Parsing fn decl");

        match self.lx.next_token() {
            Ok(Tok::Fn) => {}
            _ => {
                tracing::error!("Expected `fn`");
                return Err(ParseError::ExpectedToken {
                    tok: "fn".to_string(),
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
                });
            }
        };

        let name = match self.lx.next_token() {
            Ok(Tok::Ident(s)) => s,
            _ => {
                tracing::error!("Expected identifier");
                return Err(ParseError::ExpectedToken {
                    tok: "identifier".to_string(),
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
                });
            }
        };

        match self.lx.next_token() {
            Ok(Tok::LParen) => {}
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: "(".to_string(),
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
                })
            }
        };

        let mut args = Vec::new();
        loop {
            match self.lx.next_token() {
                Ok(Tok::RParen) => break,
                Ok(Tok::Comma) => continue,
                Ok(Tok::Ident(s)) => {
                    match self.lx.next_token() {
                        Ok(Tok::Colon) => {}
                        _ => {
                            return Err(ParseError::ExpectedToken {
                                tok: ":".to_string(),
                                line: self.lx.cx.line,
                                col: self.lx.cx.col,
                            })
                        }
                    };

                    let ty = match self.lx.next_token() {
                        Ok(Tok::Ident(s)) => Type::from(s),
                        _ => {
                            return Err(ParseError::ExpectedToken {
                                tok: "identifier".to_string(),
                                line: self.lx.cx.line,
                                col: self.lx.cx.col,
                            })
                        }
                    };

                    args.push((s, ty));
                }
                _ => {
                    return Err(ParseError::ExpectedToken {
                        tok: "identifier".to_string(),
                        line: self.lx.cx.line,
                        col: self.lx.cx.col,
                    })
                }
            };
        }

        match self.lx.next_token() {
            Ok(Tok::Arrow) => {}
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: "->".to_string(),
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
                })
            }
        };

        let ret = match self.lx.next_token() {
            Ok(Tok::Ident(s)) => Type::from(s),
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: "identifier".to_string(),
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
                })
            }
        };

        match self.lx.next_token() {
            Ok(Tok::LBrace) => {}
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: "{".to_string(),
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
                })
            }
        };

        let body = self.parse_expr()?;
        tracing::info!("Found fn body :: {:?}", body);

        match self.lx.next_token() {
            Ok(Tok::RBrace) => {}
            _ => {
                return Err(ParseError::ExpectedToken {
                    tok: "}".to_string(),
                    line: self.lx.cx.line,
                    col: self.lx.cx.col,
                })
            }
        };

        Ok(Decl::Fn {
            name,
            args,
            ret,
            body,
        })
    }

    fn parse_ident(&mut self, ident: String) -> Result<Expr, ParseError> {
        tracing::info!("Parsing ident");

        match self.lx.next_token() {
            Ok(Tok::LParen) => {
                let mut args = Vec::new();
                loop {
                    match self.lx.next_token() {
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

    use crate::ast::{IntBase, IntKind};

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
                let decl = parser.parse_decl();
                assert!(decl.is_ok());
                let decl = decl.unwrap();
                assert_eq!(decl, $expected);
            }
        };
        (Decl, must, $( $name:ident: $src:expr, $expected:expr; )*) => {
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
                let decl = parser.parse_decl();
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
        i_lit: "5", Expr::LitInt{ buf: "5".to_string(), size: 1, base: IntBase::Dec };
        f_lit: "5.0", Expr::LitFloat{ buf: "5.0".to_string(), size: 3 };
        string_lit: "\"hello\"", Expr::LitString{ buf: "hello".to_string(), size: 5 };
        char_lit: "'c'", Expr::LitChar('c');
        true_lit: "true", Expr::LitBool(true);
        false_lit: "false", Expr::LitBool(false);
        if_expr: "if true { 5 } else { 6 }", Expr::If {
            cond: Box::new(Expr::LitBool(true)),
            then: Box::new(Expr::LitInt{ buf: "5".to_string(), size: 1, base: IntBase::Dec }),
            els: Some(Box::new(Expr::LitInt{ buf: "6".to_string(), size: 1, base: IntBase::Dec })),
        };
        if_expr_no_else: "if true { 5 }", Expr::If {
            cond: Box::new(Expr::LitBool(true)),
            then: Box::new(Expr::LitInt{ buf: "5".to_string(), size: 1, base: IntBase::Dec }),
            els: None,
        };
        if_no_then: "if true { } else { 6 }", Expr::If {
            cond: Box::new(Expr::LitBool(true)),
            then: Box::new(Expr::Unit),
            els: Some(Box::new(Expr::LitInt{ buf: "6".to_string(), size: 1, base: IntBase::Dec })),
        };
        if_expr_with_cmp: "if 5 == 5 { 'a' } else { 'b' }", Expr::If {
            cond: Box::new(Expr::Binary {
                op: BinaryOp::Eq,
                lhs: Box::new(Expr::LitInt{ buf: "5".to_string(), size: 1, base: IntBase::Dec }),
                rhs: Box::new(Expr::LitInt{ buf: "5".to_string(), size: 1, base: IntBase::Dec }),
            }),
            then: Box::new(Expr::LitChar('a')),
            els: Some(Box::new(Expr::LitChar('b'))),
        };
        if_expr_with_cmp_and_unary: "if !5 == 5 { 5 } else { 6 }", Expr::If {
            cond: Box::new(Expr::Unary {
                op: UnaryOp::Not,
                expr: Box::new(Expr::Binary {
                    op: BinaryOp::Eq,
                    lhs: Box::new(Expr::LitInt{ buf: "5".to_string(), size: 1, base: IntBase::Dec }),
                    rhs: Box::new(Expr::LitInt{ buf: "5".to_string(), size: 1, base: IntBase::Dec }),
                }),
            }),
            then: Box::new(Expr::LitInt{ buf: "5".to_string(), size: 1, base: IntBase::Dec }),
            els: Some(Box::new(Expr::LitInt{ buf: "6".to_string(), size: 1, base: IntBase::Dec })),
        };
        if_expr_with_fn_call: "if foo() { 5 } else { 6 }", Expr::If {
            cond: Box::new(Expr::FnCall { name: "foo".to_string(), args: Vec::new() }),
            then: Box::new(Expr::LitInt{ buf: "5".to_string(), size: 1, base: IntBase::Dec }),
            els: Some(Box::new(Expr::LitInt{ buf: "6".to_string(), size: 1, base: IntBase::Dec })),
        };
        if_expr_with_fn_call_and_cmp: "if foo() == 5 { 5 } else { 6 }", Expr::If {
            cond: Box::new(Expr::Binary {
                op: BinaryOp::Eq,
                lhs: Box::new(Expr::FnCall {
                    name: "foo".to_string(),
                    args: Vec::new(),
                }),
                rhs: Box::new(Expr::LitInt{ buf: "5".to_string(), size: 1, base: IntBase::Dec }),
            }),
            then: Box::new(Expr::LitInt{ buf: "5".to_string(), size: 1, base: IntBase::Dec }),
            els: Some(Box::new(Expr::LitInt{ buf: "6".to_string(), size: 1, base: IntBase::Dec })),
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
                rhs: Box::new(Expr::LitInt{ buf: "5".to_string(), size: 1, base: IntBase::Dec }),
            })}),
            then: Box::new(Expr::LitInt{ buf: "5".to_string(), size: 1, base: IntBase::Dec }),
            els: Some(Box::new(Expr::LitInt{ buf: "6".to_string(), size: 1, base: IntBase::Dec })),
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
        fail_invalid_expr_add: "if 5 + 1 { 5 }", ParseError::ExpectedExpr {
            expr: "boolean".to_string(),
            line: 0,
            col: 9,
        };
        fail_invalid_expr_sub: "if 5 - 1 { 5 }", ParseError::ExpectedExpr {
            expr: "boolean".to_string(),
            line: 0,
            col: 9,
        };
        fail_invalid_expr_str: "if \"hello\" { 5 }", ParseError::ExpectedExpr {
            expr: "boolean".to_string(),
            line: 0,
            col: 11,
        };
        fail_invalid_expr_int: "if 1 { 5 }", ParseError::ExpectedExpr {
            expr: "boolean".to_string(),
            line: 0,
            col: 5,
        };
        fail_invalid_expr_float: "if 1.0 { 5 }", ParseError::ExpectedExpr {
            expr: "boolean".to_string(),
            line: 0,
            col: 7,
        };
    );
    parse_test!(
        Decl, must,
        let_decl: "let x: i32 = 5;", Decl::Var {
            name: "x".to_string(),
            ty: Type::BuiltIn(BuiltIn::Int(IntKind::I32)),
            val: Expr::LitInt{ buf: "5".to_string(), size: 1, base: IntBase::Dec },
        };
        let_decl_add: "let x: i32 = 5 + 5;", Decl::Var {
            name: "x".to_string(),
            ty: Type::BuiltIn(BuiltIn::Int(IntKind::I32)),
            val: Expr::Binary {
                op: BinaryOp::Add,
                lhs: Box::new(Expr::LitInt{ buf: "5".to_string(), size: 1, base: IntBase::Dec }),
                rhs: Box::new(Expr::LitInt{ buf: "5".to_string(), size: 1, base: IntBase::Dec }),
            },
        };
        fn_decl: "fn foo() -> i32 { 5 }", Decl::Fn {
            name: "foo".to_string(),
            args: Vec::new(),
            ret: Type::BuiltIn(BuiltIn::Int(IntKind::I32)),
            body: Expr::LitInt{ buf: "5".to_string(), size: 1, base: IntBase::Dec },
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
