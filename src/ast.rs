use crate::token::{Tok, TokKind};

pub struct Ast {
    pub decls: Vec<Decl>,
}

pub enum AstKind {
    Decl {
        vis: Visibility,
        time: Time,
        decl: Decl,
    },
    Expr,
    Tok(TokKind),
}

pub enum Visibility {
    Pub,
    Priv,
}

pub enum Time {
    Comptime,
    Runtime,
}

#[derive(Debug, PartialEq)]
pub enum Decl {
    Fn {
        name: String,
        args: Vec<(String, Type)>,
        ret: Type,
        body: Expr,
    },
    Asm(),
    Var {
        name: String,
        ty: Type,
        val: Expr,
    },
    Struct(),
    Trait(),
    Impl(),
    Use(),
    Package(),
    Enum(),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    LitInt {
        buf: String,
        size: usize,
        base: IntBase,
    },
    LitFloat {
        buf: String,
        size: usize,
    },
    LitString {
        buf: String,
        size: usize,
    },
    LitChar(char),
    LitBool(bool),
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        els: Option<Box<Expr>>,
    },
    Range {
        start: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
        inclusive: bool,
    },
    Match,
    // expr: Box<Expr>,
    // arms: Vec<MatchArm>,
    For,
    Block {
        // stmts: Vec<Stmt>,
        expr: Option<Box<Expr>>,
    },
    FnCall {
        name: String,
        args: Vec<Expr>,
    },
    Var(String),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unit,
}

pub enum Stmt {
    Expr(Expr),
    Decl(Decl),
}

pub struct SrcLoc {
    pub file: String,
    pub line: u32,
    pub col: u32,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Gt,
    Lt,
    Geq,
    Leq,
    Lsl,
    Lsr,
    Invalid,
}

impl From<Tok> for BinaryOp {
    fn from(value: Tok) -> Self {
        match value {
            Tok::Plus => BinaryOp::Add,
            Tok::Minus => BinaryOp::Sub,
            Tok::Star => BinaryOp::Mul,
            Tok::Slash => BinaryOp::Div,
            Tok::Percent => BinaryOp::Mod,
            Tok::Deq => BinaryOp::Eq,
            Tok::Neq => BinaryOp::Neq,
            Tok::Gt => BinaryOp::Gt,
            Tok::Lt => BinaryOp::Lt,
            Tok::Geq => BinaryOp::Geq,
            Tok::Leq => BinaryOp::Leq,
            Tok::Lsl => BinaryOp::Lsl,
            Tok::Lsr => BinaryOp::Lsr,
            _ => BinaryOp::Invalid,
        }
    }
}

impl From<CmpOp> for BinaryOp {
    fn from(value: CmpOp) -> Self {
        match value {
            CmpOp::Eq => BinaryOp::Eq,
            CmpOp::Neq => BinaryOp::Neq,
            CmpOp::Gt => BinaryOp::Gt,
            CmpOp::Lt => BinaryOp::Lt,
            CmpOp::Geq => BinaryOp::Geq,
            CmpOp::Leq => BinaryOp::Leq,
            _ => BinaryOp::Invalid,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum CmpOp {
    Eq,
    Neq,
    Gt,
    Lt,
    Geq,
    Leq,
    Invalid,
}

impl From<Tok> for CmpOp {
    fn from(value: Tok) -> Self {
        tracing::info!("Converting {:?} to CmpOp", value);
        match value {
            Tok::Deq => CmpOp::Eq,
            Tok::Neq => CmpOp::Neq,
            Tok::Gt => CmpOp::Gt,
            Tok::Lt => CmpOp::Lt,
            Tok::Geq => CmpOp::Geq,
            Tok::Leq => CmpOp::Leq,
            _ => CmpOp::Invalid,
        }
    }
}

impl From<Option<Tok>> for CmpOp {
    fn from(value: Option<Tok>) -> Self {
        match value {
            Some(Tok::Deq) => CmpOp::Eq,
            Some(Tok::Neq) => CmpOp::Neq,
            Some(Tok::Gt) => CmpOp::Gt,
            Some(Tok::Lt) => CmpOp::Lt,
            Some(Tok::Geq) => CmpOp::Geq,
            Some(Tok::Leq) => CmpOp::Leq,
            _ => CmpOp::Invalid,
        }
    }
}

impl From<(Tok, Tok)> for CmpOp {
    fn from(value: (Tok, Tok)) -> Self {
        match value {
            (Tok::Eq, Tok::Eq) => CmpOp::Eq,
            (Tok::Neq, Tok::Eq) => CmpOp::Neq,
            (Tok::Gt, Tok::Eq) => CmpOp::Geq,
            (Tok::Lt, Tok::Eq) => CmpOp::Leq,
            _ => CmpOp::Invalid,
        }
    }
}

impl From<(Option<Tok>, Option<Tok>)> for CmpOp {
    fn from(value: (Option<Tok>, Option<Tok>)) -> Self {
        match value {
            (Some(Tok::Eq), Some(Tok::Eq)) => CmpOp::Eq,
            (Some(Tok::Neq), Some(Tok::Eq)) => CmpOp::Neq,
            (Some(Tok::Gt), Some(Tok::Eq)) => CmpOp::Geq,
            (Some(Tok::Lt), Some(Tok::Eq)) => CmpOp::Leq,
            (Some(Tok::Gt), None) => CmpOp::Gt,
            (Some(Tok::Lt), None) => CmpOp::Lt,
            _ => CmpOp::Invalid,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
    BAnd,
    BOr,
    BXor,
    BNot,
}

impl TryFrom<&Tok> for UnaryOp {
    type Error = ();

    fn try_from(value: &Tok) -> Result<Self, Self::Error> {
        match value {
            Tok::Minus => Ok(UnaryOp::Neg),
            Tok::Bang => Ok(UnaryOp::Not),
            Tok::And => Ok(UnaryOp::BAnd),
            Tok::Or => Ok(UnaryOp::BOr),
            Tok::Caret => Ok(UnaryOp::BXor),
            Tok::Tilde => Ok(UnaryOp::BNot),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum IntKind {
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
}

#[derive(Debug, PartialEq)]
pub enum IntBase {
    Bin,
    Oct,
    Dec,
    Hex,
}

impl From<char> for IntBase {
    fn from(c: char) -> Self {
        match c {
            'b' => IntBase::Bin,
            'o' => IntBase::Oct,
            'x' => IntBase::Hex,
            _ => IntBase::Dec,
        }
    }
}

impl std::fmt::Display for IntBase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntBase::Bin => write!(f, "0b"),
            IntBase::Oct => write!(f, "0o"),
            IntBase::Dec => write!(f, ""),
            IntBase::Hex => write!(f, "0x"),
        }
    }
}

impl From<String> for IntBase {
    fn from(s: String) -> Self {
        match s.as_str() {
            "0b" => IntBase::Bin,
            "0o" => IntBase::Oct,
            "0x" => IntBase::Hex,
            _ => IntBase::Dec,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum FloatKind {
    F32,
    F64,
}

#[derive(Debug, PartialEq)]
pub enum Type {
    BuiltIn(BuiltIn),
    Custom(String),
}

#[derive(Debug, PartialEq)]
pub enum BuiltIn {
    Int(IntKind),
    Float(FloatKind),
    Bool,
    Char,
    Unit,
    // Array,
    // Slice,
    Tuple(Vec<Type>),
    Str,
    // Ptr,
}

impl From<String> for Type {
    fn from(s: String) -> Self {
        match s.as_str() {
            "i8" => Type::BuiltIn(BuiltIn::Int(IntKind::I8)),
            "i16" => Type::BuiltIn(BuiltIn::Int(IntKind::I16)),
            "i32" => Type::BuiltIn(BuiltIn::Int(IntKind::I32)),
            "i64" => Type::BuiltIn(BuiltIn::Int(IntKind::I64)),
            "i128" => Type::BuiltIn(BuiltIn::Int(IntKind::I128)),
            "u8" => Type::BuiltIn(BuiltIn::Int(IntKind::U8)),
            "u16" => Type::BuiltIn(BuiltIn::Int(IntKind::U16)),
            "u32" => Type::BuiltIn(BuiltIn::Int(IntKind::U32)),
            "u64" => Type::BuiltIn(BuiltIn::Int(IntKind::U64)),
            "u128" => Type::BuiltIn(BuiltIn::Int(IntKind::U128)),
            "f32" => Type::BuiltIn(BuiltIn::Float(FloatKind::F32)),
            "f64" => Type::BuiltIn(BuiltIn::Float(FloatKind::F64)),
            "bool" => Type::BuiltIn(BuiltIn::Bool),
            "char" => Type::BuiltIn(BuiltIn::Char),
            "str" => Type::BuiltIn(BuiltIn::Str),
            _ => Type::Custom(s),
        }
    }
}
