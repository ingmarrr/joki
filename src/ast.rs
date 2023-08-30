use crate::token::TokKind;

pub struct Ast {
    pub decls: Vec<Decl>,
}

pub enum AstKind {
    Decl,
    Expr,
    Tok(TokKind),
}

#[derive(Debug, PartialEq)]
pub enum Decl {
    Fn(),
    Asm(),
    Var { name: String, ty: Type, val: Expr },
    Struct(),
    Trait(),
    Impl(),
    Use(),
    Package(),
    Enum(),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    LitInt(String),
    LitFloat(String),
    LitString(String),
    LitChar(String),
    LitBool(bool),
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        els: Option<Box<Expr>>,
    },
    Match,
    // expr: Box<Expr>,
    // arms: Vec<MatchArm>,
    For,
    Block {
        // stmts: Vec<Stmt>,
        expr: Option<Box<Expr>>,
    },
    FnCall,
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
    Eq,
    Neq,
    Gt,
    Lt,
    Geq,
    Leq,
    Lsl,
    Lsr,
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
