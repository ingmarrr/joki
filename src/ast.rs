use crate::token::TokKind;

pub struct Ast {
    pub decls: Vec<Decl>,
}

pub enum AstKind {
    Decl,
    Expr,
    Tok(TokKind),
}

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

pub enum Expr {
    Int(String),
    Float,
    String,
    Char,
    If,
    Match,
    For,
    Block,
    FnCall,
    Unary,
    Binary,
    Unit,
}

pub struct SrcLoc {
    pub file: String,
    pub line: u32,
    pub col: u32,
}

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

pub enum UnaryOp {
    Neg,
    Not,
    BAnd,
    BOr,
    BXor,
    BNot,
}

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

pub enum FloatKind {
    F32,
    F64,
}

pub enum Type {
    BuiltIn(BuiltIn),
    Custom(String),
}

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
