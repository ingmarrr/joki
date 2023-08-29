pub enum Stmt {
    Expr {
        expr: Expr,
        semi: bool,
        loc: SrcLoc,
    },
    Let {
        name: String,
        ty: Option<Type>,
        val: Expr,
        loc: SrcLoc,
    },
    Return {
        val: Expr,
        loc: SrcLoc,
    },
}

pub enum Return {
    Expr(Expr),
    Unit,
}

pub enum Expr {
    Ident(String),
    Int {
        val: i64,
        base: u32,
        kind: IntKind,
    },
    Float {
        val: f64,
        kind: FloatKind,
    },
    String(String),
    Char(char),
    Bool(bool),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    Struct(Vec<(String, Expr)>),
    Call {
        name: String,
        args: Vec<Expr>,
    },
    Index {
        name: String,
        index: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    If {
        cond: Box<Expr>,
        then: Box<Expr>,
        els: Option<Box<Expr>>,
    },
    Block {
        stmts: Vec<Stmt>,
    },
    Fn {
        name: String,
        args: Vec<(String, Type)>,
        ret: Type,
        body: Box<Expr>,
    },
    Asm {
        name: String,
        args: Vec<(String, Type)>,
        ret: Type,
        body: Box<Expr>,
    },
    Type {
        name: String,
        fields: Vec<(String, Type)>,
    },
    Trait {
        name: String,
        methods: Vec<(String, Type)>,
    },
    Impl {
        name: String,
        methods: Vec<(String, Type)>,
    },
    For {
        name: String,
        iter: Box<Expr>,
        body: Box<Expr>,
    },
    Match {
        expr: Box<Expr>,
        arms: Vec<(Expr, Expr)>,
    },
    Assign {
        name: String,
        val: Box<Expr>,
    },
    Return {
        val: Box<Expr>,
    },
}

pub struct Type {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

pub struct SrcLoc {
    pub file: String,
    pub pack: String,
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
