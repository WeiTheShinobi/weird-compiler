#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

#[derive(Debug)]
pub struct Block {
    pub stmt: Stmt,
}

#[derive(Debug)]
pub enum Stmt {
    Return(Exp),
}

#[derive(Debug)]
pub enum Exp {
    UnaryExp(UnaryExp),
}

#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    UnaryOp(UnaryOp, Box<UnaryExp>),
}

#[derive(Debug)]
pub enum PrimaryExp {
    Expression(Box<Exp>),
    Number(i32),
}

#[derive(Debug)]
pub enum UnaryOp {
    Add,
    Minus,
    Not,
}

#[derive(Debug)]
pub struct Number {
    pub num: i32,
}