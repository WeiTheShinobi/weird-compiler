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
    AddExp(AddExp),
}

#[derive(Debug)]
pub enum AddExp {
    MulExp(MulExp),
    AddAndMul(Box<AddExp>, AddOp, MulExp),
}

#[derive(Debug)]
pub enum AddOp {
    Add,
    Sub,
}

#[derive(Debug)]
pub enum MulExp {
    UnaryExp(UnaryExp),
    MulAndUnary(Box<MulExp>, MulOp, UnaryExp)
}

#[derive(Debug)]
pub enum MulOp {
    Mul,
    Div, 
    Mod,
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
