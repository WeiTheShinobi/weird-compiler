#[derive(Debug)]
pub struct CompUnit {
    pub comp_unit: Box<Option<CompUnit>>,
    pub func_def: FuncDef,
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub params: Vec<FuncFParam>,
    pub ident: String,
    pub block: Block,
}

#[derive(Debug)]
pub struct FuncFParam {
    pub btype: BType,
    pub ident: String,
}

#[derive(Debug)]
pub enum FuncType {
    Int,
    Void,
}

#[derive(Debug)]
pub struct Block {
    pub block_item: Vec<BlockItem>,
}

#[derive(Debug)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

#[derive(Debug)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

#[derive(Debug)]
pub struct VarDecl {
    pub btype: BType,
    pub defs: Vec<VarDef>,
}

#[derive(Debug)]
pub enum VarDef {
    Id(String),
    Assign(String, InitVal),
}

#[derive(Debug)]
pub struct InitVal {
    pub exp: Exp,
}

#[derive(Debug)]
pub struct ConstDecl {
    pub btype: BType,
    pub defs: Vec<ConstDef>,
}

#[derive(Debug, Clone, Copy)]
pub enum BType {
    Int,
}

#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub const_init_val: ConstInitVal,
}

#[derive(Debug)]
pub enum ConstInitVal {
    ConstExp(ConstExp),
}

#[derive(Debug)]
pub enum ConstExp {
    Exp(Exp),
}

#[derive(Debug)]
pub enum Stmt {
    Assign(LVal, Exp),
    Exp(Option<Exp>),
    Block(Block),
    Return(Option<Exp>),
    If(Box<If>),
    While(Box<While>),
    Break,
    Continue,
}

#[derive(Debug)]
pub struct While {
    pub cond: Exp,
    pub body: Stmt,
}

#[derive(Debug)]
pub struct If {
    pub cond: Exp,
    pub if_then: Stmt,
    pub else_then: Option<Stmt>,
}

#[derive(Debug)]
pub enum Exp {
    LOrExp(LOrExp),
}

#[derive(Debug)]
pub enum LOrExp {
    LAndExp(LAndExp),
    LOrExp(Box<LOrExp>, LAndExp),
}

#[derive(Debug)]
pub enum LAndExp {
    EqExp(EqExp),
    LAndExp(Box<LAndExp>, EqExp),
}

#[derive(Debug)]
pub enum EqExp {
    RelExp(RelExp),
    EqExp(Box<EqExp>, EqOp, RelExp),
}

#[derive(Debug)]
pub enum EqOp {
    Eq,
    NotEq,
}

#[derive(Debug)]
pub enum RelExp {
    AddExp(AddExp),
    RelExp(Box<RelExp>, RelOp, AddExp),
}

#[derive(Debug)]
pub enum RelOp {
    Gt,
    Lt,
    Ge,
    Le,
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
    MulAndUnary(Box<MulExp>, MulOp, UnaryExp),
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
    Call(FuncCall),
}

#[derive(Debug)]
pub struct FuncCall {
    pub ident: String,
    pub args: Vec<Exp>,
}

#[derive(Debug)]
pub enum PrimaryExp {
    Expression(Box<Exp>),
    LVal(LVal),
    Number(i32),
}

#[derive(Debug)]
pub struct LVal {
    pub ident: String,
}

#[derive(Debug)]
pub enum UnaryOp {
    Add,
    Minus,
    Not,
}
