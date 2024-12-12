
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Inst {
    Beqz(String, String),
    Bnez(String, String),
    J(String),
    Call(String),
    Ret,
    Lw(String, String),
    Sw(String, String),
    Add(String, String, String),
    Addi(String, String, i32),
    Sub(String, String, String),
    Slt(String, String, String),
    Sgt(String, String, String),
    Seqz(String, String),
    Snez(String, String),
    Xor(String, String, String),
    Or(String, String, String),
    And(String, String, String),
    Sll(String, String, String),
    Srl(String, String, String),
    Sra(String, String, String),
    Mul(String, String, String),
    Div(String, String, String),
    Rem(String, String, String),
    Li(String, i32),
    La(String, String),
    Mv(String, String),
    NewLine,
    // example: .global var
    Directive(String),
    Comment(String),
    // example: main:
    Lable(String),
}

impl Inst {
    pub fn to_isa(self) -> String {
        match self {
            Inst::Beqz(a, b) => format!("  beqz {}, {}", a, b),
            Inst::Bnez(a, b) => format!("  bnez {}, {}", a, b),
            Inst::J(pos) => format!("  j {}", pos),
            Inst::Call(func) => format!("  call {}", func),
            Inst::Ret => format!("  ret"),
            Inst::Lw(a, b) => format!("  lw {}, {}", a,b ),
            Inst::Sw(a, b) =>format!("  sw {}, {}", a,b ),
            Inst::Add(a, b, c) => format!("  add {}, {}, {}", a, b, c),
            Inst::Addi(a, b, c) => format!("  addi {}, {}, {}", a, b, c),
            Inst::Sub(a, b, c) => format!("  sub {}, {}, {}", a, b, c),
            Inst::Slt(a, b, c) => format!("  slt {}, {}, {}", a, b, c),
            Inst::Sgt(a, b, c) => format!("  sgt {}, {}, {}", a, b, c),
            Inst::Seqz(a, b) => format!("  seqz {}, {}", a, b),
            Inst::Snez(a, b) =>format!("  snez {}, {}", a, b),
            Inst::Xor(a, b, c) => format!("  xor {}, {}, {}", a, b, c),
            Inst::Or(a, b, c) => format!("  or {}, {}, {}", a, b, c),
            Inst::And(a, b, c) => format!("  and {}, {}, {}", a, b, c),
            Inst::Sll(a, b, c) => format!("  sll {}, {}, {}", a, b, c),
            Inst::Srl(a, b, c) => format!("  srl {}, {}, {}", a, b, c),
            Inst::Sra(a, b, c) => format!("  sra {}, {}, {}", a, b, c),
            Inst::Mul(a, b, c) => format!("  mul {}, {}, {}", a, b, c),
            Inst::Div(a, b, c) => format!("  div {}, {}, {}", a, b, c),
            Inst::Rem(a, b, c) => format!("  rem {}, {}, {}", a, b, c),
            Inst::Li(a, b) => format!("  li {}, {}", a, b),
            Inst::La(a, b) => format!("  la {}, {}", a, b),
            Inst::Mv(a, b) => format!("  mv {}, {}", a, b),
            Inst::NewLine => "".to_string(),
            Inst::Directive(s) => s,
            Inst::Comment(s) => s,
            Inst::Lable(f) => f,
        }
    }
}
