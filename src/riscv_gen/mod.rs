use gen::*;

mod gen;

pub fn generate_riscv(program: koopa::ir::Program) -> Program {
    let mut riscv = gen::Program::new();
    program.generate(&mut riscv);
    riscv
}