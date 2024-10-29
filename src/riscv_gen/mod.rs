use std::fs::File;

use gen::*;

mod gen;

pub enum Error {
    
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn generate_riscv(program: koopa::ir::Program, file :File) -> Program {
    let mut riscv = gen::Program::new(file);
    program.generate(&mut riscv);
    riscv
}