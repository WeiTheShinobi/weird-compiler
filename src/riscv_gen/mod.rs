use std::fs::File;

use gen::*;
use crate::riscv_gen::context::Context;

mod gen;
mod context;

#[derive(Debug)]
pub enum Error {
    
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn generate_riscv(program: koopa::ir::Program, file :File) -> Program {
    let mut riscv = gen::Program::new(file);
    let mut cx = Context::new();
    program.generate(&mut riscv, &mut cx);
    riscv
}