use koopa::ir::Program;

use gen::Generate;
use scope::Scope;

use crate::ast::CompUnit;
use crate::ir_gen::scope::Global;

mod gen;
mod scope;
mod eval;

#[derive(Debug)]
pub enum Error {
    Error(String),
    ReassignConst(String),
    Redeclare(String),
    NoInLoop,
    Undefined(String),
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn generate_program(comp_unit: &CompUnit) -> Result<Program> {
    let mut program = Program::new();
    let mut scope = Scope::new(None, Global::new(), Vec::new());

    comp_unit.generate(&mut program, &mut scope)?;
    Ok(program)
}