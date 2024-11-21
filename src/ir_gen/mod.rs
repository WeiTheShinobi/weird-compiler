use std::collections::HashMap;

use koopa::ir::{Program, Value};

use gen::Generate;
use scope::Scope;

use crate::ast::CompUnit;

mod gen;
mod scope;
mod eval;

#[derive(Debug)]
pub enum Error {
    Error(String),
    ReassignConst(String),
    Redeclare(String),
    NoInLoop,
    Undefined,
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn generate_program(comp_unit: &CompUnit) -> Result<Program> {
    let mut program = Program::new();
    
    let insts = Vec::<Value>::new();
    let mut scope = Scope::new(None, insts, Vec::new());

    comp_unit.generate(&mut program, &mut scope)?;
    Ok(program)
}