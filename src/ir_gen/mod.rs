use std::{collections::HashMap, thread::scope};

use koopa::ir::{Program, Value};

use gen::Generate;
use scope::Scope;

use crate::ast::CompUnit;

mod gen;
mod scope;
mod eval;

pub fn generate_program(comp_unit: &CompUnit) -> Program {
    let mut program = Program::new();
    
    let insts = Vec::<Value>::new();
    let mut scope = Scope::new(None, insts, HashMap::new());

    comp_unit.generate(&mut program, &mut scope);
    program
}