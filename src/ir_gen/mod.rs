use koopa::ir::Program;

use gen::Generate;

use crate::ast::CompUnit;

mod gen;

pub fn generate_program(comp_unit: &CompUnit) -> Program {
    let mut program = Program::new();
    comp_unit.generate(&mut program);
    program
}