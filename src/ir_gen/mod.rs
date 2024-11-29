use std::vec;

use koopa::ir::{FunctionData, Program, Type};

use gen::Generate;
use scope::Scope;

use crate::ast::CompUnit;
use crate::ir_gen::scope::Global;

mod eval;
mod gen;
mod scope;

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

    init_bulidin_func(&mut program, &mut scope);

    comp_unit.generate(&mut program, &mut scope)?;
    Ok(program)
}

fn init_bulidin_func(program: &mut Program, scope: &mut Scope) {
    let getint = program.new_func(FunctionData::new_decl(
        "@getint".to_string(),
        vec![],
        Type::get_i32(),
    ));
    scope.global.function.insert("getint", getint);

    let getch = program.new_func(FunctionData::new_decl(
        "@getch".to_string(),
        vec![],
        Type::get_i32(),
    ));
    scope.global.function.insert("getch", getch);

    let getarray = program.new_func(FunctionData::new_decl(
        "@getarray".to_string(),
        vec![Type::get_pointer(Type::get_i32())],
        Type::get_i32(),
    ));
    scope.global.function.insert("getarray", getarray);

    let getarray = program.new_func(FunctionData::new_decl(
        "@getarray".to_string(),
        vec![Type::get_pointer(Type::get_i32())],
        Type::get_i32(),
    ));
    scope.global.function.insert("getarray", getarray);

    let putint = program.new_func(FunctionData::new_decl(
        "@putint".to_string(),
        vec![Type::get_i32()],
        Type::get_unit(),
    ));
    scope.global.function.insert("putint", putint);

    let putch = program.new_func(FunctionData::new_decl(
        "@putch".to_string(),
        vec![Type::get_i32()],
        Type::get_unit(),
    ));
    scope.global.function.insert("putch", putch);

    let putarray = program.new_func(FunctionData::new_decl(
        "@putarray".to_string(),
        vec![Type::get_i32(),Type::get_pointer(Type::get_i32())],
        Type::get_unit(),
    ));
    scope.global.function.insert("putarray", putarray);

    let starttime = program.new_func(FunctionData::new_decl(
        "@starttime".to_string(),
        vec![],
        Type::get_unit(),
    ));
    scope.global.function.insert("starttime", starttime);

    let stoptime = program.new_func(FunctionData::new_decl(
        "@stoptime".to_string(),
        vec![],
        Type::get_unit(),
    ));
    scope.global.function.insert("stoptime", stoptime);
}
