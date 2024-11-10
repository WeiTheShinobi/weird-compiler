use entities::ValueData;
use koopa::ir::{self, *};
use std::{
    collections::HashMap, fs::File, io::Write, vec,
};
use std::fmt::{format, Pointer};

pub struct Program {
    asm: Vec<String>,
    writer: File,
}

impl Program {
    pub fn new(file: File) -> Program {
        Program { asm: vec![], writer: file }
    }

    fn write(&mut self, inst: &str) {
        self.writer.write_all(inst.as_bytes());
        self.writer.write_all("\n".as_bytes());
    }
}

#[derive(Clone, Debug)]
struct AsmValue {
    name: String,
    offset: usize,
    is_pointer: bool,
}
struct Context {
    counter: u32,
    stack_pointer: usize,
    symbol_table: HashMap<Value, AsmValue>,
}

impl Context {
    fn new() -> Context {
        Context {
            counter: 1,
            stack_pointer: 0,
            symbol_table: HashMap::new(),
        }
    }

    fn get_useful_space(&mut self) -> String {
        let counter = self.counter.to_string();
        self.counter += 1;
        format!("t{}", counter)
    }

    fn get_symbol(&self, key: &Value) -> AsmValue {
        self.symbol_table.get(key).unwrap().clone()
    }
}

pub trait GenerateAsm {
    fn generate(&self, asm: &mut Program);
}

impl GenerateAsm for ir::Program {
    fn generate(&self, program: &mut Program) {
        program.write("  .text");
        program.write("  .globl main");
        for &func in self.func_layout() {
            let func_data = self.func(func);
            let func_name = if func_data.name().starts_with("@") {
                func_data.name().strip_prefix("@").unwrap()
            } else {
                func_data.name()
            };
            program.write(format!("{}:", func_name).as_str());
            func_data.generate(program);
        }
    }
}

impl GenerateAsm for FunctionData {
    fn generate(&self, program: &mut Program) {
        let mut cx = Context::new();
        let stack_size = calculate_stack_size(self);

        prologue(program, stack_size);
        for (&_bb, node) in self.layout().bbs() {
            for &value in node.insts().keys() {
                let value_data = self.dfg().value(value);
                dbg!(value_data);
                match value_data.kind() {
                    ValueKind::Integer(int) => {
                        let reg = cx.get_useful_space();
                        let immediate = int.value().to_string();
                        program.write(format!("  li {}, {}", reg, immediate).as_str());
                        // reg
                    }
                    ValueKind::Binary(binary) => {}
                    ValueKind::Alloc(alloc) => {
                        cx.symbol_table.insert(value, AsmValue {
                            name: value_data.name().clone().unwrap(),
                            offset: cx.stack_pointer,
                            is_pointer: true,
                        });
                        cx.stack_pointer += value_data.ty().size()
                    }
                    ValueKind::Load(load) => {
                        let load = cx.symbol_table.get(&load.src()).unwrap();
                        program.write(format!("  lw t0, sp({})", load.offset).as_str());
                        program.write(format!("  sw t0, sp({}) ", cx.stack_pointer).as_str());
                        cx.stack_pointer += value_data.ty().size()
                    }
                    ValueKind::Store(store) => {
                        dbg!(value_data);
                        dbg!(self.dfg().value(store.value()));
                        let value = cx.get_symbol(&store.value());
                        let dest = cx.get_symbol(&store.dest());

                        program.write(format!("  lw t0, sp({})", value.offset).as_str());
                        program.write(format!("  sw t0, sp({})", dest.offset).as_str());
                    }
                    ValueKind::Return(ret) => {
                        if let Some(ret_val) = ret.value() {
                            let ret_val_data = cx.get_symbol(&ret_val);
                            program.write(format!("lw a0, sp({})", ret_val_data.offset).as_str());
                        }
                        epilogue(program, stack_size);
                        program.write("  ret");
                    }
                    _ => unimplemented!("{:?}", value_data),
                }
                // emit(self, value_data, program, &mut cx);
            }
        }
    }
}

fn calculate_stack_size(function_data: &FunctionData) -> usize {
    let mut size = 0;
    for (&_bb, node) in function_data.layout().bbs() {
        for &inst in node.insts().keys() {
            let value_data = function_data.dfg().value(inst);
            match value_data.kind() {
                ValueKind::Alloc(_) =>
                    size += value_data.ty().size(),
                ValueKind::Load(_) =>
                    size += value_data.ty().size(),
                _ => (),
            }
        }
    }
    if size % 16 == 0 {
        size
    } else {
        size + 16 - (size % 16)
    }
}
fn prologue(program: &mut Program, stack_size: usize) {
    program.write("  # prologue");
    // addi -> 2^12 [-2048, 2047]
    if stack_size < 2047 {
        program.write(format!("  addi sp sp -{}", stack_size).as_str());
    } else {
        program.write(format!("  li t0 -{}", stack_size).as_str());
        program.write("  add sp sp t0");
    }
    program.write("");
}

fn epilogue(program: &mut Program, stack_size: usize) {
    program.write("  # epilogue");
    // addi -> 2^12 [-2048, 2047]
    if stack_size < 2047 {
        program.write(format!("  addi sp sp {}", stack_size).as_str());
    } else {
        program.write(format!("  li t0 {}", stack_size).as_str());
        program.write("  add sp sp t0");
    }
    program.write("");
}

fn emit(func_data: &FunctionData, v: &ValueData, program: &mut Program, cx: &mut Context) -> String {
    match v.kind() {
        ValueKind::Integer(int) => {
            let reg = cx.get_useful_space();
            let immediate = int.value().to_string();
            program.write(format!("  li {}, {}", reg, immediate).as_str());
            reg
        }
        ValueKind::Binary(binary) => {
            let lhs_value = func_data.dfg().value(binary.lhs());
            let rhs_value = func_data.dfg().value(binary.rhs());
            let lhs_value = emit(func_data, lhs_value, program, cx);
            let rhs_value = emit(func_data, rhs_value, program, cx);

            match binary.op() {
                BinaryOp::Eq => {
                    program.write(format!("  sub {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str());
                    program.write(format!("  seqz {}, {}", rhs_value, rhs_value).as_str());
                    rhs_value
                }
                BinaryOp::NotEq => {
                    program.write(format!("  sub {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str());
                    program.write(format!("  snez {}, {}", rhs_value, rhs_value).as_str());
                    rhs_value
                }
                BinaryOp::Sub => {
                    program.write(format!("  sub {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str());
                    rhs_value
                }
                BinaryOp::Mul => {
                    program.write(format!("  mul {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str());
                    rhs_value
                }
                BinaryOp::Add => {
                    program.write(format!("  add {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str());
                    rhs_value
                }
                BinaryOp::Gt => {
                    program.write(format!("  sgt {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str());
                    program.write(format!("  seqz {}, {}", rhs_value, rhs_value).as_str());
                    rhs_value
                }
                BinaryOp::Lt => {
                    program.write(format!("  slt {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str());
                    program.write(format!("  seqz {}, {}", rhs_value, rhs_value).as_str());
                    rhs_value
                }
                BinaryOp::Ge => {
                    program.write(format!("  slt {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str());
                    program.write(format!("  seqz {}, {}", rhs_value, rhs_value).as_str());
                    rhs_value
                }
                BinaryOp::Le => {
                    program.write(format!("  sgt {}, {}, {}", rhs_value, lhs_value, rhs_value).as_str());
                    program.write(format!("  seqz {}, {}", rhs_value, rhs_value).as_str());
                    rhs_value
                }
                _ => unimplemented!("op: {}", binary.op()),
            }
        }
        ValueKind::Return(ret) => {
            let value_data = func_data.dfg().value(ret.value().unwrap());
            let return_data = emit(func_data, value_data, program, cx);
            if return_data != "a0".to_string() {
                program.write(format!("  mv a0, {}", return_data).as_str());
            }
            program.write("  ret".into());
            String::new()
        }

        ValueKind::Load(load) => {
            let source = func_data.dfg().value(load.src());
            let size = source.ty().size();
            "".to_string()
        }
        ValueKind::Store(store) => { "".to_string() }
        _ => unimplemented!("{:?}", v)
    }
}

