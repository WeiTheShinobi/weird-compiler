use entities::ValueData;
use koopa::ir::{self, *};
use std::{
    collections::HashMap, fmt::format, fs::File, io::{self, Write}, vec
};
use crate::riscv_gen::Result;

pub struct Program {
    asm: Vec<String>,
    writer : File,
}

impl Program {
    pub fn new(file :File) -> Program {
        Program { asm: vec![] , writer: file}
    }

    fn write(&mut self, inst: &str) {
        self.writer.write_all(inst.as_bytes());
        self.writer.write_all("\n".as_bytes());
    }
}

struct Context {
    counter:u32,
    symbol_table: HashMap<Value, String>
}

impl Context {
    fn new() -> Context {
        Context{
            counter: 0,
            symbol_table: HashMap::new(),
        }
    }

    fn get_useful_register(&mut self) -> String {
        let counter = self.counter.to_string();
        self.counter += 1;
        format!("t{}", counter)
    }
}

pub trait GenerateAsm {
    fn generate(&self, asm: &mut Program);
}

impl GenerateAsm for ir::Program {
    fn generate(&self, asm: &mut Program) {
        asm.write("  .text");
        asm.write( "  .global");
        for &func in self.func_layout() {
            let func_data = self.func(func);
            asm.write(format!("{}:", func_data.name()).as_str());
            func_data.generate(asm);
        }
    }
}

impl GenerateAsm for FunctionData {
    fn generate(&self, asm: &mut Program) {
        let mut cx = Context::new();
        for (&_bb, node) in self.layout().bbs() {
            let inst = *node.insts().back_key().unwrap();
            let value_data = self.dfg().value(inst);
            dbg!(&value_data);
            emit(self, value_data,asm, &mut cx);
        }
    }
}


fn emit(func_data: &FunctionData, v: &ValueData, asm: &mut Program, cx: &mut Context) -> String {
    match v.kind() {
        ValueKind::Integer(int) => {
            if int.value() == 0 {
                return "x0".to_string();
            }
            let reg = cx.get_useful_register();
            let immediate = int.value().to_string();
            asm.write(format!("  li {}, {}", reg, immediate).as_str());
            reg
        },
        ValueKind::Binary(binary) => {
            let rhs_value = func_data.dfg().value(binary.rhs());
            // dbg!(&rhs_value);
            let lhs_value = func_data.dfg().value(binary.lhs());
            // dbg!(&lhs_value);
            let rhs_value = emit(func_data, rhs_value, asm, cx);
            let lhs_value = emit(func_data, lhs_value, asm, cx);
            
            dbg!(binary.op());
            match binary.op() {
                BinaryOp::Eq => {
                    asm.write(format!("  xor {}, {}, x0", rhs_value, rhs_value).as_str());
                    asm.write(format!("  seqz {}, {}", rhs_value, rhs_value).as_str());
                    rhs_value
                },
                BinaryOp::Sub => {
                    let reg = cx.get_useful_register();
                    asm.write(format!("  sub {}, {}, {}", reg, lhs_value, rhs_value).as_str());
                    reg
                },
                _ => unreachable!("unary op not implement"),
            }
        },
        ValueKind::Return(ret) => {
            let value_data = func_data.dfg().value(ret.value().unwrap());
            dbg!(&value_data);
            let return_data = emit(func_data, value_data, asm, cx);
            if return_data != "a0".to_string() {
                asm.write(format!("  mv a0, {}", return_data).as_str());
            }
            asm.write("  ret".into());
            String::new()
        },
        _ => unreachable!()
    }
}

