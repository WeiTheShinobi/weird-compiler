use std::collections::HashMap;

use super::{
    inst::Inst,
    Program,
};

pub fn peephole(mut program: Program) -> Program {
    let mut memo = HashMap::<String, String>::new();
    for i in 0..program.insts.len() {
        let inst = program.insts[i].clone();
        match inst {
            Inst::Beqz(_, _)
            | Inst::Bnez(_, _)
            | Inst::J(_)
            | Inst::Call(_)
            | Inst::Ret
            | Inst::Lable(_)
            | Inst::Directive(_) => memo.clear(),
            Inst::Sw(rs, rd) => {
                memo.insert(rs, rd);
            }
            Inst::Lw(rs, rd) => {
                if let Some(last_rd) = memo.get(&rs) {
                    if *last_rd == rd {
                        program.insts[i] = Inst::NewLine;
                    }
                }
            }
            Inst::Add(rs, _, _)
            | Inst::Addi(rs, _, _)
            | Inst::Sub(rs, _, _)
            | Inst::Slt(rs, _, _)
            | Inst::Sgt(rs, _, _)
            | Inst::Seqz(rs, _)
            | Inst::Snez(rs, _)
            | Inst::Xor(rs, _, _)
            | Inst::Or(rs, _, _)
            | Inst::And(rs, _, _)
            | Inst::Sll(rs, _, _)
            | Inst::Srl(rs, _, _)
            | Inst::Sra(rs, _, _)
            | Inst::Mul(rs, _, _)
            | Inst::Div(rs, _, _)
            | Inst::Rem(rs, _, _)
            | Inst::Li(rs, _)
            | Inst::La(rs, _)
            | Inst::Mv(rs, _) => {
                memo.remove(&rs);
            }
            _ => {}
        }
    }
    let v: Vec<_> = program
        .insts
        .iter()
        .filter(|&inst| !matches!(inst, Inst::NewLine))
        .cloned()
        .collect();
    program.insts = v;
    program
}

#[cfg(test)]
mod test {
    use std::cmp::max;

    use crate::riscv_gen::{inst::Inst, Program};

    use super::peephole;

    fn print_inst(result: &Vec<Inst>, wanted: &Vec<Inst>) {
        println!("=============");
        for i in 0..max(result.len(), wanted.len()) {
            println!("inst: {:?}", i);
            println!("result: {:?}", result[i]);

            if wanted.len() > i {
                println!("wanted: {:?}", wanted[i]);
            }
            println!("=============");
        }
    }

    fn run(name: &str, case: Program, wanted: Vec<Inst>) {
        println!("{}", name);
        let result = peephole(case);
        print_inst(&result.insts, &wanted);

        assert_eq!(result.insts.len(), wanted.len());
        result
            .insts
            .iter()
            .zip(wanted)
            .for_each(|(result, wanted)| {
                assert_eq!(*result, wanted);
            });
        println!("");
    }

    #[test]
    fn test_peephole() {
        run(
            "1",
            Program {
                insts: vec![
                    Inst::Sw("t0".to_string(), "0(sp)".to_string()),
                    Inst::Lw("t0".to_string(), "0(sp)".to_string()),
                ],
            },
            vec![Inst::Sw("t0".to_string(), "0(sp)".to_string())],
        );
        run(
            "2",
            Program {
                insts: vec![
                    Inst::Sw("t0".to_string(), "0(sp)".to_string()),
                    Inst::Add("t0".to_string(), "t1".to_string(), "t2".to_string()),
                    Inst::Lw("t0".to_string(), "0(sp)".to_string()),
                ],
            },
            vec![
                Inst::Sw("t0".to_string(), "0(sp)".to_string()),
                Inst::Add("t0".to_string(), "t1".to_string(), "t2".to_string()),
                Inst::Lw("t0".to_string(), "0(sp)".to_string()),
            ],
        );
    }
}
