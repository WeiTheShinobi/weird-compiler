use std::env::args;
use std::fmt;

use std::fs::{read_to_string, File};

use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use std::io;
use std::process::exit;

mod ast;
mod ir_gen;
mod riscv_gen;

lalrpop_mod!(sysy);
fn main() {
    if let Err(err) = try_main() {
        eprintln!("{}", err);
        exit(-1);
    }
}

fn try_main() -> Result<(), Error> {
    let mut args = args();
    args.next();

    let mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    let input = read_to_string(input).map_err(Error::File)?;
    let ast = sysy::CompUnitParser::new()
        .parse(&input)
        .map_err(|_| Error::Parse)?;

    match mode.as_str() {
        "-koopa" => {
            let output_file = File::create(output).map_err(Error::File)?;
            let program = ir_gen::generate_program(&ast);
            KoopaGenerator::new(output_file)
                .generate_on(&program)
                .unwrap();
        }
        "-riscv" => {
            let output_file = File::create(output).map_err(Error::File)?;
            let koopa = ir_gen::generate_program(&ast);
            riscv_gen::generate_riscv(koopa, output_file);
        }
        _ => {
            println!("unsupport mode: {}", mode);
        }
    }
    Ok(())
}

enum Error {
    File(io::Error),
    Parse,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Parse => write!(f, "error occurred while parsing"),
            Self::File(err) => write!(f, "invalid input SysY file: {}", err),
        }
    }
}
