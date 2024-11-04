use std::env::args;
use std::fmt;

use std::fs::{read_to_string, File};

use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use std::io;
use std::process::{exit, Output};

mod ast;
mod ir_gen;
mod riscv_gen;

lalrpop_mod!(sysy);
fn main() {
    let mut args = args();
    args.next();
    let mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    if let Err(err) = try_main(Args {
        mode,
        input,
        output,
    }) {
        eprintln!("{}", err);
        exit(-1);
    }
}

#[derive(Debug)]
struct Args {
    mode: String,
    input: String,
    output: String,
}
impl Args {
    fn new(mode: String, input: String, output: String) -> Args {
        Args {
            mode,
            input,
            output,
        }
    }
}
fn try_main(args: Args) -> Result<(), Error> {
    let input = read_to_string(args.input).map_err(Error::File)?;
    let ast = sysy::CompUnitParser::new()
        .parse(&input)
        .map_err(|_| Error::Parse)?;

    match args.mode.as_str() {
        "-koopa" => {
            let output_file = File::create(args.output).map_err(Error::File)?;
            let program = ir_gen::generate_program(&ast);
            KoopaGenerator::new(output_file)
                .generate_on(&program)
                .unwrap();
        }
        "-riscv" => {
            let output_file = File::create(args.output).map_err(Error::File)?;
            let koopa = ir_gen::generate_program(&ast);
            riscv_gen::generate_riscv(koopa, output_file);
        }
        _ => {
            unreachable!("unsupport mode: {}", args.mode);
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

#[cfg(test)]
mod test {

    use std::{
        fs::{self},
        path::PathBuf,
    };

    use crate::{try_main, Args};

    #[test]
    fn gen() {
        let mut file_names = Vec::new();
        let path = "./tests/input";
        for entry in fs::read_dir(path).unwrap() {
            let entry = entry.unwrap();
            if let Some(file_name) = entry.file_name().to_str() {
                file_names.push(file_name.to_string());
            }
        }

        let output = "./tests/output";
        if !PathBuf::from(output).exists() {
            fs::create_dir(output).unwrap();
        }
        for file_name in file_names {
            println!("start compile {}", &file_name);
            let args = Args::new(
                "-koopa".to_string(),
                format!("{}{}", "./tests/input/", file_name),
                format!(
                    "{}{}{}",
                    "./tests/output/",
                    file_name.split('.').next().unwrap(),
                    ".koopa"
                ),
            );
            if let Err(e) = try_main(args) {
                dbg!(e.to_string());
            }

            let args = Args::new(
                "-riscv".to_string(),
                format!("{}{}", "./tests/input/", file_name),
                format!(
                    "{}{}{}",
                    "./tests/output/",
                    file_name.split('.').next().unwrap(),
                    ".riscv"
                ),
            );
            if let Err(e) = try_main(args) {
                dbg!(e.to_string());
            }
        }
    }
}
