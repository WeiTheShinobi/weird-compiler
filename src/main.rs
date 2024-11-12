use std::env::args;
use std::fmt::{self, write};

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
            let program = ir_gen::generate_program(&ast).map_err(Error::KoopaGen)?;
            KoopaGenerator::new(output_file)
                .generate_on(&program)
                .unwrap();
            Ok(())
        }
        "-riscv" => {
            let output_file = File::create(args.output).map_err(Error::File)?;
            let koopa = ir_gen::generate_program(&ast).map_err(Error::KoopaGen)?;
            riscv_gen::generate_riscv(koopa, output_file);
            Ok(())
        }
        _ => {
            unreachable!("unsupport mode: {}", args.mode);
        }
    }
}

enum Error {
    File(io::Error),
    Parse,
    KoopaGen(ir_gen::Error),
    RiscvGen(riscv_gen::Error),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Parse => write!(f, "error occurred while parsing"),
            Self::File(err) => write!(f, "invalid input SysY file: {}", err),
            Self::KoopaGen(err) => write!(f, "koopa gen error: {:?}", err),
            Self::RiscvGen(err) => write!(f, "gen isa error: {:?}", err),
        }
    }
}

#[cfg(test)]
mod test {
    macro_rules! test_koopa {
        ($file_name: ident) => {
            #[test]
            fn $file_name() {
                let file_name = stringify!($file_name);
                if !PathBuf::from("./tests/output").exists() {
                    fs::create_dir("./tests/output").unwrap();
                }

                let old_koopa = format!("./tests/output/{}{}", file_name, ".koopa");
                if PathBuf::from(&old_koopa).exists() {
                    fs::remove_file(&old_koopa).unwrap();
                }

                println!("start compile {}", &file_name);
                let args = Args::new(
                    "-koopa".to_string(),
                    format!("{}{}{}", "./tests/input/", file_name, ".c"),
                    format!("{}{}{}", "./tests/output/", file_name, ".koopa"),
                );
                println!("koopa {}", file_name);
                if let Err(e) = try_main(args) {
                    panic!("{}", e.to_string());
                }
            }
        };
    }

    macro_rules! test_riscv {
        ($file_name: ident) => {
            #[test]
            fn $file_name() {
                let file_name = stringify!($file_name);
                if !PathBuf::from("./tests/output").exists() {
                    fs::create_dir("./tests/output").unwrap();
                }

                let old_riscv = format!("./tests/output/{}{}", file_name, ".riscv");
                if PathBuf::from(&old_riscv).exists() {
                    fs::remove_file(&old_riscv).unwrap();
                }

                let args = Args::new(
                    "-riscv".to_string(),
                    format!("{}{}{}", "./tests/input/", file_name, ".c"),
                    format!("{}{}{}", "./tests/output/", file_name, ".riscv"),
                );
                println!("riscv {}", file_name);
                if let Err(e) = try_main(args) {
                    panic!("{}", e.to_string());
                }
            }
        };
    }

    mod koopa {
        use crate::{try_main, Args};
        use std::{
            fs::{self},
            path::PathBuf,
        };

        test_koopa!(arithmetic);
        test_koopa!(const_decl);
        test_koopa!(hello);
        test_koopa!(land);
        test_koopa!(logic);
        test_koopa!(lor);
        test_koopa!(unary_exp);
        test_koopa!(var);
        test_koopa!(var2);
        test_koopa!(block);
        test_koopa!(block2);
    }
    mod riscv {
        use crate::{try_main, Args};
        use std::{
            fs::{self},
            path::PathBuf,
        };

        test_riscv!(arithmetic);
        test_riscv!(const_decl);
        test_riscv!(hello);
        test_riscv!(land);
        test_riscv!(logic);
        test_riscv!(lor);
        test_riscv!(unary_exp);
        test_riscv!(var);
        test_riscv!(var2);
        test_riscv!(block);
        test_riscv!(block2);
    }
}
