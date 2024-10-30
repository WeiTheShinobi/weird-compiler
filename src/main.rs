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
            println!("unsupport mode: {}", args.mode);
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

    use std::fs::read_to_string;

    use crate::{try_main, Args};

    fn path(file_name: &str, mode: &str) -> (String, String, String, String) {
        let mut input = "./tests/input/".to_string();
        input.push_str(file_name);
        input.push_str(".c");
        let mut output = "./tests/output/".to_string();
        output.push_str(file_name);
        output.push_str(mode);
        let mut wanted = "./tests/wanted/".to_string();
        wanted.push_str(file_name);
        wanted.push_str(mode);
        (input, output, wanted, file_name.to_string())
    }

    #[test]
    fn koopa() {
        let test_cases = [
            (path("hello", ".koopa")),
            (path("unary_exp", ".koopa")),
            (path("arithmetic", ".koopa")),
        ];

        for test_cases in test_cases {
            let mode = "-koopa".to_string();
            let args = Args::new(mode, test_cases.0, test_cases.1.clone());

            if let Err(e) = try_main(args) {
                dbg!(e.to_string());
            }
            let actual = read_to_string(test_cases.1.clone()).expect(&format!(
                "Failed to read file at path: {}",
                test_cases.1.clone()
            ));
            let expect = read_to_string(test_cases.2.clone()).expect(&format!(
                "Failed to read file at path: {}",
                test_cases.2.clone()
            ));

            assert_eq!(actual, expect, "file name: {}", test_cases.3);
        }
    }

    #[test]
    fn riscv() {
        let mode = "-riscv";
        let test_cases = [
            // (path("hello", ".riscv")),
            (path("unary_exp", ".riscv")),
            (path("arithmetic", ".riscv")),
        ];

        for test_cases in test_cases {
            let args = Args::new(mode.to_string(), test_cases.0, test_cases.1.clone());

            if let Err(e) = try_main(args) {
                dbg!(e.to_string());
            }
            let actual = read_to_string(test_cases.1.clone()).expect(&format!(
                "Failed to read file at path: {}",
                test_cases.1.clone()
            ));
            let expect = read_to_string(test_cases.2.clone()).expect(&format!(
                "Failed to read file at path: {}",
                test_cases.2.clone()
            ));

            assert_eq!(actual, expect, "\nfile name: {}", test_cases.3);
        }
    }
}
