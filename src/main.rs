use std::env::args;
use std::fmt;

use std::fs::{File, read_to_string};

use std::io;
use std::process::exit;
use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;

mod ast;
mod ir_gen;

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

    let _mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();
    let output_file = File::create(output).map_err(Error::File)?;

    let input = read_to_string(input).map_err(Error::File)?;
    let ast =  sysy::CompUnitParser::new().parse(&input).map_err(|_| Error::Parse)?;

    let program = ir_gen::generate_program(&ast);

    KoopaGenerator::new(output_file).generate_on(&program).unwrap();
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