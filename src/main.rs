use std::env::args;
use lalrpop_util::lalrpop_mod;
use std::io::Result;
use std::fs::read_to_string;

lalrpop_mod!(sysy);
mod ast;

use ast::*;
fn main() -> Result<()> {
    let mut args = args();
    args.next();

    let mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    let input = read_to_string(input)?;
    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();

    println!("{:#?}", ast);
    Ok(())
}