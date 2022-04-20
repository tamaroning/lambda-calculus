mod parser;
mod typing;

use chumsky::Parser;
use parser::*;
use typing::*;

fn main() {
    let src = std::env::args().nth(1).unwrap();
    println!("term = `{}`", src);

    let parse_res = parser().parse(src);
    let term = match parse_res {
        Ok(term) => term,
        Err(e) => {
            println!("Parse Error: {:?}", e);
            std::process::exit(1);
        }
    };
    //println!("internal-term = {:?}", term);

    let mut tyctx = TypingContext::new(vec![]);
    let ty = match term.typing(&mut tyctx) {
        Ok(ty) => ty,
        Err(e) => {
            println!("Type Error: {:?}", e);
            std::process::exit(1);
        }
    };
    println!("term: {:?}", ty);
}
