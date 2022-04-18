use chumsky::prelude::*;
use chumsky::Parser;

#[derive(Debug, Clone)]
enum Term {
    Var(Var),
    Abs(Var, Box<Type>, Box<Term>),
    App(Box<Term>, Box<Term>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Var {
    sym: String,
}

impl Var {
    fn new(sym: String) -> Self {
        Var { sym }
    }
}

#[derive(Debug, Clone)]
enum Type {
    Unit,
    Fn(Box<Type>, Box<Type>), // ->
}

fn parser() -> impl Parser<char, Term, Error = Simple<char>> {
    let lambda = just("lambda").padded();
    let dot = just(".").padded();
    let open_paren = just("(").padded();
    let close_paren = just(")").padded();

    let ident = text::ident::<char, Simple<char>>().padded().map(Var::new);

    // type
    let colon = just(":").padded();
    let arrow = just("->").padded();
    let unit = just("Unit").padded().to(Type::Unit);
    let ty = recursive(|ty| {
        let fnty = ty.clone().then_ignore(arrow).then(ty).map(|(l, r)|
            Type::Fn(Box::new(l), Box::new(r))
        );
        
        choice((
            unit,
            fnty,
        ))
    });

    let ty_annot = colon.ignore_then(ty);

    recursive(|term| {
        let smallest = choice((
            ident.map(Term::Var),
            open_paren
                .ignore_then(term.clone())
                .then_ignore(close_paren),
        ));

        let app = smallest
            .clone()
            .then(term.clone())
            .map(|(l, r)| Term::App(Box::new(l), Box::new(r)));

        let abs = recursive(|abs| {
            lambda
                .ignore_then(ident)
                .then(ty_annot)
                .then_ignore(dot)
                .then(choice((
                    // body
                    abs,
                    app.clone(),
                    smallest.clone(),
                )))
                .map(|((var, arg_ty), body)|
                    Term::Abs(var, Box::new(arg_ty), Box::new(body))
                )
        });

        choice((abs, app, smallest)).then_ignore(end())
    })
}

fn main() {
    let src = std::env::args().nth(1).unwrap();

    let parse_res = parser().parse(src);
    let term = match parse_res {
        Ok(term) => term,
        Err(e) => {
            println!("Parse Error: {:?}", e);
            std::process::exit(1);
        }
    };
    println!("{:?}", term);
}
