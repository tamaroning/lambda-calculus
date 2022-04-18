use chumsky::prelude::*;
use chumsky::Parser;

#[derive(Debug, Clone)]
enum Term {
    Var(Var),
    Abs(Var, Box<Term>),
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
enum DbTerm {
    Var(usize), // de Bruijn's index
    Abs(Box<DbTerm>),
    App(Box<DbTerm>, Box<DbTerm>),
}

#[derive(Debug)]
struct NamingContext {
    vars: Vec<Var>,
}

impl NamingContext {
    fn new(vars: Vec<Var>) -> Self {
        NamingContext { vars }
    }

    // searches x from right to left and returns its index
    fn get_dbindex(&self, x: &Var) -> Option<usize> {
        for (index, var) in self.vars.iter().rev().enumerate() {
            if *x == *var {
                return Some(index);
            }
        }
        None
    }
}

fn remove_names(term: &Term, ctx: &mut NamingContext) -> DbTerm {
    match term {
        Term::Var(var) => {
            let dbindex = ctx.get_dbindex(var);
            match dbindex {
                Some(dbidx) => DbTerm::Var(dbidx),
                None => panic!("Couldn't find `{}` in given naming context", var.sym),
            }
        }
        Term::Abs(var, body) => {
            ctx.vars.push(var.clone());
            DbTerm::Abs(Box::new(remove_names(&**body, ctx)))
        }
        Term::App(l, r) => DbTerm::App(
            Box::new(remove_names(&**l, ctx)),
            Box::new(remove_names(&**r, ctx)),
        ),
    }
}

fn parser() -> impl Parser<char, Term, Error = Simple<char>> {
    let lambda = just("lambda").padded();
    let dot = just(".").padded();
    let open_paren = just("(").padded();
    let close_paren = just(")").padded();

    let ident = text::ident::<char, Simple<char>>().padded().map(Var::new);

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
                .then_ignore(dot)
                .then(choice((
                    // body
                    abs,
                    app.clone(),
                    smallest.clone(),
                )))
                .map(|(var, body)| Term::Abs(var, Box::new(body)))
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

    let mut naming_context = NamingContext::new(vec![]);
    let dbterm = remove_names(&term, &mut naming_context);

    println!("{:?}", dbterm);
}
