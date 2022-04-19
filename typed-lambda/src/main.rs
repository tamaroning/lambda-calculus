use chumsky::prelude::*;
use chumsky::Parser;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Term {
    Lit(Lit),
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

#[derive(Debug, Clone, PartialEq, Eq)]
enum Lit {
    Unit,
    True,
    False,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Type {
    // Base Types
    Unit,
    Bool,

    Fn(Box<Type>, Box<Type>), // ->
}

fn parser() -> impl Parser<char, Term, Error = Simple<char>> {
    let lambda = just("lambda").padded();
    let dot = just(".").padded();
    let open_paren = just("(").padded().debug("(");
    let close_paren = just(")").padded().debug(")");
    let colon = just(":").padded();
    let arrow = just("->").padded();

    // type
    let unit = just("Unit").padded().to(Type::Unit);
    let bool = just("Bool").padded().to(Type::Bool);
    let base_ty = choice((unit, bool));
    let ty = recursive(|ty| {
        let fnty = base_ty
            .clone()
            .clone()
            .then_ignore(arrow)
            .then(ty)
            .map(|(l, r)| Type::Fn(Box::new(l), Box::new(r)));

        choice((fnty, base_ty))
    });

    // Type Annotation
    let ty_annot = colon.ignore_then(ty).debug("ty_annot");

    // Literal
    let lit = choice((
        just("unit").padded().to(Lit::Unit).debug("unit"),
        just("true").padded().to(Lit::True).debug("true"),
        just("false").padded().to(Lit::False).debug("false"),
    ))
    .debug("lit");

    // Identifier
    let ident = text::ident::<char, Simple<char>>()
        .padded()
        .map(Var::new)
        .debug("ident");

    // Atomic
    let atom = choice((lit.map(Term::Lit), ident.clone().map(Term::Var)));

    recursive(|term| {
        let paren_term = term
            .clone()
            .delimited_by(open_paren, close_paren)
            .debug("paren_term");

        let smallest = choice((atom, paren_term));

        let app = smallest // abs and app don't come first
            .clone()
            .then(term.clone())
            .map(|(l, r)| Term::App(Box::new(l), Box::new(r)))
            .debug("app");

        let abs = lambda
            .ignore_then(ident)
            .then(ty_annot)
            .then_ignore(dot)
            .then(term)
            .map(|((var, arg_ty), body)| Term::Abs(var, Box::new(arg_ty), Box::new(body)))
            .debug("abs");

        // Priority
        // Abs > App > Lit > Ident > "("Term")"
        choice((abs, app, smallest))
            .then_ignore(end())
            .debug("term")
        //smallest.then_ignore(end())
    })
}

type TypingResult = Result<Type, String>;

struct TypingContext(Vec<(Var, Type)>);

impl TypingContext {
    fn new(pairs: Vec<(Var, Type)>) -> Self {
        TypingContext(pairs)
    }

    fn get_type(&self, x: &Var) -> Option<Type> {
        for (var, ty) in self.0.iter().rev() {
            if *x == *var {
                return Some(ty.clone());
            }
        }
        None
    }
}

trait Typing {
    fn typing(&self, ctx: &mut TypingContext) -> TypingResult;
}

impl Typing for Term {
    fn typing(&self, ctx: &mut TypingContext) -> TypingResult {
        match self {
            Term::Lit(lit) => lit.typing(ctx),
            Term::Abs(var, argty, body) => {
                ctx.0.push((var.clone(), *argty.clone()));
                let retty = body.typing(ctx)?;
                let fnty = Type::Fn(argty.clone(), Box::new(retty));
                Ok(fnty)
            }
            Term::Var(var) => match ctx.get_type(var) {
                Some(ty) => Ok(ty),
                None => Err(format!("Couldn't find type of `{}`", var.sym)),
            },
            Term::App(l, r) => {
                let l_ty = l.typing(ctx)?;
                let r_ty = r.typing(ctx)?;

                match l_ty {
                    Type::Fn(dom, range) if *dom == r_ty => Ok(*range),
                    _ => Err(format!("Couldn't apply `{:?}` to `{:?}`", l_ty, r_ty)),
                }
            }
        }
    }
}

impl Typing for Lit {
    fn typing(&self, _: &mut TypingContext) -> TypingResult {
        match self {
            Lit::Unit => Ok(Type::Unit),
            Lit::True => Ok(Type::Bool),
            Lit::False => Ok(Type::Bool),
        }
    }
}

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

#[cfg(test)]
fn debug_parse(src: &str) -> (Option<Term>, Vec<Simple<char>>) {
    let parser = parser();
    parser.parse_recovery_verbose(src)
}

#[test]
fn parse_ok() {
    let (term, _) = debug_parse("unit");
    assert_eq!(term, Some(Term::Lit(Lit::Unit)));

    let (term, _) = debug_parse("lambda x: Unit. x");
    assert_eq!(
        term,
        Some(Term::Abs(
            Var::new("x".to_string()),
            Box::new(Type::Unit),
            Box::new(Term::Var(Var::new("x".to_string())))
        ))
    );

    let (term, _) = debug_parse("( unit )");
    assert_eq!(term, Some(Term::Lit(Lit::Unit)));
}

#[test]
fn parse_err() {}
