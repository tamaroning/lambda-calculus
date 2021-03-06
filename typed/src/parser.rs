use chumsky::prelude::*;
use chumsky::Parser;

use crate::ty::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Lit(Lit),
    Var(Var),
    Abs(Var, Option<Type>, Box<Term>),
    App(Box<Term>, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Var {
    pub sym: String,
}

impl Var {
    pub fn new(sym: String) -> Self {
        Var { sym }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lit {
    Unit,
    True,
    False,
}

pub fn parser() -> impl Parser<char, Term, Error = Simple<char>> {
    let lambda_kw = just("lambda").padded();
    let in_kw = just("let").padded();
    let let_kw = just("in").padded();
    let if_kw = just("if").padded();
    let then_kw = just("then").padded();
    let else_kw = just("else").padded();
    let eq = just("=").padded();
    let dot = just(".").padded();
    let open_paren = just("(").padded();
    let close_paren = just(")").padded();
    let colon = just(":").padded();
    let arrow = just("->").padded();

    //let keyword = choice((lambda_kw.clone(), in_kw.clone(), let_kw.clone(), if_kw.clone(), then_kw.clone(), else_kw.clone()));

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
        //.filter_map(|span: Span, s: String| { Err(Simple::custom(span, format!("'{}'", s))) })
        .padded()
        .map(Var::new)
        .debug("ident");

    let term = recursive(|term| {
        // Syntax
        //   atom ::= <ident> | <lit> | "(" term ")"
        //   app  ::= atom term
        //   abs  ::= lambda <ident> (":" <Type>)? "." term
        //   term ::= abs | app | atom
        let paren_term = open_paren
            .ignore_then(term.clone())
            .then_ignore(close_paren)
            .debug("paren_term");

        let atom = choice((lit.map(Term::Lit), ident.clone().map(Term::Var), paren_term));

        let app = atom
            .clone() // abs and app don't come first
            .then(term.clone())
            .map(|(l, r)| Term::App(Box::new(l), Box::new(r)))
            .debug("app");

        let abs = lambda_kw
            .ignore_then(ident.clone())
            .then(ty_annot.clone())
            .then_ignore(dot)
            .then(term.clone())
            .map(|((var, arg_ty), body)| Term::Abs(var, Some(arg_ty), Box::new(body)))
            .debug("abs");

        // FIXME:
        let if_else = if_kw
            .ignore_then(term.clone())
            .then_ignore(then_kw)
            .then(term.clone())
            .then_ignore(else_kw)
            .then(term.clone())
            .map(|((cond, then), els)| Term::If(Box::new(cond), Box::new(then), Box::new(els)));

        // Suger Syntax
        // let x : T = t1 in t
        // (lambda x: T. t) t1
        // FIXME: not working
        let sugar_let = let_kw
            .ignore_then(ident)
            .then(ty_annot)
            .then_ignore(eq)
            .then_ignore(in_kw)
            .then(term.clone())
            .then_ignore(in_kw)
            .then(term)
            .map(|(((id, argty), t1), t)| {
                Term::App(
                    Box::new(Term::Abs(id, Some(argty), Box::new(t))),
                    Box::new(t1),
                )
            });

        // Priority
        //   Abs > App > Lit > Ident > "("Term")"
        choice((sugar_let, if_else, abs, app, atom)).debug("term")
    });

    term.then_ignore(end())
}

#[cfg(test)]
fn debug_parse(src: &str) -> (Option<Term>, Vec<Simple<char>>) {
    let parser = parser();
    parser.parse_recovery_verbose(src)
}

#[test]
fn parse_ok() {
    // var
    let (term, _) = debug_parse("var1");
    assert_eq!(term, Some(Term::Var(Var::new("var1".to_string()))));

    // lit
    let (term, _) = debug_parse("unit");
    assert_eq!(term, Some(Term::Lit(Lit::Unit)));

    // abs
    let (term, _) = debug_parse("lambda x: Unit. x");
    assert_eq!(
        term,
        Some(Term::Abs(
            Var::new("x".to_string()),
            Some(Type::Unit),
            Box::new(Term::Var(Var::new("x".to_string())))
        ))
    );

    // (ident)
    let (term, _) = debug_parse("(x)");
    assert_eq!(term, Some(Term::Var(Var::new("x".to_string()))));

    // (lit)
    let (term, _) = debug_parse("(unit)");
    assert_eq!(term, Some(Term::Lit(Lit::Unit)));

    // ((lit))
    let (term, _) = debug_parse("(( unit ))");
    assert_eq!(term, Some(Term::Lit(Lit::Unit)));

    // abs abs
    let (term, _) = debug_parse("lambda x: Unit -> Unit. lambda y: Unit. x y");
    assert_eq!(
        term,
        Some(Term::Abs(
            Var::new("x".to_string()),
            Some(Type::Fn(Box::new(Type::Unit), Box::new(Type::Unit))),
            Box::new(Term::Abs(
                Var::new("y".to_string()),
                Some(Type::Unit),
                Box::new(Term::App(
                    Box::new(Term::Var(Var::new("x".to_string()))),
                    Box::new(Term::Var(Var::new("y".to_string()))),
                ))
            ))
        ))
    );

    // ident ident
    let (term, _) = debug_parse("x x");
    assert_eq!(
        term,
        Some(Term::App(
            Box::new(Term::Var(Var::new("x".to_string()))),
            Box::new(Term::Var(Var::new("x".to_string())))
        ))
    );

    // ident ident ident
    let (term, _) = debug_parse("x y z");
    assert_eq!(
        term,
        Some(Term::App(
            Box::new(Term::Var(Var::new("x".to_string()))),
            Box::new(Term::App(
                Box::new(Term::Var(Var::new("y".to_string()))),
                Box::new(Term::Var(Var::new("z".to_string())))
            ))
        ))
    );

    // (abs) lit
    let (term, _) = debug_parse("(lambda x: Unit. x) unit");
    assert_eq!(
        term,
        Some(Term::App(
            Box::new(Term::Abs(
                Var::new("x".to_string()),
                Some(Type::Unit),
                Box::new(Term::Var(Var::new("x".to_string())))
            )),
            Box::new(Term::Lit(Lit::Unit))
        ))
    );

    // (abs) (abs) lit
    let (term, _) = debug_parse("(lambda x: Unit. x) (lambda x: Unit. x) unit");
    assert_eq!(
        term,
        Some(Term::App(
            Box::new(Term::Abs(
                Var::new("x".to_string()),
                Some(Type::Unit),
                Box::new(Term::Var(Var::new("x".to_string())))
            )),
            Box::new(Term::App(
                Box::new(Term::Abs(
                    Var::new("x".to_string()),
                    Some(Type::Unit),
                    Box::new(Term::Var(Var::new("x".to_string())))
                )),
                Box::new(Term::Lit(Lit::Unit))
            ))
        ))
    );
}

#[test]
fn parse_err() {
    let (term, _) = debug_parse("lambda (x): Unit. x");
    assert_eq!(term, None);

    let (term, _) = debug_parse("lambda unit.");
    assert_eq!(term, None);

    let (term, _) = debug_parse("((unit)");
    assert_eq!(term, None);

    let (term, _) = debug_parse("unit)");
    assert_eq!(term, None);

    let (term, _) = debug_parse("let x : Unit = unit in x");
    assert_eq!(term, None);
}

#[allow(unused)]
#[cfg(test)]
//#[test]
fn debug_test() {
    let (term, e) = debug_parse("(unit)");
    for e in &e {
        println!("Error: {:?}", e)
    }
    let term = term.unwrap();
    println!("Parse OK");
    assert_eq!(term, Term::Lit(Lit::Unit));
    panic!()
}
