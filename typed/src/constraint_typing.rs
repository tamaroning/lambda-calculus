use std::collections::HashSet;

use crate::constraint_ty::*;
use crate::parser::*;
pub type ConstrTypingResult = Result<(ConstrType, ConstrSet), String>;

pub struct ConstrTypingContext(Vec<(Var, ConstrType)>);

impl ConstrTypingContext {
    pub fn new(pairs: Vec<(Var, ConstrType)>) -> Self {
        ConstrTypingContext(pairs)
    }

    pub fn get_type(&self, x: &Var) -> Option<ConstrType> {
        for (var, ty) in self.0.iter().rev() {
            if *x == *var {
                return Some(ty.clone());
            }
        }
        None
    }
}

// Constraint Set { l = r }_i
#[derive(Debug)]
pub struct ConstrSet {
    v: Vec<(ConstrType, ConstrType)>,
    // keep all numbers which identify type variables
    used: HashSet<i32>,
}

impl ConstrSet {
    fn new() -> Self {
        ConstrSet {
            v: Vec::new(),
            used: HashSet::new(),
        }
    }

    fn concat(mut v1: Self, mut v2: Self) -> Self {
        v1.v.append(&mut v2.v);
        v1.used.extend(v2.used);
        // TODO: concat hashset
        v1
    }

    fn push(&mut self, c: (ConstrType, ConstrType)) {
        self.v.push(c);
    }

    fn make_fresh_ty_var(&mut self) -> ConstrType {
        let max_ty_var_id = self.used.iter().max();
        let published = match max_ty_var_id {
            Some(m) => m + 1,
            None => 0,
        };
        self.used.insert(published);
        ConstrType::Var(published)
    }
}

pub trait ConstrTyping {
    fn constr_typing(&self, ctx: &mut ConstrTypingContext) -> ConstrTypingResult;
}

impl ConstrTyping for Term {
    fn constr_typing(&self, ctx: &mut ConstrTypingContext) -> ConstrTypingResult {
        match self {
            Term::Var(var) => var.constr_typing(ctx),
            Term::Lit(lit) => lit.constr_typing(ctx),
            Term::Abs(var, argty, body) => {
                if argty.is_none() {
                    todo!("type inference");
                }
                let argty = argty.clone().unwrap().to_constr_ty();

                ctx.0.push((var.clone(), argty.clone()));
                let (body_ty, constr_set) = body.constr_typing(ctx)?;
                let fnty = ConstrType::Fn(Box::new(argty), Box::new(body_ty));
                Ok((fnty, constr_set))
            }
            Term::App(l, r) => {
                let (l_constr_ty, l_constr_set) = l.constr_typing(ctx)?;
                let (r_constr_ty, r_constr_set) = r.constr_typing(ctx)?;
                let mut new_constr_set = ConstrSet::concat(l_constr_set, r_constr_set);

                // create fresh type variable
                let fresh_ty_var = new_constr_set.make_fresh_ty_var();

                let new_constr = (
                    l_constr_ty,
                    ConstrType::Fn(Box::new(r_constr_ty), Box::new(fresh_ty_var.clone())),
                );
                new_constr_set.push(new_constr);

                Ok((fresh_ty_var, new_constr_set))
            }
            Term::If(_, _, _) => todo!(),
        }
    }
}

impl ConstrTyping for Var {
    fn constr_typing(&self, ctx: &mut ConstrTypingContext) -> ConstrTypingResult {
        match ctx.get_type(self) {
            Some(ty) => Ok((ty, ConstrSet::new())),
            None => Err(format!("Couldn't find type of `{}`", self.sym)),
        }
    }
}

impl ConstrTyping for Lit {
    fn constr_typing(&self, _: &mut ConstrTypingContext) -> ConstrTypingResult {
        match self {
            Lit::Unit => Ok((ConstrType::Unit, ConstrSet::new())),
            Lit::True => Ok((ConstrType::Bool, ConstrSet::new())),
            Lit::False => Ok((ConstrType::Bool, ConstrSet::new())),
        }
    }
}

#[test]
fn test() {
    use chumsky::Parser;

    let src = "(lambda x: Unit. x) unit".to_string();

    let parse_res = parser().parse(src);
    let term = match parse_res {
        Ok(term) => term,
        Err(e) => {
            println!("Parse Error: {:?}", e);
            std::process::exit(1);
        }
    };
    println!("internal-term = {:?}", term);

    let mut ctx = ConstrTypingContext::new(vec![]);
    let ty = match term.constr_typing(&mut ctx) {
        Ok(ty) => ty,
        Err(e) => {
            println!("Type Error: {:?}", e);
            std::process::exit(1);
        }
    };
    println!("term: {:?}", ty);

    panic!()
}
