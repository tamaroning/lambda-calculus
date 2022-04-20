use crate::parser::*;

pub type TypingResult = Result<Type, String>;

pub struct TypingContext(Vec<(Var, Type)>);

impl TypingContext {
    pub fn new(pairs: Vec<(Var, Type)>) -> Self {
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

pub trait Typing {
    fn typing(&self, ctx: &mut TypingContext) -> TypingResult;
}

impl Typing for Term {
    fn typing(&self, ctx: &mut TypingContext) -> TypingResult {
        match self {
            Term::Lit(lit) => lit.typing(ctx),
            Term::Abs(var, ref argty, body) => {
                if argty.is_none() {
                    unimplemented!("infer type")
                }
                let argty = argty.clone().unwrap();

                ctx.0.push((var.clone(), argty.clone()));
                let retty = body.typing(ctx)?;
                let fnty = Type::Fn(Box::new(argty), Box::new(retty));
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
            Term::If(cond, then, els) => {
                let cond_ty = cond.typing(ctx)?;
                match cond_ty {
                    Type::Bool => {
                        let (then_ty, els_ty) = (then.typing(ctx)?, els.typing(ctx)?);
                        if then_ty == els_ty {
                            Ok(then_ty)
                        } else {
                            Err(format!(
                                "Types mismatch: then : `{:?}` and else: `{:?}`",
                                then_ty, els_ty
                            ))
                        }
                    }
                    _ => Err(format!("Expected `Bool`, but found `{:?}`", cond_ty)),
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
