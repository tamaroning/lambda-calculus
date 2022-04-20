use crate::constraint_ty::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    // Base Types
    Unit,
    Bool,

    Fn(Box<Type>, Box<Type>), // ->
}

impl Type {
    pub fn to_constr_ty(&self) -> ConstrType {
        match self {
            Type::Unit => ConstrType::Unit,
            Type::Bool => ConstrType::Bool,
            Type::Fn(dom, range) => {
                ConstrType::Fn(Box::new(dom.to_constr_ty()), Box::new(range.to_constr_ty()))
            }
        }
    }
}
