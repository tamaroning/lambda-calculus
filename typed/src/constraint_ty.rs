// Constraint Types
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstrType {
    // Type Variables
    Var(i32),
    Unit,
    Bool,
    Fn(Box<ConstrType>, Box<ConstrType>),
}
