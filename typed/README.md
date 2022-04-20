# typed-lambda-calculus
## Usage
```
cargo run "lambda x: Unit. x"
```

## Syntax
- atom ::= `ident` | `lit` | "(" term ")"
- app  ::= atom term
- abs  ::= lambda `ident` (":" `Type`)? "." term
- term ::= abs | app | atom

## Type System
- Base Types : Unit, and Bool.
- Type Constructor : Arrow (->)

If type annotations are omitted, the type checker infers the types.
