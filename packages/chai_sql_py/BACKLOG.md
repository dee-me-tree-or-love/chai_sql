# Features for the Typed SQL checker
<!-- TODO: reference this in the Docs->DeveloperResources? -->

- [ ] type checking of the Basic SQL Fragment
  - [ ] Basic SQL frafment syntax described (& parseable?)
    - [ ] syntax spec & tests?
  - [ ] Typing rules based on SQL semantics
    - [ ] type checking boolean conditions (without predicates)
    - [ ] type checking basic select queries
    - [ ] type checking boolean conditions (with predicates)
    - [ ] type checking compound queries

- [ ] comment-based SQL type annotations
  - [ ] raw SQL with type annotations in comments should be parseable
  - [ ] the parsed SQL AST should be transformed into a type-annotated AST
        AST -> Typer -> Typed AST
  - [ ] type annotations should follow from the type inference rules?

## Task order

1. Produce annotated ASTs
2. Construct the type checker
3. Extend typed syntax
4. Add type alias declarations
5. Extend supported SQL syntax

## Typing implementation framework

1. Understand semantics
2. Develop type rules
3. Express them through type defintions
