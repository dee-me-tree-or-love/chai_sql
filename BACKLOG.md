# Features for the Typed SQL checker
<!-- TODO: reference this in the Docs->DeveloperResources? -->

- [ ] comment-based SQL type annotations
  - [ ] raw SQL with type annotations in comments should be parseable
  - [ ] the parsed SQL AST should be transformed into a type-annotated AST
    - AST -> Typer -> Typed AST
  - [ ] type annotations should follow from the type inference rules?
- [ ] type checking of the Basic SQL Fragment
  - [ ] type checking boolean conditions (without predicates)
  - [ ] type checking boolean conditions (with predicates)
  - [ ] type checking basic select queries

## Task order

1. Produce annotated ASTs
2. Construct the type checker
3. Extend typed syntax
4. Add type alias declarations
5. Extend supported SQL syntax
