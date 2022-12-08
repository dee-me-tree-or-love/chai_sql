---
title: SQL fragment
...

# Basic SQL fragment

Following Guagliardo and Libkin (2017), we start with
the _"fragment that [they] call basic SQL"_.

## Covered syntax

The syntax below is an adaption of the proposed basic SQL fragment to
[ABNF](https://en.wikipedia.org/wiki/Augmented_Backus%E2%80%93Naur_form).

<!-- TODO: review if there are better things to fork -->
> *The SQL ABNF syntax is inspired on the BNF proposed by Foster and Godbole (2016).*

### Included features

<!-- TODO: add list of included and not included features of SQL -->

## Queries

<!-- TODO: find out which syntax highlighting would work best here -->
<!-- TODO: rewrite the syntax to an easier format -->

```abnf  title="Query syntax"
; utility definitions
_Token_       = 1*HEXDIG
Alias         = _Token_

; tables
Table         = _Token_
TableAccess   = Table ["AS" Alias]
              / TableAccess "," Table ["AS" Alias]

; columns
Column        = _Token_
ColumnAccess  = Column ["AS" Alias]
              / ColumnAccess "," Column ["AS" Alias]

; query
Query  = "SELECT" ["DISTINCT"] ColumnAccess "FROM" TableAccess "WHERE" Condition
       / "SELECT" ["DISTINCT"] "*" "FROM" TableAccess "WHERE" Condition
       / Query ("UNION" / "INTERSECT" / "EXCEPT") ["ALL"] Query
```

### Typing Queries

<!-- TODO: What about query like `SELECT 1;`? -->
<!-- TODO: add support to the follwing stuff: -->
- [ ] column `SELECT-FROM-WHERE` statement
- [ ] asterix `SELECT-FROM-WHERE` statement
- [ ] constant `SELECT-FROM-WHERE` statement
- [ ] support for `DISTINCT` select modifier
- [ ] query `UNION`, `INTERSECT`, `EXCEPT` operators
- [ ] `ALL` modifier for query operators

#### Query type inference rules

<!-- TODO: follow the semantics chapter to work this out -->
```plaintext  title="Query type inference"

```

## Conditions

<!-- FIXME: rewrite to ABNF -->
<!-- TODO: define the predicate ABNF syntax -->
```abnf  title="Condition syntax"
; Assuming `Predicate` syntax for base types is defined
Condition     = "TRUE" | "FALSE" 
              / Predicate
              / Column "IS" ["NOT"] "NULL"
              / Column "IS" ["NOT"] "IN" Query
              / "EXISTS" Query
              / Condition "AND" Condition
              / Condition "OR" Condition
              / "NOT" Condition
```

### Typing conditions

<!-- TODO: add support to the follwing stuff: -->
- [x] Base cases: `TRUE | FALSE `
- [ ] Predicate logic on basic SQL types
- [x] `NULL` comparison
- [x] `IN` value inclusion comparison
- [x] `EXISTS` non-empty query check
- [x] Basic boolean logic: `AND`, `OR`, `NOT`

#### Condition type inference rules

```plaintext  title="Condition type inference"

(C0) ----------------
        TRUE: bool

(C1) ----------------
       FALSE: bool

       X: bool     Y: bool
(C2) ----------------------
         X AND Y: bool

       X: bool     Y: bool
(C3) ----------------------
         X OR Y: bool

         X: bool
(C4) ----------------
       NOT X: bool

         T: Name
(C5) --------------------------
       T IS [NOT] NULL: bool

       T: Name        Q: Query
(C6) ---------------------------
         T [NOT] IN Q: bool

         Q: Query
(C7) -------------------
       EXISTS Q: bool
```

---

## References

- Guagliardo, P., & Libkin, L. (2017). A formal semantics of SQL queries, its validation, and applications. _Proceedings of the VLDB Endowment, 11_(1), 27–39. <https://doi.org/10.14778/3151113.3151116>
- Foster, E. C., & Godbole, S. (2016). Bnf syntax for selected sql statements. In E. C. Foster & S. Godbole (Eds.), _Database Systems: A Pragmatic Approach_ (pp. 539–583). Apress. <https://doi.org/10.1007/978-1-4842-1191-5_29>
