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
Following the work of Guagliardo and Libkin (2017, p.29), this syntax covers only the following features:

- [ ] Basic 

### Semantic interpretation

<!-- TODO: elaborate on the semantics of SQL discussed by Guagliardo and Libkin -->

## Queries

<!-- TODO: find out which syntax highlighting would work best here -->
<!-- TODO: rewrite the syntax to an easier format -->

```abnf  title="Query syntax"
; utility definitions
TOKEN         = 1*HEXDIG
Alias         = TOKEN

; tables
Table         = TOKEN
TableAccess   = Table ["AS" Alias]
              / TableAccess "," Table ["AS" Alias]

; columns
Column        = TOKEN
ColumnAccess  = Column ["AS" Alias]
              / ColumnAccess "," Column ["AS" Alias]

; query
Query  = "SELECT" ["DISTINCT"] ColumnAccess
         "FROM" TableAccess "WHERE" Condition
       / "SELECT" ["DISTINCT"] "*"
         "FROM" TableAccess "WHERE" Condition
       / Query ("UNION" / "INTERSECT" / "EXCEPT") ["ALL"] Query
```

### Typing Queries

<!-- TODO: What about query like `SELECT 1;`? -->
- [x] column `SELECT-FROM-WHERE` statement
- [x] asterix `SELECT-FROM-WHERE` statement
- [ ] constant `SELECT-FROM-WHERE` statement
- [x] selection with `DISTINCT` select modifier
- [x] compound queries `UNION`, `INTERSECT`, and `EXCEPT`
- [x] compound queries with `ALL` modifier

#### Query type inference rules

<!-- TODO(backlog): follow the semantics chapter to work this out -->
<!-- FIXME: explain type polymorphism & operators  -->
```hs  title="Query type inference"
-- Basics
-- ~~~~~~
-- TODO: consider these to make it less ambigious?
--   (QB0) FROM s : TableReference[XS] |- FROM s : QueryResult[XS]
--   (QB0) s : TableReference[XS] |- FROM s : QueryResult[XS]
(QB0) s : TableReference[XS] |- s : QueryResult[XS]
--

-- Selections
-- ~~~~~~~~~~
-- See https://www.sqlite.org/lang_select.html
(QS0)   s : QueryResult[XS] -- apply (QB0) -- FIXME: this doesn't make sense atm
     &  p : bool
     |- SELECT * FROM s WHERE p : QueryResult[XS]
--
(QS1)   s : QueryResult[XS]
     &  p : bool
     -- TODO: define QueryResult<Distinct>[A]
     |- SELECT DISTINCT * FROM s WHERE p : QueryResult<Distinct>[XS]
--
-- TODO: define QueryResult[A] -> Selection[B]
(QS2)   c : QueryResult[XS] -> Selection[YS]
     &  s : QueryResult[XS]
     &  p : bool
     |- SELECT c FROM s WHERE p : QueryResult[YS]
--
(QS3)   c : QueryResult[XS] -> Selection[YS]
     &  s : QueryResult[XS]
     &  p : bool
     |- SELECT DISTINCT c FROM s WHERE p : QueryResult<Distinct>[YS]

-- Compound queries
-- ~~~~~~~~~~~~~~~~
-- See https://www.sqlite.org/lang_select.html#compound_select_statements
(QO0)   q1 : QueryResult[XS]
     &  q2 : QueryResult[YS]
     -- TODO: define A + B
     |- q1 UNION q2 : QueryResult<Distinct>[XS + YS]
--
(QO1)   q1 : QueryResult[XS]
     &  q2 : QueryResult[YS]
     -- TODO: define A & B
     |- q1 INTERSECT q2 : QueryResult<Distinct>[XS & YS]
--
(QO2)   q1 : QueryResult[XS]
     &  q2 : QueryResult[YS]
     -- TODO: define A / B
     |- q1 EXCEPT q2 : QueryResult<Distinct>[XS / YS]
--
(QO3)   q1 : QueryResult[XS]
     &  q2 : QueryResult[YS]
     |- q1 UNION ALL q2 : QueryResult[XS + YS]
--
(QO4)   q1 : QueryResult[XS]
     &  q2 : QueryResult[YS]
     |- q1 INTERSECT ALL q2 : QueryResult[XS & YS]
--
(QO5)   q1 : QueryResult[XS]
     &  q2 : QueryResult[YS]
     |- q1 EXCEPT ALL q2 : QueryResult[XS / YS]
```

## Conditions

<!-- FIXME: rewrite to ABNF -->
<!-- TODO: define the predicate ABNF syntax -->
```abnf  title="Condition syntax"
; predicate
Predicate     = TOKEN

; condition
Condition     = "TRUE" 
              / "FALSE" 
              / Predicate
              / Column "IS" ["NOT"] "NULL"
              / Column "IS" ["NOT"] "IN" Query
              / "EXISTS" Query
              / Condition "AND" Condition
              / Condition "OR" Condition
              / "NOT" Condition
```

### Typing conditions

- [x] Base cases: `TRUE | FALSE `
- [ ] Predicate logic on basic SQL types
- [x] `NULL` comparison
- [x] `IN` value inclusion comparison
- [x] `EXISTS` non-empty query check
- [x] Basic boolean logic: `AND`, `OR`, `NOT`

#### Condition type inference rules

<!-- TODO: consider which syntax format works best here -->
```hs title="Condition type inference"
-- Basics
-- ~~~~~~
--
(C0)                        |- TRUE          : bool
--
(C1)                        |- FALSE         : bool

-- Stanadrd logics
-- ~~~~~~~~~~~~~~~
--
(C2) x : bool & y : bool    |- x AND y       : bool
--
(C3) x : bool & y : bool    |- x OR y        : bool
--
(C4) x : bool               |- NOT x         : bool

-- Query checking
-- ~~~~~~~~~~~~~~
--
(C5) t : ColumnReference    |- t IS NULL     : bool
--
(C5) t : ColumnReference    |- t IS NOT NULL : bool
--
(C6) q : QueryResult        |- EXISTS q      : bool
--
(C7)    t : ColumnReference
     &  q : QueryResult
     |- t IN q     : bool
--
(C8)    t : ColumnReference
     &  q : QueryResult
     |- t NOT IN q : bool
```

---

## References

- Guagliardo, P., & Libkin, L. (2017). A formal semantics of SQL queries, its validation, and applications. _Proceedings of the VLDB Endowment, 11_(1), 27–39. <https://doi.org/10.14778/3151113.3151116>
- Foster, E. C., & Godbole, S. (2016). Bnf syntax for selected sql statements. In E. C. Foster & S. Godbole (Eds.), _Database Systems: A Pragmatic Approach_ (pp. 539–583). Apress. <https://doi.org/10.1007/978-1-4842-1191-5_29>

## Extra reading

- <https://en.wikipedia.org/wiki/Typing_rule>
- <https://stanford-cs221.github.io/autumn2021-extra/modules/logic/inference-rules.pdf>
