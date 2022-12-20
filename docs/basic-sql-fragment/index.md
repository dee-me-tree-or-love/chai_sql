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
> *The SQL syntax below is inspired on the BNF proposed by Foster and Godbole (2016).*

### Included features

<!-- TODO: add list of included and not included features of SQL -->
Following the work of Guagliardo and Libkin (2017, p.29), this syntax covers only the following features:

- [ ] Basic 

### Semantic interpretation

<!-- TODO: elaborate on the semantics of SQL discussed by Guagliardo and Libkin -->

## Queries

<!-- TODO: rewrite to PEGs -->
```abnf  title="Query syntax"
; utility definitions
TOKEN         = "foo"
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
Query  = "SELECT" ["DISTINCT"] ColumnAccess QueryFrom
       / "SELECT" ["DISTINCT"] "*" QueryFrom
       / Query
         ("UNION" / "INTERSECT" / "EXCEPT")
         ["ALL"]
         Query ";"

QueryFrom = "FROM" TableAccess ";"
          / "FROM" TableAccess "WHERE" Condition ";"
```

### Typing Queries

- [x] terms
  - [x] table access
  - [x] column access
  - [x] asterix access
  - [x] constant access
  - [x] aliases
- [ ] queries
  - [x] from
  - [x] from where
  - [x] select
  - [x] select distinct
  - [ ] compound queries

#### Query type inference rules

<!-- TODO(backlog): follow the semantics chapter to work this out -->
<!-- TODO: rewrite this into PEGs? -->
```hs  title="Query type inference"
-- Terms
-- `````
     -- TODO: currently no term "expressions" are typed
     --   - [ ] add support for full-names: e.g. R.C
     --   - [ ] add support for expressions: e.g. coalesce(X,Y,...)
     -- TODO: define type-operation `includes? XS YS`
(T0) G |- t : BT /\ includes? BT BaseTypes |= t : BT
(T1) G |- t_1 : BT_1 /\ ... /\ t_n : BT_n
       |= t_1, ..., t_n : BT_1, ..., BT_n
(T2) G |- t_1, ..., t_m : BT_1, ..., BT_m
       |= t_1 as N_1, ..., t_m as N_m : BT_1, ..., BT_m
-- Relations
-- `````````
     -- TODO: will this deal with subqueries?
(R0) G |- r : DbView<x>[YS] /\ includes? DbView<x>[YS] Schema 
       |= r : DbView<x>[YS]
     -- TODO: define type-operation (`U`)nion: `XS U YS`
(R1) G |- r_1 : DbView<x_1>[YS_1] /\ ... /\ r_n : DbView<x_n>[YS_n]
       |= r_1, ..., r_n : DbView<bag>[YS_1 U ... U YS_n]
(R2) G |- r_1, ..., r_n : DbView<x>[YS_1 U ... U YS_n]
       |= r_1 as N_1, ...,  r_m as N_m : DbView<x>[YS_1 U ... U YS_n]
-- Query sources
-- `````````````
(S0) G |- s : DbView<x>[YS]
       |= FROM s : DbView<x>[YS]
     -- NB: Trilean is the type of 3VL used in SQL
     --     see: https://en.wikipedia.org/wiki/Three-valued_logic
(S1) G |- s : DbView<x>[YS] /\ c : Trilean
       |= FROM s WHERE c : DbView<x>[YS]
-- Queries
-- ```````
(Q0) G |- t : BTS |= SELECT t ;          : DbView<bag>[BTS]
(Q1) G |- t : BTS |= SELECT DISTINCT t ; : DbView<set>[BTS]
(Q2) G |- t : BTS /\ fw : DbView<x>[YS] /\ includes? BTS YS
       |= SELECT t fw; : DbView<bag>[BTS]
(Q3) G |- t : BTS /\ fw : DbView<x>[YS] /\ includes? BTS YS
       |= SELECT DISTINCT t fw; : DbView<set>[BTS]
     -- NB: Here we deviate from Guagliardo & Libkin (2017) by not
     --     making a distinction between the scope of `SELECT * ...`
(Q4) G |- fw : DbView<x>[YS] |= SELECT * fw; : DbView<bag>[YS]
(Q5) G |- fw : DbView<x>[YS] |= SELECT DISTINCT * fw; : DbView<set>[YS]
-- Query operations
-- ````````````````
(O0) G |- q_1 : DbView<x>[YS] /\ q_2 : DbView<x>[YS]
       |= q_1 UNION ALL q_2 : DbView<bag>[YS]
(O1) G |- q_1 : DbView<x>[YS] /\ q_2 : DbView<x>[YS]
       |= q_1 INTERSECT ALL q_2 : DbView<bag>[YS]
(O2) G |- q_1 : DbView<x>[YS] /\ q_2 : DbView<x>[YS]
       |= q_1 EXCEPT ALL q_2 : DbView<bag>[YS]
(O3) G |- q_1 : DbView<x>[YS] /\ q_2 : DbView<x>[YS]
       |= q_1 UNION q_2 : DbView<set>[YS]
(O4) G |- q_1 : DbView<x>[YS] /\ q_2 : DbView<x>[YS]
       |= q_1 INTERSECT q_2 : DbView<set>[YS]
(O5) G |- q_1 : DbView<x>[YS] /\ q_2 : DbView<x>[YS]
       |= q_1 EXCEPT q_2 : DbView<set>[YS]
```

## Conditions

<!-- FIXME: rewrite to PEGs -->
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

- [x] Base cases: `TRUE | FALSE | NULL`
- [x] `NULL` comparison
- [x] `IN` value inclusion comparison
- [x] `EXISTS` non-empty query check
- [x] Basic boolean logic: `AND`, `OR`, `NOT`, `IS`, `IS NOT`
- [ ] Extended predicate logic

#### Condition type inference rules

<!-- TODO: fix this up -->
```hs title="Condition type inference"
-- Basics
-- ``````
(C0) |= <true>   : Trilean
(C1) |= <false>  : Trilean
(C2) |= <null>   : Trilean

-- Stanadrd logics
-- ```````````````
(L0) G |- x : Trilean /\ y : Trilean |= x AND y : Trilean
(L1) G |- x : Trilean /\ y : Trilean |= x OR y : Trilean
(L2) G |- x : Trilean |= NOT x : Trilean
(L3) G |- l : BTS /\ r : BTS /\ len? BTS 1
       |= l IS r : Trilean
(L4) G |- l : BTS /\ r : BTS /\ len? BTS 1
       |= l IS NOT r: Trilean
-- Query predicates
-- ``````````````
     -- TODO: define type operation `len? XS N`
(P2) G |- q : DbView<x>[YS]  |= EXISTS q : Trilean
(P4) G |- t : BTS_a /\ ts : BTS_b /\ len? BTS_a 1 /\ includes? BTS_a BTS_b
       |= t IN ts : Trilean
(P5) G |- t : BTS_a /\ ts : BTS_b /\ len? BTS_a 1 /\ includes? BTS_a BTS_b
       |= t NOT IN ts : Trilean
(P4) G |- t : BTS /\ q : DbView<x>[YS] /\ len? BTS 1 /\ includes? BTS YS
       |= t IN q : Trilean
(P5) G |- t : BTS /\ q : DbView<x>[YS] /\ len? BTS 1 /\ includes? BTS YS
       |= t NOT IN q : Trilean
```

---

## References

- Guagliardo, P., & Libkin, L. (2017). A formal semantics of SQL queries, its validation, and applications. _Proceedings of the VLDB Endowment, 11_(1), 27–39. <https://doi.org/10.14778/3151113.3151116>
- Foster, E. C., & Godbole, S. (2016). Bnf syntax for selected sql statements. In E. C. Foster & S. Godbole (Eds.), _Database Systems: A Pragmatic Approach_ (pp. 539–583). Apress. <https://doi.org/10.1007/978-1-4842-1191-5_29>
  - TODO: maybe this can be removed?

## Extra reading

- <https://en.wikipedia.org/wiki/Typing_rule>
- <https://stanford-cs221.github.io/autumn2021-extra/modules/logic/inference-rules.pdf>
