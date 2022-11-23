# Basic SQL Fragment

Following the paper by Guagliardo & Libkin (2017), we start with the
_"fragment that [they] call basic SQL"_.

## Included syntax

The syntax below is an adaption of the basic SQL fragment.

### Queries

```plaintext title="Query syntax"
-- tables
tau:beta :=   T1 AS N1, ..., Tk AS Nk
              for tau = (T1,...,Tk), beta=(N1,...,Nk), k > 0

-- column aliases
alpha:beta' :=   t1 AS N'1, ..., tm AS N'm
                 for alpha = (t1,...,tm), beta=(N'1,...,N'm), m > 0

Query  :=     SELECT[DISTINCT] alpha:beta' FROM tau:beta WHERE Condition
       |      SELECT[DISTINCT] * FROM tau:beta WHERE Condition
       |      Query (UNION | INTERSECT | EXCEPT) [ALL] Query
```

#### Typing Queries

<!-- TODO: follow the semantics chapter to work this out -->

```plaintext

```

### Conditions

```plaintext  title="Condition syntax"
-- Parameterized with collection of predicates P
Condition     :=     TRUE | FALSE 
              |      P'(t*), P' \in P
              |      t IS [NOT] NULL
              |      t' IS [NOT] IN Query
              |      EXISTS Query
              |      Condition AND Condition
              |      Condition OR Condition
              |      NOT Condition
```

#### Typing conditions without Predicates P

<!-- TODO: follow the semantics chapter to work this out -->

```plaintext

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
