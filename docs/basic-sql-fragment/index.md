# Basic SQL Fragment

Following the paper by Guagliardo & Libkin (2017), we start with the
_"fragment that [they] call basic SQL"_.

## Included syntax

### Queries

<!-- TODO -->

### Conditions

#### Conditions with Predicates P

<!-- TODO: work it out -->

#### Conditions without Predicates P

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

- TODO: add paper
