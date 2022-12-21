---
title: Simple DB schemas
...

# Simple DB schema for Chai SQL

> - [ ] (backlog): Features supported in Python `chai_sql`
>   - [ ] parser for the database format
>   - [ ] evaluation

---

To support basic schema operations, we provide a simple syntax for describing DB relations.

## Included features

This schema is purposefully simplified and allows to specify only the very basic information about the relations.

Only the checked features below are supported:

- [x] Table names
- [x] Column names
- [x] Column value types
- [ ] Keys
- [ ] Constraints
- [ ] Dependencies
- [ ] Indexes
- [ ] Comments

### Example

```hs title="Example schema definition in Chai SQL schema syntax"
types: { Number, String, Boolean }

relation Cat: {
    id: Number,
    name: String,
}

relation Person: {
    id: Number,
    name: String,
    age: Number,
}
```

## Syntax

Below is the defintion of the abstract syntax of the schema file:

<!-- TODO: rewrite the syntax as PEG -->
```hs title="Approximate syntax definition for Chai SQL schemas"
-- Collection of declared type names
(
    types: { (<Type>,)* }
)?

-- Listing of all relations
(
    relation <Name>: { (<Key>: <Type>,)+ }
)*
```

## Semantic correctness

The following properties should be respected for a schema to be semantically correct.

1. All custom defined types should be declared with `types: [...]`
2. Relation attribute types are be available in `types: [...]`
3. The `types` collection can only be declared once
4. The `types` collection contains no duplicate types
5. Relation names should be unique
6. Each relation should have at least one attribute
7. Attribute names within each relation should be unique
