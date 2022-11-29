# Type Annotations for SQL

<!-- TODO: give an introduction -->

## Type Annotation examples

<!-- TODO: write a preamble -->

```sql  title="Type annotation in SQL example"
-- @typecheck
-- @returns DbResult[Grouper[String], AVG[Number]]
SELECT name, AVG(age)
FROM (
  -- @returns DbResult[String, Number]
  SELECT firstName AS name, age FROM students
) AS s 
-- @returns DbResult[*] -> DbResult[*, Grouped[String], *]
GROUP BY name;
```

### Syntactic sugar

To reduce the amount of typing, type annotations support extra syntactic sugar.

```sql  title="Sugary type annotation in SQL example"
-- @tc
-- @tr {Grouper[String], AVG[Number]}
SELECT name, AVG(age)
FROM (
  -- @tr {String, Number}
  SELECT firstName AS name, age FROM students
) AS s 
-- @tr {*} -> {*, Grouped[String], *}
GROUP BY name;
```

## Type Annotation syntax

<!-- TODO: write a preamble -->

```plaintext  title="Type annotation syntax"
```
