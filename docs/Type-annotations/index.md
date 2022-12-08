# Type Annotations for SQL

<!-- TODO: give an introduction -->
<!-- TODO: decide on the <package-name> for this project -->

## Type Annotation examples

<!-- TODO: write a preamble -->

```sql  title="Type annotation in SQL example"
-- @<TODO: package-name>:typecheck

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
-- @<TODO: package-name>:tc

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
<!-- TODO: develop a full syntax system -->

### Enabling the type checker `@<TODO: ackage-name>:typecheck`

```sql  title="Type annotation start command"
-- @<TODO: ackage-name>:typecheck
```

### Adding the type to select statements

#### `DbResult[<T>+]`

The standard way is by specifying `DbResult[<T>+]`

```sql  title="Type annotation start command"
-- @returns DbResult[String, Number]
SELECT p.name, p.age
FROM people AS p;
```

#### `{<T>+}`

With syntactic sugar it can be replaced by `{<T>+}`

```sql  title="Type annotation start command"
-- @returns {String, Number}
SELECT p.name, p.age
FROM people AS p;
```
