# Type Annotations for SQL

<!-- TODO: provide an introduction -->
<!-- FIXME: describe the running example schema -->

## Type Annotation examples

```sql  title="Type annotation in SQL example"
-- @chaisql:check(my-db.schema)

-- @chaisql:returns DbView<bag>[String, Number]
SELECT name, AVG(age)
FROM (
  -- @chaisql:returns DbView<bag>[String, Number]
  SELECT firstName AS name, age FROM students
) AS s
-- TODO: define the type hints over grouping?
-- TODO: this is not a part of the "Basic SQL fragment..."
GROUP BY name;
```

### Syntactic sugar

To reduce the amount of typing, type annotations support extra syntactic sugar.

```sql  title="Sugary type annotation in SQL example"
-- @cs:check(my-db.schema)

-- @cs:~ (String, Number)
SELECT name, AVG(age)
FROM (
  -- @cs:~ (String, Number)
  SELECT firstName AS name, age FROM students
) AS s 
GROUP BY name;
```

Full syntactic sugar reference is available [below](#syntactic-sugar-reference).

## Type Annotation syntax

<!-- TODO: write a preamble -->

### Enabling the type checker

#### Without loading a DB schema

> - [ ] (backlog): Feature supported in Python `chai_sql`
>   - [x] parser
>   - [ ] evaluation

This method allows to only check abstract data types and types of constants.

```sql
-- @chaisql:check
```

#### With loading a DB schema

> - [ ] (backlog): Feature supported in Python `chai_sql`
>   - [x] parser
>   - [ ] evaluation

This method allows to check data types parameterized with a database schema.

```sql
-- @chaisql:check(<DB schema file reference>)
```

##### Supported schema formats

> - [ ] (backlog): Features supported in Python `chai_sql`
>   - [ ] parser for the database format
>   - [ ] evaluation

- [ ] [ChaiSQL Simple Schema]
- [ ] [Prisma](https://www.prisma.io/docs/concepts/components/prisma-schema)

### Specifying type hints

#### Returned types

> - [ ] (backlog): Feature supported in Python `chai_sql`
>   - [x] parser
>   - [ ] evaluation

```sql
-- @chaisql:returns <T>
<Query piece to be typed>
```

##### Some examples

1. Specifying the type of a `SELECT` query:

    ```sql  title="Type hint example, select"
    -- @chaisql:returns DbView<bag>[String, Number]
    SELECT p.name, p.age
    FROM people AS p;
    ```

2. Specifying the type of a term:

    ```sql  title="Type hint example, term"
    -- @chaisql:returns DbView<bag>[String, Number, String]
    SELECT
      -- @chaisql:returns String
      p.name,
      -- @chaisql:returns Number
      p.age,
      -- @chaisql:returns String
      "friend" AS class
    FROM people AS p;
    ```

3. Specifying the type of a sub-query:

    ```sql  title="Type hint example, sub-query"
    -- @chaisql:returns DbView<bag>[String, Number, String]
    SELECT
      p.name,
      p.age,
      p.class
    FROM (
      -- @chaisql:returns DbView<bag>[String, Number, String, String]
      SELECT
        ppl.name,
        ppl.age,
        "friend" AS class,
        "real" AS property
      FROM people AS ppl;
    ) AS p;
    ```

### Specifying type aliases

#### New type alias

> - [ ] (backlog): Feature supported in Python `chai_sql`
>   - [x] parser
>   - [ ] evaluation

```sql
-- @chaisql:newtype <T> = <R>
```

Note that the type aliases are evaluated regardless of their order.
So all the following examples are legal:

```sql
-- @chaisql:newtype FooView = DbView[Bar]
-- @chaisql:newtype Bar = String
```

and

```sql
-- @chaisql:newtype Bar = String
-- @chaisql:newtype FooView = DbView[Bar]
```

##### Example type aliases

1. Reusing the same type:

    ```sql  title="Type alias example, reuse"
    -- @chaisql:newtype PersonView = DbView<bag>[String, Number, String]

    -- @chaisql:returns PersonView
    SELECT
      p.name,
      p.age,
      p.class
    FROM (
      -- @chaisql:returns PersonView
      SELECT
        ppl.name,
        ppl.age,
        "friend" AS class
      FROM people AS ppl;
    ) AS p;
    ```

2. Providing extra meaning:

    ```sql  title="Type hint example, term"
    -- @chaisql:newtype Name = String
    -- @chaisql:newtype Age = Number
    -- @chaisql:newtype Remark = String

    -- @chaisql:returns DbView<bag>[Name, Age, Remark]
    SELECT
      -- @chaisql:returns Name
      p.name,
      -- @chaisql:returns Age
      p.age,
      -- @chaisql:returns Remark
      "friend" as class
    FROM people AS p;
    ```

3. Combining the examples above

    ```sql  title="Type hint example, term"
    -- @chaisql:newtype Name = String
    -- @chaisql:newtype Age = Number
    -- @chaisql:newtype Remark = String

    -- @chaisql:newtype PersonView = DbView<bag>[String, Number, String]

    -- @chaisql:returns PersonView
    SELECT
      -- @chaisql:returns Name
      p.name,
      -- @chaisql:returns Age
      p.age,
      -- @chaisql:returns Remark
      "friend" as class
    FROM people AS p;
    ```

## Syntactic sugar reference

> - [ ] (backlog): Feature supported in Python `chai_sql`
>   - [ ] parser
>   - [ ] evaluation

- TODO: develop a syntactic sugar reference

| Syntactic sugar alias | Full version | Purpose |
| --------------------- | ------------ | ------- |
| `@cs:<command>` | `@chaisql:<command>` | Invoke any `chaisql` command |
| `@chai:<command>` | `@chaisql:<command>` | Invoke any `chaisql` command |
| `@<chaisql/alias>:~` | `@chaisql:returns` | Specify type hint |
| `@<chaisql/alias>:+` | `@chaisql:newtype` | Declare new type alias |
