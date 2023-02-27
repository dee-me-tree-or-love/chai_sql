# Type Annotations for SQL

This document describes the type annotation syntax used by ChaiSQL.

> 💡 With ChaiSQL being under development, this document represents the
> initial, but consolidated design decisions.

## Examples

```sql  title="Type annotation in SQL"
-- @chaisql:check

-- @chaisql:newtype Name = String

-- @chaisql:returns DbView <bag> {id: Number, name: Name}
SELECT id, name
FROM person;
```

<details>
<summary>Example discussed:</summary>

```sql
-- -#- This command instructs the ChaiSQL engine to perform the checking:
-- @chaisql:check

-- -#- This command creates a type alias Name, equating it to a String type:
-- @chaisql:newtype Name = String

-- -#- This command specifies the expected type of the query result:
-- @chaisql:returns DbView <bag> {id: Number, name: Name}
-- -#- Since we select, id, and name from the person table, we know the types:
SELECT id, name
FROM person;
```

</details>

### Syntactic sugar

To reduce the amount of typing, type annotations are designed to support syntactic sugar.

```sql  title="Type annotation in SQL, syntactic sugar"
-- @chai:!

-- @chai:+ Name = String

-- @chai:~ DbView <bag> {id: Number, name: Name}
SELECT id, name
FROM person;
```

> Full syntactic sugar reference is available [below](#syntactic-sugar-reference).

## Type Annotation syntax

The comment-based type annotations allow to specify type hints without
modifying any existing SQL query syntax, allowing, therefore, to execute
the query in a RDBMS directly.

### High level overview

Below is the general structure of ChaiSQL annotations.

```elixir
(-- @chaisql:<meta commands>)*
(-- @chaisql:<annotation command>)?
<SQL Query>;
```

### ChaiSQL commands: Trigger, Alias, Type Hint

#### ChaiSQL trigger

To inform the ChaiSQL that the SQL queries provided in the file need to be
checked, it requires an explicit command, before any other type-related
information is provided. The command is specified as follows:

```sql
-- @chaisql:check
```

> This informs the ChaiSQL process to check the provided SQL file.

#### ChaiSQL type alias

Motivated by the need to, e.g., reuse a complex type in many places, or give a
more descriptive identity to the underlying type, it may be convenient to
declare a custom alias for a primitive or compound type.

This is done as follows:

```sql
-- @chaisql:newtype [Alias] = [Primitive or compound type]
```

This informs the ChaiSQL to associate a new type `[Alias]` with `[Primitive or compound type]`.
A number of restrictions apply, requiring unique `[Alias]` declarations - same `[Alias]` cannot be
declared more than once. And that `[Primitive or compound type]` may reuse other aliases, but
should resolve to a primitive or compound type without any aliases. Furthermore, `[Alias`] should
consist of alphanumeric values, including `_` and `-`.

To illustrate this, consider the example:

```sql
-- @chaisql:newtype Key = Number
```

> this declares a new type alias Key.

```sql
-- @chaisql:newtype IdView = DbView <bag> {id: Key}
```

> this declares a new type alias IdView, linked to a compound type, reusing the Key type alias.

#### ChaiSQL type hint

This command specifies the expected type of the result of an SQL SELECT query.
It can be used to annotate both nested and top-level queries. ChaiSQL will use
these type hints to check the type-safety of the provided query. It should be
done as follows:

```sql
-- @chaisql:returns [Primitive, compound, or alias type]
<SQL SELECT to annotate>
```

This command specifies a type hint for the SQL SELECT below. It must always
appear directly above the SQL query that it annotates. The
`[Primitive, compound, or alias type]` part may contain any well-written type
expression, be it a primitive type, a compound, or a type alias.

Consider the following example:

```sql
-- @chaisql:returns DbView <bag> {name: String}
SELECT name FROM person;
```

##### Set/bag notations elaborated

ChaiSQL’s compound type `DbView <[notation: bag|set]]> {[key]: [type], ...}`
expects the notation, either `<set>` or `<bag>`, as its first type argument.

This information is added to make the result type of the `SELECT` queries more
detailed. In particular, it illustrates the use of SQL’s native bag or set
semantics of the query results.

In short, `bag`s allow duplicates in the result, meanwhile `set`s ensure that
all duplicates are removed. This becomes relevant, when we attempt to
distinguish between SQL’s `SELECT` and `SELECT DISTINCT`, or the SQL’s query
operators `UNION/INTERSECT/EXCEPT` and `UNION/INTERSECT/EXCEPT ALL`.

1. `SELECT` without `DISTINCT`:

    Every regular `SELECT` query results in a `DbView <bag> {[key]: [type], ...}`.

    ```sql  title="Type hint example, SELECT *"
    -- @chaisql:check

    -- @chaisql:returns DbView <bag> {name: String}
    SELECT name
    FROM person;
    ```

    > In this case, the result of the query may contain duplicate names.

2. `SELECT` *with* `DISTINCT`

    Every regular SELECT query, results in a DbView <bag> {[key]: [type], ...}.

    ```sql
    -- @chaisql:check
    -- @chaisql:returns DbView <set> {name: String}
    SELECT DISTINCT name
    FROM person;
    ```

    > In this case, the result of the query does not contain any duplicate names.

3. `UNION/INTERSECT/EXCEPT` without `ALL`

    > Note: although here we consider `UNION` as the main example for the coming
      two cases, the same principle applies if we replace it with
      `INTERSECT` or `EXCEPT` SQL query operators.

    By the semantics of `UNION/INTERSECT/EXCEPT` (accepted by most RDBMS),
    unless the SQL author specifies `ALL` modifiers to this operator, the
    result of the query expression follows set semantics.

    > Following the SQL language specification (accepted by most RDBMS), to use
      this operator, both the left hand side and the right hand side query must
      have equal number and order of the returned columns, with compatible data
      types. This, therefore, should also be captured at the type hint level.

    ```sql title="UNION operator, without all"
    -- @chaisql:check

    -- @chaisql:returns DbView <set> {name: String}
    (
      -- @chaisql:returns DbView <bag> {name: String}
      SELECT name FROM person
      -- -#- Here we select all names from the person table.
    )
    UNION
    (
      -- @chaisql:returns DbView <bag> {name: String}
      SELECT name FROM cat
      -- -#- Here we select all names from the cat table.
    );
    ```

    > As a result, we select the **set** of all names from the person, or cat table.

4. `UNION/INTERSECT/EXCEPT` **with** `ALL`

    In contrast to the previous example, where we use the SQL query operators
    without the `ALL` modifier, here we explicitly mention it, to
    allow duplicate results.

    ```sql title="UNION operator, with all"
    -- @chaisql:check

    -- @chaisql:returns DbView <bag> {name: String}
    (
      -- @chaisql:returns DbView <bag> {name: String}
      SELECT name FROM person
      -- -#- Here we select all names from the person table.
    )
    UNION ALL
    (
      -- @chaisql:returns DbView <bag> {name: String}
      SELECT name FROM cat
      -- -#- Here we select all names from the cat table.
    );
    ```

    > As a result, we select the **bag** of all names from the person, or cat table.
    > These names may be duplicates, without being filtered out.

### Some examples

1. `SELECT *`:

    The asterisk, SELECT *, selects all columns from the “from” clause of the query.

    ```sql  title="Type hint example, SELECT *"
    -- @chaisql:check

    -- @chaisql:returns DbView <bag> {id: Number, name: String}
    SELECT *
    FROM person;
    ```

2. Direct column access in `SELECT`:

    The `SELECT` queries with direct access to the column names are the bread
    and butter of any SQL programmer. This type of queries, specifies precisely
    which columns are projected from the `FROM` clause of the query.

    ```sql  title="Type hint example, specific columns"
    -- @chaisql:check

    -- @chaisql:returns DbView <bag> {name: String}
    SELECT name
    FROM person;
    ```

3. Aliased column access in `SELECT`:

    This select variation allows the programmer to override the name of
    the selected column with custom aliases.

    ```sql  title="Type hint example, column alias"
    -- @chaisql:check

    -- @chaisql:returns DbView <bag> {personName: String}
    SELECT name AS personName
    FROM person;
    ```

4. Fully-qualified column access in `SELECT`

    Besides specifying aliases, SQL allows the fully-qualified column access,
    where the column is preceded by the table name. In such cases, only the
    column name needs to be specified in the type hint.

    ```sql  title="Type hint example, fully qualified column access"
    -- @chaisql:check

    -- @chaisql:returns DbView <bag> {name: String}
    SELECT p.name
    FROM person AS p;
    ```

5. Specifying the type of a sub-query:

    The type hints are always scoped to the nearest query below. Thus, this
    can be used in any cases, where a subquery is relevant and a type hint
    is beneficial.

    ```sql  title="Type hint example, sub-query"
    -- @chaisql:check

    -- @chaisql:returns DbView <bag> {name: String}
    SELECT name
    FROM person
    WHERE id IN (
      -- @chaisql:returns DbView <set> {personId: Number}
      SELECT DISTINCT personId
      FROM catFriends
    );
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
>   - [x] parser
>   - [ ] evaluation

- TODO: develop a syntactic sugar reference

| Syntactic sugar alias | Full version | Purpose |
| --------------------- | ------------ | ------- |
| `@cs:<command>` | `@chaisql:<command>` | Invoke any `chaisql` command |
| `@chai:<command>` | `@chaisql:<command>` | Invoke any `chaisql` command |
| `@<chaisql/alias>:!` | `@chaisql:check` | Tag file for checking |
| `@<chaisql/alias>:~` | `@chaisql:returns` | Specify type hint |
| `@<chaisql/alias>:+` | `@chaisql:newtype` | Declare new type alias |
