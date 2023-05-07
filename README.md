# ChaiSQL

Internship project adding an optional type system to SQL.

> :building_construction: ChaiSQL is under heavy development.

## Examples

<!--mkdocs-intro-examples-start-->
```sql  title="ChaiSQL example"
-- @chaisql:check

-- @chaisql:newtype Name = String
-- @chaisql:newtype Age = Number

-- @chaisql:newtype PersonView = DbView <bag> {name: Name, age: Age}

-- @chaisql:returns PersonView
SELECT
    -- @chaisql:returns Name
    p.name,
    -- @chaisql:returns Age
    p.age
FROM people AS p;
```

<!--mkdocs-intro-examples-end-->

## Documentation

Information about ChaiSQL is available in:

- [`./docs`](./docs/) directory
- on the [MkDocs](https://www.mkdocs.org/)-powered website:
  [ChaiSQL Documentation](https://dee-me-tree-or-love.github.io/chai_sql/).

<!-- :: User content -->

<!--mkdocs-intro-details-start-->

## Installation

<!-- TODO: describe how to install `typed_sql` -->
*Coming soon!*

## Supported features

<!-- TODO: describe supported & planned features -->
*Coming soon!*

<!--mkdocs-intro-details-end-->

<!-- :: Developer content -->
## Development

The implementation of ChaiSQL is currently in progress and is at an early stage.  

The solutions may change and different experimental implementations (all WIP) are mentioned below.

> :bulb: These packages are aimed to provide same functionality and are
> used to validate the implementation options.

### Python implementation: [`./packages/chai_sql_py/`](./packages/chai_sql_py/)

> :snake:
> Python implementation aims to benefit all Python features and make the
> ChaiSQL tool easily extensible.

### Haskell implementation: [`./packages/chai_sql_hs/`](./packages/chai_sql_hs/)

> :beetle:
> Haskell implementation focuses on the Haskell's type system and powerful
> functional features.
