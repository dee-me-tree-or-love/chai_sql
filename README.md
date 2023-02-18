# Chai SQL

Internship project adding an optional type system to SQL

## Examples

<!--mkdocs-intro-examples-start-->
```sql  title="ChaiSQL basic example"
-- @chaisql:check

-- @chaisql:newtype Name = String
-- @chaisql:newtype Age = Number
-- @chaisql:newtype Remark = String

-- @chaisql:newtype PersonView = DbView<bag>[Name, Age, Remark]

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

See more in the [`./docs`](./docs/index.md)
<!--mkdocs-intro-examples-end-->

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

### Python implementation: [`./packages/chai_sql_py/README.md`](./packages/chai_sql_py/README.md)
