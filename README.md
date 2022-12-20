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

### Python project: `chai_sql`

#### We use [`poetry`](https://python-poetry.org/) to configure `chai_sql`

You can install it with `pip install poetry`.  
The project configuration can be found in [`pyproject.toml`](./pyproject.toml).

##### `poetry install` - to setup all requirements

#### `poetry run <dependency executable>` - to run an executable module with `poetry`

### For various tasks: [`Makefile`](./Makefile)

The [`Makefile`](./Makefile) contains utility targets for repository development tasks.
