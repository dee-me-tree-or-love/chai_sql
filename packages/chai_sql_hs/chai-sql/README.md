# `chai-sql`

This is the Haskell implementation of ChaiSQL.

> :bulb: This package is a work in progress and serves to illustrate the
> capabilities of ChaiSQL. All suggestions and improvement ideas
> are very welcome! :dizzy:

## Setup

- [`./app`](./app/) - Defines the executable `chai-sql`
- [`./src`](./src/) - Defines the library/backend for `chai-sql`
  - [`./src/CLI`](./src/CLI/) - Defines the CLI interface and handling
  - [`./src/Language`](./src/Language/) - Defines the *ChaiSQL* language tooling

## Development

### 🚚 `stack build` - builds the project

### 💨 `stack run -- [Arguments]` - executes a freshly built project

#### Getting help

```bash
$ stack run -- --help
# TODO: provide once this is implemented
```

### 📦 `stack install` - to make *ChaiSQL* available globally

> After this, all examples in
> [section above](#-stack-run----arguments---executes-a-freshly-built-project)
> can replace `stack run --` with `chaisql-cli`

#### 🧰 `chaisql-cli` is available locally after installing

```bash
$ chaisql-cli --help
# TODO: provide once this is implemented
```
