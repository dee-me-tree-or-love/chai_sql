# `chai-sql`

This is the Haskell implementation of ChaiSQL.

> :bulb: This package is a work in progress and serves to illustrate the
> capabilities of ChaiSQL. All suggestions and improvement ideas
> are very welcome! :dizzy:

## Setup

- [`./app`](./app/) - Defines the executable `chaisql`
- [`./src`](./src/) - Defines the library/backend for `chaisql`
  - [`./src/CLI`](./src/CLI/) - Defines the CLI interface and handling
  - [`./src/ChaiMicroSql`](./src/ChaiMicroSql/) - Defines the *ChaiSQL* language tooling

## Running the project

### 💨 `stack run -- [arguments]` - executes a freshly built project with arguments

#### 💡 `stack run -- --help` - getting the project help

```bash
$ stack run -- --help
The chaisql program

chaisql [COMMAND] ... [OPTIONS]

...
```

## Installing the executable

### 📦 `stack install` - to make *ChaiSQL* available globally

> After this, all examples in the section above
> can use `chaisql` instead of `stack run --`

```bash
$ stack install
...
chai-sql> build (lib + exe)
...
Copied executables to ~/.local/bin:
- chaisql
```

#### 🧰 `chaisql` is now available locally

```bash
$ chaisql --help
The chaisql program

chaisql [COMMAND] ... [OPTIONS]

...
```

> :warning: If you no longer need the executable,
> you need to remove it from `~/.local/bin` manually.

## Development

> 💡 Quick tip: run `cat README.md | grep stack` to quickly locate all `stack` commands.

### 🚚 `stack build` - builds the project

### 🧪 `stack test` - executes the test suite

#### 🔬 `stack test --coverage` - executes the test suite with coverage report

### 📝 `stack exec -- doctest src` - checks the examples in the library documentation

### 📔 `stack exec -- haddock --html src/**/*.hs -o .docs` - builds the API documentation

### 🏃 `stack run -- --help` - compile and run the CLI

> See contents of [`./.samples`](./.samples/) directory for SQL examples.

#### 🔍 `stack run -- infer -p .samples/expression.sql -s .samples/schema.yaml` - run the inference

#### 🩺 `stack run -- check -p .samples/expression-typed-ok.sql -s .samples/schema.yaml` - run the type checking

### 🧹 `stack clean` - to remove build artifacts
