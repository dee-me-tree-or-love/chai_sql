# `chai_sql_docs`

This is a documentation package for ChaiSQL, powered by `mkdocs`.

---

## Using [`poetry`](https://python-poetry.org/)

> To find out how to [install poetry, read here](https://python-poetry.org/docs/#installation).

The project configuration can be found in [`pyproject.toml`](./pyproject.toml).

### `poetry install` - setup all requirements

---

## For other development tasks: [`Makefile`](./Makefile)

The [`Makefile`](./Makefile) contains utility targets.

### `make docs-serve` - start a local mkdocs website

```bash
$ make docs-serve
...
INFO     -  Documentation built in ... seconds
INFO     -  [13:42:35] Watching paths for changes: 'src', 'mkdocs.yml', ...
INFO     -  [13:42:35] Serving on http://127.0.0.1:8000/
INFO     -  [13:42:37] Browser connected: http://127.0.0.1:8000/
```

### `make docs-build` - generate mkdocs website files

```bash
$ make docs-build
...
INFO     -  Cleaning site directory
INFO     -  Building documentation to directory: <...>/site
...
INFO     -  Documentation built in ... seconds
# For a quick check of produced HTML outputs
$ ls ./site | grep html
404.html
index.html
```
