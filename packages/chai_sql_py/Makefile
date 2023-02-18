TMPD_DIR=.tmp
TEST_DB=test.sqlite
DATA_DIR=data
POETRY_RUN=poetry run
OK_MSG="✅ Done!"
OK_CLEAN_MSG="🧹 Done!"

# User facing targets
# ~~~~~~~~~~~~~~~~~~~

# Project dev tools
# `````````````````

.PHONY: full-check
full-check fc: style-fix unit-test mypy-check

.PHONY: style-check
style-check sc: FLAG=--check
style-check sc: black isort

.PHONY: style-fix
style-fix sf: black isort

.PHONY: unit-test
unit-test ut:
	${POETRY_RUN} pytest --doctest-modules

.PHONY: mypy-check
mypy-check mc:
	${POETRY_RUN} mypy .

.PHONY: isort
isort:
	${POETRY_RUN} isort ${FLAG} .

.PHONY: black
black:
	${POETRY_RUN} black ${FLAG} .

# Project setup tools
# ```````````````````

.PHONY: clean-cache
clean-cache cc:
	find . \( -iname "__pycache__" -o -iname ".hypothesis" \) -print0 | xargs -0 rm -rf
	rm -rf .eggs *.egg-info/ .coverage build/ .cache .pytest_cache
	@echo ${OK_CLEAN_MSG}