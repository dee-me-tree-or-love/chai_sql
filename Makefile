TMPD_DIR=.tmp
TEST_DB=test.db
DATA_DIR=data
POETRY_RUN=poetry run
OK_MSG="✅ Done!"
OK_CLEAN_MSG="🧹 Done!"

# User facing targets
# ~~~~~~~~~~~~~~~~~~~

# Project dev tools
# `````````````````

.PHONY: style-fix
style-fix sf: black isort

.PHONY: style-fix
style-check sc: FLAG=--check
style-check sc: black isort

.PHONY: unit-test
unit-test ut:
	${POETRY_RUN} pytest --doctest-modules

.PHONY: mypy-check
mypy-check mc:
	${POETRY_RUN} mypy .

.PHONY: clean-cache
clean-cache cc:
	find . \( -iname "__pycache__" -o -iname ".hypothesis" \) -print0 | xargs -0 rm -rf
	rm -rf .eggs *.egg-info/ .coverage build/ .cache .pytest_cache
	@echo ${OK_CLEAN_MSG}

.PHONY: isort
isort:
	${POETRY_RUN} isort ${FLAG} .

.PHONY: black
black:
	${POETRY_RUN} black ${FLAG} .


# MKDocs
# ``````

.PHONY: docs-serve
docs-serve:
	${POETRY_RUN} mkdocs serve

# SQLite management
# `````````````````

.PHONY: db-init-test-sqlite
db-init-test-sqlite: ${TMPD_DIR}/${TEST_DB}
	@echo ${OK_MSG}

.PHONY: db-delete-test-sqlite
db-delete-test-sqlite:
	@echo "🪣 Removing sqlite data..."
	rm -rf ${TMPD_DIR}/${TEST_DB}
	@echo ${OK_CLEAN_MSG}

.PHONY: db-clean-test-sqlite
db-clean-test-sqlite: db-delete-test-sqlite db-init-test-sqlite

.PHONY: db-exec-test-sqlite-queries
db-exec-test-sqlite-queries: ${TMPD_DIR}/${TEST_DB}
	sqlite3 ${TMPD_DIR}/${TEST_DB} < ${DATA_DIR}/queries.sql

# Utility targets
# ~~~~~~~~~~~~~~~

${TMPD_DIR}/${TEST_DB}:
	@echo "🏗️ Creating SQLite db..."
	sqlite3 -init ${DATA_DIR}/bootstrap.sql ${TMPD_DIR}/${TEST_DB} ""