TMPD_DIR=.tmp
TEST_DB=test.db
DATA_DIR=data

# User facing targets
# ~~~~~~~~~~~~~~~~~~~

# Project dev tools
# `````````````````

.PHONY: style-fix
style-fix: black-fix isort-fix

.PHONY: isort-fix
isort-fix:
	poetry run isort .

.PHONY: black-fix
black-fix:
	poetry run black .

.PHONY: unit-test
unit-test:
	poetry run pytest --doctest-modules

.PHONY: mypy-check
mypy-check:
	poetry run mypy typed_sql


# SQLite management
# `````````````````

.PHONY: init-test-sqlite
init-test-sqlite: ${TMPD_DIR}/${TEST_DB}
	@echo "✅ Done!"

.PHONY: delete-test-sqlite
delete-test-sqlite:
	@echo "🪣 Removing sqlite data..."
	rm -rf ${TMPD_DIR}/${TEST_DB}
	@echo "🧹 Done!"

.PHONY: clean-test-sqlite
clean-test-sqlite: delete-test-sqlite init-test-sqlite

.PHONY:	run-test-sqlite-queries
run-test-sqlite-queries: ${TMPD_DIR}/${TEST_DB}
	sqlite3 ${TMPD_DIR}/${TEST_DB} < ${DATA_DIR}/queries.sql

# MKDocs
# ``````

.PHONY: serve-docs
serve-docs:
	poetry run mkdocs serve

# Utility targets
# ~~~~~~~~~~~~~~~

${TMPD_DIR}/${TEST_DB}:
	@echo "🏗️ Creating SQLite db..."
	sqlite3 -init ${DATA_DIR}/bootstrap.sql ${TMPD_DIR}/${TEST_DB} ""
