TMPD_DIR=.tmp
TEST_DB=test.db
DATA_DIR=data

# Utility targets
# ~~~~~~~~~~~~~~~

${TMPD_DIR}/${TEST_DB}:
	@echo "🏗️ Creating SQLite db..."
	sqlite3 -init ${DATA_DIR}/bootstrap.sql ${TMPD_DIR}/${TEST_DB} ""


# User facing targets
# ~~~~~~~~~~~~~~~~~~~

# SQLite management
# -----------------

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
# ------

.PHONY: serve-docs
serve-docs:
	poetry run mkdocs serve