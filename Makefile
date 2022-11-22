TMPD_DIR=.tmp
TEST_DB=test.db


# Core targets
# ~~~~~~~~~~~~

${TMPD_DIR}/${TEST_DB}:
	sqlite3 -init resources/bootstrap.sql ${TMPD_DIR}/${TEST_DB} ""


# User facing targets
# ~~~~~~~~~~~~~~~~~~~

.PHONY: init-test-sqlite
init-test-sqlite: ${TMPD_DIR}/${TEST_DB}
	@echo "✅ Done!"

.PHONY: delete-test-sqlite
delete-test-sqlite:
	rm -rf ${TMPD_DIR}/${TEST_DB}
	@echo "🧹 Done!"

.PHONY:	run-test-sqlite-queries
run-test-sqlite-queries: ${TMPD_DIR}/${TEST_DB}
	sqlite3 ${TMPD_DIR}/${TEST_DB} < resources/queries.sql

.PHONY: serve-docs
serve-docs:
	poetry run mkdocs serve