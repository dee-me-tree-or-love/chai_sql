TMP_DIR=.tmp
TEST_DB=test.sqlite
DATA_DIR=data
OK_MSG="✅ Done!"
OK_CLEAN_MSG="🧹 Done!"

# SQLite management
# `````````````````

.PHONY: db-init-test-sqlite
db-init-test-sqlite: ${TMP_DIR}/${TEST_DB}
	@echo ${OK_MSG}

.PHONY: db-delete-test-sqlite
db-delete-test-sqlite:
	@echo "🪣 Removing sqlite data..."
	rm -rf ${TMP_DIR}/${TEST_DB}
	@echo ${OK_CLEAN_MSG}

.PHONY: db-clean-test-sqlite
db-clean-test-sqlite: db-delete-test-sqlite db-init-test-sqlite

.PHONY: db-exec-test-sqlite-queries
db-exec-test-sqlite-queries: ${TMP_DIR}/${TEST_DB}
	sqlite3 ${TMP_DIR}/${TEST_DB} < ${DATA_DIR}/queries.sql

.PHONY: db-exec-test-sqlite-queries-interactive
db-exec-test-sqlite-queries-interactive: ${TMP_DIR}/${TEST_DB}
	sqlite3 ${TMP_DIR}/${TEST_DB} -interactive

# Utility targets
# ~~~~~~~~~~~~~~~

${TMP_DIR}/${TEST_DB}:
	@echo "🏗️ Creating SQLite db..."
	sqlite3 -init ${DATA_DIR}/bootstrap.sql ${TMP_DIR}/${TEST_DB} ""