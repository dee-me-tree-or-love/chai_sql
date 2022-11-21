TMPD_DIR=.tmp
TEST_DB=test.db

# Core targets

.tmp/test.db:
	sqlite3 -init resources/bootstrap.sql ${TMPD_DIR}/${TEST_DB} ""

# User convenience targets:

.PHONY: init-test-sqlite
init-test-sqlite: .tmp/test.db

.PHONY: delete-test-sqlite
delete-test-sqlite:
	rm -rf ${TMPD_DIR}/${TEST_DB}