from typing import Iterator, Sequence

import sqlparse

from chai_sql.models import RawSqlAst


def _get_sqlparse_asts(t: str) -> Sequence[sqlparse.sql.Statement]:
    """Parse the raw SQL using `sqlparse`

    Args:
        t (str): raw sql text

    Returns:
        Sequence[sqlparse.sql.Statement]: collection of SQL Statements

    Examples:
        >>> simple_sql = "select * from foo;"
        >>> _get_sqlparse_asts(simple_sql) # doctest: +ELLIPSIS
        (<Statement 'select...' at 0x...>,)

        >>> comments_in_sql = "-- bar\\nselect * from foo;"
        >>> _get_sqlparse_asts(comments_in_sql) # doctest: +ELLIPSIS
        (<Statement '-- bar...' at 0x...>,)

        >>> multiple_statements_in_sql = "-- bar\\nselect * from foo; select * from foobar;"
        >>> _get_sqlparse_asts(multiple_statements_in_sql) # doctest: +ELLIPSIS
        (<Statement '-- bar...' at 0x...>, <Statement 'select...' at 0x...>)
    """
    return sqlparse.parse(t)


def _sqlparse_ast_2_sql(s: sqlparse.sql.Statement) -> RawSqlAst:
    raise NotImplementedError()


def parse(raw_sql: str) -> Iterator[RawSqlAst]:
    asts = _get_sqlparse_asts(raw_sql)
    return (_sqlparse_ast_2_sql(s) for s in asts)
