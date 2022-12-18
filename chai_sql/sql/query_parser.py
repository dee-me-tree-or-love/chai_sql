from typing import Generic, Iterator, Sequence, TypeVar

# See Colab for experiments with `sqlparse`
#   https://colab.research.google.com/drive/100hW8Qw5iSNafhi54Ucriw1SA0bdBgjX?usp=sharing
import sqlparse

from chai_sql.models import KnownSqlOptions, RawSqlAst, RawSqlToken

T = TypeVar("T")


def _get_sqlparse_asts(t: str) -> Sequence[sqlparse.sql.Statement]:
    """Parse the raw SQL using `sqlparse`

    Args:
        t (str): raw sql text

    Returns:
        Sequence[sqlparse.sql.Statement]: collection of SQL Statements

    Examples:
        >>> simple_sql = "select * from foo;"
        >>> _get_sqlparse_asts(simple_sql)
        (<Statement 'select...' at 0x...>,)
        >>> _tokens = _get_sqlparse_asts(simple_sql)[0].tokens
        >>> next(_tokens[0].flatten())
        <DML 'select' at 0x...>

        >>> comments_in_sql = "-- bar\\nselect * from foo;"
        >>> _get_sqlparse_asts(comments_in_sql)
        (<Statement '-- bar...' at 0x...>,)

        >>> multiple_statements_in_sql = "-- bar\\nselect * from foo; select * from foobar;"
        >>> _get_sqlparse_asts(multiple_statements_in_sql)
        (<Statement '-- bar...' at 0x...>, <Statement 'select...' at 0x...>)
    """
    return sqlparse.parse(t)


def _sqlparse_statement_2_known_option(
    s: sqlparse.sql.Statement,
) -> KnownSqlOptions:
    """
    Determines whether the passed SQL statement is a known option.

    Examples:
        >>> simple_sql = "select * from foo;"
        >>> statement_1 = _get_sqlparse_asts(simple_sql)[0]
        >>> _sqlparse_statement_2_known_option(statement_1)
        <KnownSqlOptions.SELECT: 1>

        >>> complex_sql = "insert into foo values 'baz';"
        >>> statement_2 = _get_sqlparse_asts(complex_sql)[0]
        >>> _sqlparse_statement_2_known_option(statement_2)
        <KnownSqlOptions.NOT_SUPPORTED: 0>
    """
    try:
        return KnownSqlOptions[s.get_type().upper()]
    except KeyError:
        # TODO: add logging
        return KnownSqlOptions.NOT_SUPPORTED


class SqlParseRawSqlAst(RawSqlAst[sqlparse.sql.Statement], Generic[T]):
    def tokens(self):
        """
        Examples:
            >>> simple_sql = "select * from foo;"
            >>> statement_1 = _get_sqlparse_asts(simple_sql)[0]
            >>> sql_tree = SqlParseRawSqlAst(KnownSqlOptions.SELECT, statement_1)
            >>> tokens = sql_tree.tokens()
            >>> next(tokens)
            RawSqlToken(position=(0, 0), option='...Keyword.DML', value='select')
            >>> next(tokens)
            RawSqlToken(position=(1, 0), option='...Whitespace', value=' ')
            >>> next(tokens)
            RawSqlToken(position=(2, 0), option='...Wildcard', value='*')
            >>> next(tokens)
            RawSqlToken(position=(3, 0), option='...Whitespace', value=' ')
            >>> next(tokens)
            RawSqlToken(position=(4, 0), option='...Keyword', value='from')
            >>> next(tokens)
            RawSqlToken(position=(5, 0), option='...Whitespace', value=' ')
            >>> next(tokens)
            RawSqlToken(position=(6, 0), option='...Name', value='foo')
            >>> next(tokens)
            RawSqlToken(position=(7, 0), option='...Punctuation', value=';')
            >>> try: next(tokens)
            ... except StopIteration: print('Iterator exhausted')
            Iterator exhausted
        """
        return (
            RawSqlToken((index_group, index_elmnt), str(token.ttype), token.value)
            for index_group, group in enumerate(self.origin.tokens)
            for index_elmnt, token in enumerate(group.flatten())
        )


def _sqlparse_ast_2_sql(s: sqlparse.sql.Statement) -> SqlParseRawSqlAst:
    s_option = _sqlparse_statement_2_known_option(s)
    return SqlParseRawSqlAst(s_option, s)


def parse(raw_sql: str) -> Iterator[RawSqlAst]:
    """
    Returns an Abstract Syntax Tree-ish represtentation of the raw SQL queries.
    Each SQL statement in the SQL query becomes a separate item yielded by the iterator.

    Args:
        raw_sql (str): an SQL query.

    Returns:
        Iterator[RawSqlAst]: an iterator with custom RawSqlAst objects

    Examples:
        >>> l1 = list(parse("select * from foo;"))
        >>> (len(l1), l1)
        (1, [SqlParseRawSqlAst(option=<KnownSqlOptions.SELECT: 1>, origin=<...>)])

        >>> l2 = list(parse("select * from foo; select * from bar;"))
        >>> (len(l2), l2)
        (2, [SqlParseRawSqlAst(option=<KnownSqlOptions.SELECT: 1>, origin=<...>), SqlParseRawSqlAst(option=<KnownSqlOptions.SELECT: 1>, origin=<...>)])
    """
    asts = _get_sqlparse_asts(raw_sql)
    return (_sqlparse_ast_2_sql(s) for s in asts)
