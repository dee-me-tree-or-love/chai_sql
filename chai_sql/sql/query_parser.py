from typing import Generic, Iterator, Sequence, TypeVar

# See Colab for experiments with `sqlparse`
#   https://colab.research.google.com/drive/100hW8Qw5iSNafhi54Ucriw1SA0bdBgjX?usp=sharing
import sqlparse

from chai_sql.models import KnownSqlCommand, SqlAst, SqlAstNode

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


def _sqlparse_statement_2_known_command(
    s: sqlparse.sql.Statement,
) -> KnownSqlCommand:
    """
    Determines whether the passed SQL statement is a known option.

    Examples:
        >>> simple_sql = "select * from foo;"
        >>> statement_1 = _get_sqlparse_asts(simple_sql)[0]
        >>> _sqlparse_statement_2_known_command(statement_1)
        <KnownSqlCommand.SELECT: 1>

        >>> complex_sql = "insert into foo values 'baz';"
        >>> statement_2 = _get_sqlparse_asts(complex_sql)[0]
        >>> _sqlparse_statement_2_known_command(statement_2)
        <KnownSqlCommand.NOT_SUPPORTED: 0>
    """
    try:
        return KnownSqlCommand[s.get_type().upper()]
    except KeyError:
        # TODO(tech-debt): add logging
        return KnownSqlCommand.NOT_SUPPORTED


class SqlParseSqlAst(SqlAst[sqlparse.sql.Statement], Generic[T]):
    def roots(self):
        """
        Examples:
            >>> simple_sql = "select * from foo;"
            >>> statement = _get_sqlparse_asts(simple_sql)[0]
            >>> sql_tree = SqlParseSqlAst(statement, KnownSqlCommand.SELECT)
            >>> children = sql_tree.roots()
            >>> next(children)
            SqlAstNode(children=<...>, nr_children=0, value='select', family='Token.Keyword.DML')
            >>> next(children)
            SqlAstNode(children=<...>, nr_children=0, value=' ', family='Token.Text.Whitespace')
            >>> next(children)
            SqlAstNode(children=<...>, nr_children=0, value='*', family='Token.Wildcard')
            >>> next(children)
            SqlAstNode(children=<...>, nr_children=0, value=' ', family='Token.Text.Whitespace')
            >>> next(children)
            SqlAstNode(children=<...>, nr_children=0, value='from', family='Token.Keyword')
            >>> next(children)
            SqlAstNode(children=<...>, nr_children=0, value=' ', family='Token.Text.Whitespace')
            >>> n = next(children)
            >>> n
            SqlAstNode(children=<...>, nr_children=1, value='foo', family='None')
            >>> next(n.children)
            SqlAstNode(children=<...>, nr_children=0, value='foo', family='Token.Name')
            >>> next(children)
            SqlAstNode(children=<...>, nr_children=0, value=';', family='Token.Punctuation')
            >>> try: next(children)
            ... except StopIteration: print('Iterator exhausted')
            Iterator exhausted
        """
        return map(self.__token_2_node, self.source.tokens)

    def __token_2_node(self, token: sqlparse.sql.Statement) -> SqlAstNode:
        children_source = token.tokens if token.is_group else []
        return SqlAstNode(
            children=map(self.__token_2_node, children_source),
            nr_children=len(children_source),
            value=token.value,
            family=str(token.ttype),
        )


def _sqlparse_ast_2_sql(s: sqlparse.sql.Statement) -> SqlParseSqlAst:
    return SqlParseSqlAst(s, _sqlparse_statement_2_known_command(s))


def parse(raw_sql: str) -> Iterator[SqlParseSqlAst]:
    """
    Returns an Abstract Syntax Tree represtentation of the raw SQL queries.
    Each SQL statement in the SQL query becomes a separate item yielded by the iterator.

    Args:
        raw_sql (str): an SQL query.

    Returns:
        Iterator[SqlAst]: an iterator with custom SqlAst objects

    Examples:
        >>> l1 = list(parse("select * from foo;"))
        >>> (len(l1), l1)
        (1, [SqlParseSqlAst(source=<...>, command=<KnownSqlCommand.SELECT: 1>)])

        >>> l2 = list(parse("select * from foo; select * from bar;"))
        >>> (len(l2), l2)
        (2, [SqlParseSqlAst(source=<...>, command=<KnownSqlCommand.SELECT: 1>), SqlParseSqlAst(source=<...>, command=<KnownSqlCommand.SELECT: 1>)])
    """
    asts = _get_sqlparse_asts(raw_sql)
    return (_sqlparse_ast_2_sql(s) for s in asts)
