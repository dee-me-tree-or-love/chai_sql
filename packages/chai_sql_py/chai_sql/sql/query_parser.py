from typing import Generic, Iterator, Sequence, TypeVar

# See Colab for experiments with `sqlparse`
#   https://colab.research.google.com/drive/100hW8Qw5iSNafhi54Ucriw1SA0bdBgjX?usp=sharing
import sqlparse

from chai_sql.models import RoseTree, SqlAst, SqlAstNode, SqlCommandKind

T = TypeVar("T")


def _sqlparse_parse(t: str) -> Sequence[sqlparse.sql.Statement]:
    """Parse the raw SQL using `sqlparse`

    Args:
        t (str): raw sql text

    Returns:
        Sequence[sqlparse.sql.Statement]: collection of SQL Statements

    Examples:
        >>> simple_sql = "select * from foo;"
        >>> _sqlparse_parse(simple_sql)
        (<Statement 'select...' at 0x...>,)
        >>> _tokens = _sqlparse_parse(simple_sql)[0].tokens
        >>> next(_tokens[0].flatten())
        <DML 'select' at 0x...>

        >>> comments_in_sql = "-- bar\\nselect * from foo;"
        >>> _sqlparse_parse(comments_in_sql)
        (<Statement '-- bar...' at 0x...>,)

        >>> multiple_statements_in_sql = "-- bar\\nselect * from foo; select * from foobar;"
        >>> _sqlparse_parse(multiple_statements_in_sql)
        (<Statement '-- bar...' at 0x...>, <Statement 'select...' at 0x...>)
    """
    return sqlparse.parse(t)


def _sqlparse_statement_2_command(
    s: sqlparse.sql.Statement,
) -> SqlCommandKind:
    """
    Determines whether the passed SQL statement is a known option.

    Examples:
        >>> simple_sql = "select * from foo;"
        >>> statement_1 = _sqlparse_parse(simple_sql)[0]
        >>> _sqlparse_statement_2_command(statement_1)
        <SqlCommandKind.SELECT: 1>

        >>> complex_sql = "insert into foo values 'baz';"
        >>> statement_2 = _sqlparse_parse(complex_sql)[0]
        >>> _sqlparse_statement_2_command(statement_2)
        <SqlCommandKind.NOT_SUPPORTED: 0>
    """
    try:
        return SqlCommandKind[s.get_type().upper()]
    except KeyError:
        # TODO(tech-debt): add logging
        return SqlCommandKind.NOT_SUPPORTED


SqlParseRoseTree = RoseTree[sqlparse.sql.Token, SqlAstNode]


def _sqlparse_statement_2_node(statement: sqlparse.sql.Statement) -> SqlAstNode:
    return SqlAstNode(kind=str(statement.ttype), value=statement.value)


def _sqlparse_statement_2_tree(statement: sqlparse.sql.Statement) -> SqlParseRoseTree:
    subtree_base = statement.tokens if statement.is_group else []
    return SqlParseRoseTree(
        source=statement,
        node=_sqlparse_statement_2_node(statement),
        nr_children=len(subtree_base),
        children=list(map(_sqlparse_statement_2_tree, subtree_base)),
    )


def _sqlparse_2_sql_ast(s: sqlparse.sql.Statement) -> SqlAst:
    return SqlAst(_sqlparse_statement_2_command(s), _sqlparse_statement_2_tree(s))


def parse(raw_sql: str) -> Iterator[SqlAst]:
    """
    Returns an Abstract Syntax Tree represtentation of the raw SQL queries.
    Each SQL statement in the SQL query becomes a separate item yielded by the iterator.

    Args:
        raw_sql (str): an SQL query.

    Returns:
        Iterator[SqlAst]: an iterator with custom SqlAst objects

    Examples:
        >>> asts1 = list(parse("select * from foo;"))
        >>> (len(asts1), asts1)
        (1, [SqlAst(kind=<SqlCommandKind.SELECT: 1>, tree=RoseTree(...)])

        >>> l2 = list(parse("select * from foo; select * from bar;"))
        >>> (len(l2), l2)
        (2, [SqlAst(kind=<SqlCommandKind.SELECT: 1>, tree=RoseTree(...), SqlAst(kind=<SqlCommandKind.SELECT: 1>, tree=RoseTree(...)])
    """
    asts = _sqlparse_parse(raw_sql)
    return (_sqlparse_2_sql_ast(s) for s in asts)
