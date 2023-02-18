from typing import Any, Dict

import pytest

from chai_sql.models import RoseTree, SqlAst, SqlAstNode
from chai_sql.sql.query_parser import parse

__base_parse_checks__: Dict[str, Any] = {
    "ast": (True, lambda x: isinstance(x, SqlAst)),
    "rose-trees": (
        True,
        lambda x: all((isinstance(t, RoseTree) for t in x.tree.dfs_iter())),
    ),
}


@pytest.mark.parametrize(
    "test_input,check_map",
    [
        (
            "select 1;",
            {
                **__base_parse_checks__,
                **{
                    "node-values": (
                        ["select", "1"],
                        lambda x: (
                            [
                                t.node.value.lower()
                                for t in x.tree.dfs_iter()
                                if "keyword" in t.node.kind.lower()
                                or "literal" in t.node.kind.lower()
                            ]
                        ),
                    ),
                },
            },
        ),
        (
            "select * from cats;",
            {
                **__base_parse_checks__,
                **{
                    "values": (
                        ["select", "*", "from", "cats"],
                        lambda x: (
                            [
                                t.node.value.lower()
                                for t in x.tree.dfs_iter()
                                if "keyword" in t.node.kind.lower()
                                or "wildcard" in t.node.kind.lower()
                                or "name" in t.node.kind.lower()
                            ]
                        ),
                    ),
                },
            },
        ),
        (
            "-- foo\nselect name, age from cats;",
            {
                **__base_parse_checks__,
                **{
                    "values": (
                        ["select", "name", "age", "from", "cats"],
                        lambda x: (
                            [
                                t.node.value.lower()
                                for t in x.tree.dfs_iter()
                                if "keyword" in t.node.kind.lower()
                                or "wildcard" in t.node.kind.lower()
                                or "name" in t.node.kind.lower()
                            ]
                        ),
                    ),
                },
            },
        ),
    ],
)
def test_parse_sql_2_ast(test_input, check_map):
    parse_result = parse(test_input)
    for ast in parse_result:
        for key, (expected, check) in check_map.items():
            print(f"Expectation: {key} -> checking...")
            assert check(ast) == expected
            print(f"Expectation: {key} -> ok")


def test_comment_info_in_parse_sql_2_ast():
    commented_sql = "-- Foo, Bar, Bop\nselect *, name from cats;"
    parse_result = parse(commented_sql)
    ast = next(parse_result)
    tree = ast.tree
    nodes = [t.node for t in tree.dfs_iter()]
    comments = [n for n in nodes if "comment" in n.kind.lower()]
    assert len(comments) == 1
    comment_values = [c.value.strip() for c in comments]
    assert comment_values == ["-- Foo, Bar, Bop"]
    identifiers = [
        n for n in nodes if "wildcard" in n.kind.lower() or "name" in n.kind.lower()
    ]
    identifier_values = [c.value.strip() for c in identifiers]
    assert identifier_values == ["*", "name", "cats"]
