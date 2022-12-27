from typing import Any, Dict

import pytest

from chai_sql.models import SqlAst, SqlAstNode
from chai_sql.sql.query_parser import parse

__base_parse_checks__: Dict[str, Any] = {
    "tree": (True, lambda x: isinstance(x, SqlAst)),
    "tokens": (
        True,
        lambda x: all((isinstance(t, SqlAstNode) for t in x.dfs_nodes())),
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
                    "values": (
                        ["select", "1"],
                        lambda x: (
                            [
                                t.value.lower()
                                for t in x.dfs_nodes()
                                if "keyword" in t.family.lower()
                                or "literal" in t.family.lower()
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
                                t.value.lower()
                                for t in x.dfs_nodes()
                                if "keyword" in t.family.lower()
                                or "wildcard" in t.family.lower()
                                or "name" in t.family.lower()
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
                                t.value.lower()
                                for t in x.dfs_nodes()
                                if "keyword" in t.family.lower()
                                or "wildcard" in t.family.lower()
                                or "name" in t.family.lower()
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
    for ast in parse_result:
        nodes = list(ast.dfs_nodes())
        comments = [n for n in nodes if "comment" in n.family.lower()]
        assert ["-- Foo, Bar, Bop"] == [c.value.strip() for c in comments]
        identifiers = [
            n
            for n in nodes
            if "wildcard" in n.family.lower() or "name" in n.family.lower()
        ]
        assert ["*", "name", "cats"] == [c.value.strip() for c in identifiers]
