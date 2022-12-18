from typing import Any

import pytest

from chai_sql.models import RawSqlAst, RawSqlToken
from chai_sql.sql.query_parser import parse

__base_parse_checks__ = {
    "tree": (True, lambda x: isinstance(x, RawSqlAst)),
    "tokens": (
        True,
        lambda x: all((isinstance(t, RawSqlToken) for t in x.tokens())),
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
                                for t in x.tokens()
                                if "keyword" in t.option.lower()
                                or "literal" in t.option.lower()
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
                                for t in x.tokens()
                                if "keyword" in t.option.lower()
                                or "wildcard" in t.option.lower()
                                or "name" in t.option.lower()
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
                                for t in x.tokens()
                                if "keyword" in t.option.lower()
                                or "wildcard" in t.option.lower()
                                or "name" in t.option.lower()
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
            print(f"{key} -> checking...")
            assert check(ast) == expected
            print(f"{key} -> ok")


def test_comment_info_in_parse_sql_2_ast():
    commented_sql = "-- Foo, Bar, Bop\nselect *, name from cats;"
    parse_result = parse(commented_sql)
    for ast in parse_result:
        tokens = list(ast.tokens())
        comments = [t for t in tokens if "comment" in t.option.lower()]
        assert ["-- Foo, Bar, Bop"] == [c.value.strip() for c in comments]
        identifiers = [
            t
            for t in tokens
            if "wildcard" in t.option.lower() or "name" in t.option.lower()
        ]
        assert ["*", "name", "cats"] == [c.value.strip() for c in identifiers]
