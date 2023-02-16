import pytest

from chai_sql.models import (
    ChaiSqlAttribute,
    ChaiSqlRelation,
    ChaiSqlSchema,
    ChaiSqlType,
)
from chai_sql.sql.schema_loader import get_default_parser, parse

__BASE_PARSER__ = get_default_parser()


@pytest.fixture
def base_parser():
    return __BASE_PARSER__


BASE_TEST_CHAISQL_SCHEMA = """
types: { Number, String, Boolean }

relation Cat: {
    id: Number,
    name: String,
}

relation Person: {
    id: Number,
    name: String,
    age: Number,
}
"""

BASE_EXPECTED_SCHEMA = ChaiSqlSchema(
    [ChaiSqlType("Number"), ChaiSqlType("String"), ChaiSqlType("Boolean")],
    [
        ChaiSqlRelation(
            "Cat",
            [
                ChaiSqlAttribute("id", ChaiSqlType("Number")),
                ChaiSqlAttribute("name", ChaiSqlType("String")),
            ],
        ),
        ChaiSqlRelation(
            "Person",
            [
                ChaiSqlAttribute("id", ChaiSqlType("Number")),
                ChaiSqlAttribute("name", ChaiSqlType("String")),
                ChaiSqlAttribute("age", ChaiSqlType("Number")),
            ],
        ),
    ],
)


@pytest.mark.parametrize(
    "test_schema_str,expected_schema",
    [(BASE_TEST_CHAISQL_SCHEMA, BASE_EXPECTED_SCHEMA)],
)
def test_parse_schema(base_parser, test_schema_str, expected_schema):
    result = parse(test_schema_str, base_parser)
    assert result.rule_name == "schema"
    assert result[0].rule_name == "schema_body"
