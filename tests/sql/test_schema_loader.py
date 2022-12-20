import pytest

from chai_sql.models import (
    ChaiSqlAttribute,
    ChaiSqlRelation,
    ChaiSqlSchema,
    ChaiSqlType,
)
from chai_sql.sql.schema_loader import load

BASE_TEST_CHAISQL_SCHEMA = """
types: [Number, String, Boolean]

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
def test_load_schema(test_schema_str, expected_schema):
    schema = load(test_schema_str)
    assert schema.as_dict() == expected.as_dict()
