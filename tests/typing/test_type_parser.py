import pytest

from chai_sql.typing.type_parser import _get_arpeggio_parser, parse

__ARPEGGIO_PARSER__ = _get_arpeggio_parser()


@pytest.fixture
def base_parser():
    return __ARPEGGIO_PARSER__


def test_parse_start(base_parser):
    text = "@chai_sql:check"
    result = parse(text, base_parser)
    assert result[0].rule_name == "app_control"
    assert result[0].value == "@ | chai_sql | : | check"
    assert result[0][0].rule_name == "trigger"
    assert result[0][0].value == "@"
    assert result[0][1].rule_name == "app_reference"
    assert result[0][1].value == "chai_sql"
    #  result[0][2] is matched by ":", which has no name
    assert result[0][2].rule_name == ""
    assert result[0][2].value == ":"
    assert result[0][3].rule_name == "app_command"
    assert result[0][3].value == "check"


def test_parse_start_with_schema(base_parser):
    text = "@chai_sql:check(schema.foo)"
    result = parse(text, base_parser)
    assert result[0].rule_name == "app_control"
    assert result[0].value.startswith("@ | chai_sql | : | check")
    assert result[0][0].rule_name == "trigger"
    assert result[0][0].value == "@"
    assert result[0][1].rule_name == "app_reference"
    assert result[0][1].value == "chai_sql"
    # result[0][2] is matched by ":", which has no name
    assert result[0][2].rule_name == ""
    assert result[0][2].value == ":"
    assert result[0][3].rule_name == "app_command"
    assert result[0][3].value.startswith("check")
    # the input is nested inside the check
    assert result[0][3][1].rule_name == ""
    assert result[0][3][1].value == "("
    assert result[0][3][2].rule_name == "app_input"
    assert result[0][3][2].value == "s | c | h | e | m | a | . | f | o | o"
    assert result[0][3][2].flat_str() == "schema.foo"
    assert result[0][3][3].rule_name == ""
    assert result[0][3][3].value == ")"
