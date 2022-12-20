import pytest

from chai_sql.typing.type_parser import _get_arpeggio_parser, parse

__ARPEGGIO_PARSER__ = _get_arpeggio_parser()


@pytest.fixture
def base_parser():
    return __ARPEGGIO_PARSER__


class TestStandardSyntax:
    def test_parse_start(self, base_parser):
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

    def test_parse_start_with_schema(self, base_parser):
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
        # the input is nested inside the command
        assert result[0][3][0][1].rule_name == ""
        assert result[0][3][0][1].value == "("
        assert result[0][3][0][2].rule_name == "app_schema_input"
        assert result[0][3][0][2].value == "s | c | h | e | m | a | . | f | o | o"
        assert result[0][3][0][2].flat_str() == "schema.foo"
        assert result[0][3][0][3].rule_name == ""
        assert result[0][3][0][3].value == ")"

    def test_parse_returns_number(self, base_parser):
        text = "@chai_sql:returns Number"
        result = parse(text, base_parser)
        assert result[0].rule_name == "app_control"
        assert result[0].value.startswith("@ | chai_sql | : | returns")
        assert result[0][0].rule_name == "trigger"
        assert result[0][0].value == "@"
        assert result[0][1].rule_name == "app_reference"
        assert result[0][1].value == "chai_sql"
        # result[0][2] is matched by ":", which has no name
        assert result[0][2].rule_name == ""
        assert result[0][2].value == ":"
        assert result[0][3].rule_name == "app_command"
        assert result[0][3].value.startswith("returns")
        assert result[0][3].flat_str().startswith("returns")
        # the input is nested inside the command
        assert result[0][3][0][1].rule_name == "app_type_reference"
        assert result[0][3][0][1].value == "N | u | m | b | e | r"
        assert result[0][3][0][1].flat_str() == "Number"

    def test_parse_newtype_age_is_number(self, base_parser):
        text = "@chai_sql:newtype Age = Number"
        result = parse(text, base_parser)
        assert result[0].rule_name == "app_control"
        assert result[0].value.startswith("@ | chai_sql | : | newtype")
        assert result[0][0].rule_name == "trigger"
        assert result[0][0].value == "@"
        assert result[0][1].rule_name == "app_reference"
        assert result[0][1].value == "chai_sql"
        # result[0][2] is matched by ":", which has no name
        assert result[0][2].rule_name == ""
        assert result[0][2].value == ":"
        assert result[0][3].rule_name == "app_command"
        assert result[0][3].value.startswith("newtype")
        assert result[0][3].flat_str().startswith("newtype")
        # the input is nested inside the command
        assert result[0][3][0][1].rule_name == "app_type_reference"
        assert result[0][3][0][1].value == "A | g | e"
        assert result[0][3][0][1].flat_str() == "Age"
        assert result[0][3][0][2].rule_name == ""
        assert result[0][3][0][2].value == "="
        assert result[0][3][0][3].rule_name == "app_type_reference"
        assert result[0][3][0][3].value == "N | u | m | b | e | r"
        assert result[0][3][0][3].flat_str() == "Number"
