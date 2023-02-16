import pytest

from chai_sql.typing.type_parser import get_default_parser, parse

__BASE_PARSER__ = get_default_parser()


@pytest.fixture
def base_parser():
    return __BASE_PARSER__


# TODO(tech-debt): fix up repetition and pattern duplication


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
        # specific command check
        assert result[0][3][0].rule_name == "app_check"
        assert result[0][3][0].value.startswith("check")
        assert result[0][3][0].flat_str().startswith("check")

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
        # specific command check
        assert result[0][3][0].rule_name == "app_check"
        assert result[0][3][0].value.startswith("check")
        assert result[0][3][0].flat_str().startswith("check")
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
        # specific command check
        assert result[0][3][0].rule_name == "app_returns"
        assert result[0][3][0].value.startswith("returns")
        assert result[0][3][0].flat_str().startswith("returns")
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
        # specific command check
        assert result[0][3][0].rule_name == "app_newtype"
        assert result[0][3][0].value.startswith("newtype")
        assert result[0][3][0].flat_str().startswith("newtype")
        # the input is nested inside the command
        assert result[0][3][0][1].rule_name == "app_type_reference"
        assert result[0][3][0][1].value == "A | g | e"
        assert result[0][3][0][1].flat_str() == "Age"
        assert result[0][3][0][2].rule_name == ""
        assert result[0][3][0][2].value == "="
        assert result[0][3][0][3].rule_name == "app_type_reference"
        assert result[0][3][0][3].value == "N | u | m | b | e | r"
        assert result[0][3][0][3].flat_str() == "Number"


class TestAliasSyntax:
    def test_parse_start(self, base_parser):
        text = "@cs:ck"
        result = parse(text, base_parser)
        assert result[0].rule_name == "app_control"
        assert result[0].value == "@ | cs | : | ck"
        assert result[0][0].rule_name == "trigger"
        assert result[0][0].value == "@"
        assert result[0][1].rule_name == "app_reference"
        assert result[0][1].value == "cs"
        #  result[0][2] is matched by ":", which has no name
        assert result[0][2].rule_name == ""
        assert result[0][2].value == ":"
        assert result[0][3].rule_name == "app_command"
        assert result[0][3].value == "ck"
        # specific command check
        assert result[0][3][0].rule_name == "app_check"
        assert result[0][3][0].value.startswith("ck")
        assert result[0][3][0].flat_str().startswith("ck")

    def test_parse_start_with_schema(self, base_parser):
        text = "@cs:ck(schema.foo)"
        result = parse(text, base_parser)
        assert result[0].rule_name == "app_control"
        assert result[0].value.startswith("@ | cs | : | ck")
        assert result[0][0].rule_name == "trigger"
        assert result[0][0].value == "@"
        assert result[0][1].rule_name == "app_reference"
        assert result[0][1].value == "cs"
        # result[0][2] is matched by ":", which has no name
        assert result[0][2].rule_name == ""
        assert result[0][2].value == ":"
        assert result[0][3].rule_name == "app_command"
        assert result[0][3].value.startswith("ck")
        # specific command check
        assert result[0][3][0].rule_name == "app_check"
        assert result[0][3][0].value.startswith("ck")
        assert result[0][3][0].flat_str().startswith("ck")
        # the input is nested inside the command
        assert result[0][3][0][1].rule_name == ""
        assert result[0][3][0][1].value == "("
        assert result[0][3][0][2].rule_name == "app_schema_input"
        assert result[0][3][0][2].value == "s | c | h | e | m | a | . | f | o | o"
        assert result[0][3][0][2].flat_str() == "schema.foo"
        assert result[0][3][0][3].rule_name == ""
        assert result[0][3][0][3].value == ")"

    def test_parse_returns_number(self, base_parser):
        text = "@cs:~ Number"
        result = parse(text, base_parser)
        assert result[0].rule_name == "app_control"
        assert result[0].value.startswith("@ | cs | : | ~")
        assert result[0][0].rule_name == "trigger"
        assert result[0][0].value == "@"
        assert result[0][1].rule_name == "app_reference"
        assert result[0][1].value == "cs"
        # result[0][2] is matched by ":", which has no name
        assert result[0][2].rule_name == ""
        assert result[0][2].value == ":"
        assert result[0][3].rule_name == "app_command"
        assert result[0][3].value.startswith("~")
        assert result[0][3].flat_str().startswith("~")
        # specific command check
        assert result[0][3][0].rule_name == "app_returns"
        assert result[0][3][0].value.startswith("~")
        assert result[0][3][0].flat_str().startswith("~")
        # the input is nested inside the command
        assert result[0][3][0][1].rule_name == "app_type_reference"
        assert result[0][3][0][1].value == "N | u | m | b | e | r"
        assert result[0][3][0][1].flat_str() == "Number"

    def test_parse_newtype_age_is_number(self, base_parser):
        text = "@cs:+ Age = Number"
        result = parse(text, base_parser)
        assert result[0].rule_name == "app_control"
        assert result[0].value.startswith("@ | cs | : | +")
        assert result[0][0].rule_name == "trigger"
        assert result[0][0].value == "@"
        assert result[0][1].rule_name == "app_reference"
        assert result[0][1].value == "cs"
        # result[0][2] is matched by ":", which has no name
        assert result[0][2].rule_name == ""
        assert result[0][2].value == ":"
        assert result[0][3].rule_name == "app_command"
        assert result[0][3].value.startswith("+")
        assert result[0][3].flat_str().startswith("+")
        # specific command check
        assert result[0][3][0].rule_name == "app_newtype"
        assert result[0][3][0].value.startswith("+")
        assert result[0][3][0].flat_str().startswith("+")
        # the input is nested inside the command
        assert result[0][3][0][1].rule_name == "app_type_reference"
        assert result[0][3][0][1].value == "A | g | e"
        assert result[0][3][0][1].flat_str() == "Age"
        assert result[0][3][0][2].rule_name == ""
        assert result[0][3][0][2].value == "="
        assert result[0][3][0][3].rule_name == "app_type_reference"
        assert result[0][3][0][3].value == "N | u | m | b | e | r"
        assert result[0][3][0][3].flat_str() == "Number"
