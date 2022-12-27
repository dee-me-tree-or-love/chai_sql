from dataclasses import dataclass
from typing import Any, Generic, Protocol, TypeVar

from chai_sql.models import (
    ChaiSqlAst,
    ChaiSqlSchema,
    GenericParserResult,
    GenericParserWrapper,
    SqlAst,
)
from chai_sql.shared.arpeggio_parser_wrapper import (
    ArpeggioParserWrapper,
    wrap_arpeggio_parser,
)

TYPE_GRAMMAR_PEG = """
// lang:cleanpeg
typer_statement = (app_control) EOF
// Base application controls
app_control = trigger app_reference ":" app_command
app_command = app_check / app_returns / app_newtype
app_check = app_check_alias ( "(" app_schema_input ")" )?
app_check_alias = "check" / "ck"
app_schema_input = r'[^\\(\\)]'*
app_returns = app_returns_alias app_type_reference
app_returns_alias = "returns" / "~"
app_newtype = app_newtype_alias app_type_reference "=" app_type_reference
app_newtype_alias = "newtype" / "+"
// TODO: review whether type references should be improved
app_type_reference = r'[a-zA-Z]'*
// Common pieces
trigger = "@"
app_reference = "chai_sql" / "chaisql" / "ChaiSQL" / "chai" / "cs"
"""


def _get_arpeggio_parser(debug=False) -> ArpeggioParserWrapper:
    """
    Prepares the Arpeggio-based type info parser.

    Returns:
        ArpeggioParserWrapper: a wrapped Arpeggio parser

    Examples:
        >>> parser = _get_arpeggio_parser()
        >>> parser.parse("@chai_sql:check")
        [ [ trigger '@' [0], [  'chai_sql' [1] ],  ':' [9], [ [ [  'check' [10] ] ] ] ], EOF [15] ]

        >>> parser.parse("@chai_sql:check(schema.foo)")
        [ [ trigger '@' [0], [  'chai_sql' [1] ],  ':' [9], [ [ [  'check' [10] ],  '(' [15], [  's' [16],  ...  'f' [23],  'o' [24],  'o' [25] ],  ')' [26] ] ] ], EOF [27] ]
    """
    return wrap_arpeggio_parser(TYPE_GRAMMAR_PEG, "typer_statement", debug=debug)


def get_default_parser(**kwargs) -> ArpeggioParserWrapper:
    return _get_arpeggio_parser(**kwargs)


def parse(text: str, parser: GenericParserWrapper) -> GenericParserResult:
    return parser.parse(text)


def translate(parse_result: GenericParserResult) -> ChaiSqlSchema:
    raise NotImplementedError()


def annotate(sql: SqlAst) -> ChaiSqlAst:
    raise NotImplementedError()
