from dataclasses import dataclass
from typing import Any, Generic, TypeVar, Protocol

from arpeggio.cleanpeg import ParserPEG

from chai_sql.models import ChaiSqlAst, RawSqlAst

TYPE_GRAMMAR_PEG = """
// lang:cleanpeg
typer_statement = (app_control) EOF
// Base application controls
app_control = trigger app_reference ":" app_command
app_command = app_check / app_returns / app_newtype
app_check = "check" ("(" app_schema_input ")")?
app_schema_input = r'[^()]'*
app_returns = "returns" app_type_reference
app_newtype = "newtype" app_type_reference "=" app_type_reference
app_type_reference = r'[^(\\=\\.)]'*
// Common pieces
trigger = "@"
app_reference = "chai_sql" / "chaisql" / "ChaiSQL" / "chai" / "cs"
"""

T = TypeVar("T")


class Parseable(Protocol):
    def parse(self: T, *args: Any, **kwargs: Any) -> Any:
        ...


P = TypeVar("P", bound=Parseable)
R = TypeVar("R")


@dataclass
class ParserWrapper(Generic[P, R]):
    parser: P

    def parse(self, text: str, *args, **kwargs) -> R:
        return self.parser.parse(text, *args, **kwargs)


# TODO: define what is the return of the parser here
def _get_arpeggio_parser(debug=False) -> ParserWrapper[ParserPEG, Any]:
    """
    Prepares the Arpeggio-based type info parser.

    Returns:
        ParserPEG: an Arpeggio parser

    Examples:
        >>> parser = _get_arpeggio_parser()
        >>> parser.parse("@chai_sql:check")
        [ [ trigger '@' [0], [  'chai_sql' [1] ],  ':' [9], [ [  'check' [10] ] ] ], EOF [15] ]

        >>> parser.parse("@chai_sql:check(schema.foo)")
        [ [ trigger '@' [0], [  'chai_sql' [1] ],  ':' [9], [ [  'check' [10],  '(' [15], [  's' [16],  ...  'o' [25] ],  ')' [26] ] ] ], EOF [27] ]
    """
    # TODO: define a better approach for DEBUG settings
    parser = ParserPEG(TYPE_GRAMMAR_PEG, "typer_statement", debug=debug)
    return ParserWrapper(parser)


def get_default_parser(**kwargs) -> ParserWrapper[ParserPEG, Any]:
    return _get_arpeggio_parser(**kwargs)


def parse(text: str, parser: ParserWrapper[P, R]) -> R:
    return parser.parse(text)


def annotate(sql: RawSqlAst) -> ChaiSqlAst:
    raise NotImplementedError()
