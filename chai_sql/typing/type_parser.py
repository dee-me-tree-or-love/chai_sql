from dataclasses import dataclass
from typing import Any, Generic, TypeVar

from arpeggio.cleanpeg import ParserPEG

from chai_sql.models import ChaiSqlAst, RawSqlAst

# TODO:
#   Consider:
#       1. Arpeggio: http://textx.github.io/Arpeggio/2.0/
#       2. DHParser: https://gitlab.lrz.de/badw-it/DHParser

TYPE_GRAMMAR_PEG = """
// lang:cleanpeg
typer_statement = (app_control) EOF
// Base application controls
app_control = trigger app_reference ":" app_command
app_command = app_check
app_check = "check" ("(" app_input ")")?
app_input = r'[^()]'*
// Common pieces
trigger = "@"
app_reference = "chai_sql" / "chai" / "ChaiSQL" / "cs"
"""

P = TypeVar("P")
R = TypeVar("R")


@dataclass
class ParserWrapper(Generic[P, R]):
    parser: P

    def parse(self, text: str, *args, **kwargs) -> R:
        return self.parser.parse(text, *args, **kwargs)


def _get_arpeggio_parser() -> ParserWrapper[ParserPEG, Any]:
    """
    Prepares the Arpeggio-based type info parser.

    Returns:
        ParserPEG: an Arpeggio parser

    Examples:
        >>> parser = _get_arpeggio_parser()
        >>> parser.parse("@chai_sql:check")
        [ [ trigger '@' [0], [  'chai_sql' [1] ],  ':' [9], [  'check' [10] ] ], EOF [15] ]

        >>> parser.parse("@chai:check")
        [ [ trigger '@' [0], [  'chai' [1] ],  ':' [5], [  'check' [6] ] ], EOF [11] ]

        >>> parser.parse("@cs:check")
        [ [ trigger '@' [0], [  'cs' [1] ],  ':' [3], [  'check' [4] ] ], EOF [9] ]
    """
    opts = {
        # TODO: define a better approach for DEBUG settings
        # "debug":True
    }
    parser = ParserPEG(TYPE_GRAMMAR_PEG, "typer_statement", **opts)
    return ParserWrapper(parser)


def get_default_parser() -> ParserWrapper[ParserPEG, Any]:
    return _get_arpeggio_parser()


def parse(text: str, parser: ParserWrapper[P, R]) -> R:
    return parser.parse(text)


def annotate(sql: RawSqlAst) -> ChaiSqlAst:
    raise NotImplementedError()
