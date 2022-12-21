from typing import Any, TypeAlias

from arpeggio.cleanpeg import ParserPEG

from chai_sql.models import ParserWrapper

# TODO: define what is the return of the parser here
ArpeggioParserWrapper: TypeAlias = ParserWrapper[ParserPEG, Any]


def wrap_arpeggio_parser(
    peg_grammar: str, start_rule: str, debug=False
) -> ArpeggioParserWrapper:
    """
    Prepares the Arpeggio-based type info parser.

    Returns:
        ParserPEG: an Arpeggio parser

    Examples:
    >>> grammar = 'greeting = "hello" / "hi"'
    >>> start = "greeting"
    >>> parser = wrap_arpeggio_parser(grammar, start)
    >>> parser.parse("hello")
    [  'hello' [0] ]
    >>> parser.parse("hi")
    [  'hi' [0] ]
    """
    parser = ParserPEG(peg_grammar, start_rule, debug=debug)
    return ParserWrapper(parser)
