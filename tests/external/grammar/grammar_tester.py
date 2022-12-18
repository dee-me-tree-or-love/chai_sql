from typing import Any, Callable, Tuple

import click
from abnf import Rule


# FIXME: this seems to be broken
# FIXME: don't use ABNF, it seems problematic. Use EBNF instead? ANTLR uses EBNF...


# class FromFileGrammar(Rule):
#     pass

# def test_grammar(source: str, sample: str, assertion: Callable[Tuple, Any]):
#     grammar = FromFileGrammar.from_file(source)
#     result = grammar.parse(sample)
#     return assertion(result)
