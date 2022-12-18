from typing import Any, Callable, Tuple

import click
from abnf import Rule


class FromFileGrammar(Rule):
    pass


# FIXME: this seems to be broken
# FIXME: don't use ABNF, it seems problematic. Use EBNF instead? ANTLR uses EBNF...


def test_grammar(
    source: str, sample: str, assertion: Callable[Tuple["Node", int], Any]
):
    grammar = FromFileGrammar.from_file(source)
    result = grammar.parse(sample)
    return assertion(result)


@click.command()
@click.argument("input-grammar", type=click.Path(exists=True))
@click.argument("test-string", type=str)
def main(input_grammar, test_string):
    # try:
    result = test_grammar(input_grammar, test_string, lambda x: x)
    print(result)


# except Exception as error:
#     print("🚒 something went wrong")
#     print(error)


if __name__ == "__main__":
    main()
