from dataclasses import dataclass
from typing import Any, Generic, Protocol, TypeVar

from chai_sql.models import (
    GenericParserResult,
    GenericParserWrapper,
    RoseTree,
    SqlAst,
    SqlAstNode,
    TypeCommandAst,
    TypeCommandAstNode,
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
app_returns = app_returns_alias app_type_expression
app_returns_alias = "returns" / "~"
app_newtype = app_newtype_alias app_type_expression "=" app_type_expression
app_newtype_alias = "newtype" / "+"
// TODO: review whether type references should be improved
app_type_expression = r'[a-zA-Z]'*
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
        >>> dir(parser.parse("@chai_sql:check"))
        False

        >>> parser.parse("@chai_sql:check")
        [ [ trigger '@' [0], [  'chai_sql' [1] ],  ':' [9], [ [ [  'check' [10] ] ] ] ], EOF [15] ]

        >>> parser.parse("@chai_sql:check(schema.foo)")
        [ [ trigger '@' [0], [  'chai_sql' [1] ],  ':' [9], [ [ [  'check' [10] ],  '(' [15], [  's' [16],  ...  'f' [23],  'o' [24],  'o' [25] ],  ')' [26] ] ] ], EOF [27] ]
    """
    return wrap_arpeggio_parser(TYPE_GRAMMAR_PEG, "typer_statement", debug=debug)


def _arpeggio_parse(text, **kwargs):
    parser = _get_arpeggio_parser(**kwargs)
    return parser.parse(text)


TypeRoseTree = RoseTree[Any, TypeCommandAstNode]


def _arpeggio_tree_2_type_tree(parse_tree: Any) -> TypeRoseTree:
    node = TypeCommandAstNode(kind=parse_tree.rule_name, value=rule_name.flat_str())
    try:
        subtree_base = [subtree for subtree in parse_tree]
        return TypeRoseTree(
            source=parse_tree,
            node=node,
            nr_children=len(subtree_base),
            children=list(map(_arpeggio_tree_2_type_tree, subtree_base)),
        )
    except TypeError:
        return TypeRoseTree(
            source=parse_tree,
            node=node,
            nr_children=0,
            children=[],
        )


def _parse_arpegio_comment(comment: str, **kwargs) -> TypeCommandAst:
    """
    Examples:
        >>> _parse_arpegio_comment("@chai_sql:check")
        False
    """
    tree = _arpeggio_parse(comment, **kwargs)
    return TypeCommandAst(tree=_arpeggio_tree_2_type_tree(tree))


def parse(comment: str) -> TypeCommandAst:
    return _parse_arpegio_comment(comment)
