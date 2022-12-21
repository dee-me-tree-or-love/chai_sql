from dataclasses import dataclass
from typing import Any, Generic, Protocol, TypeVar

from chai_sql.models import (
    ChaiSqlAttribute,
    ChaiSqlRelation,
    ChaiSqlSchema,
    ChaiSqlType,
    GenericParserResult,
    GenericParserWrapper,
)
from chai_sql.shared.arpeggio_parser_wrapper import (
    ArpeggioParserWrapper,
    wrap_arpeggio_parser,
)

SCHEMA_GRAMMAR_PEG = """
// lang:cleanpeg
schema = (schema_body) EOF
schema_body = types_body? relation_body*
// types
types_body = "types" binder "{" type_declarations? "}"
type_declarations = type_spec ', ' type_declarations / type_spec
// relations
relation_body = "relation" name_spec binder "{" relation_attrs "}"
relation_attrs = attribute_spec relation_attrs / attribute_spec 
attribute_spec = name_spec binder type_spec ','
// Common pieces
// TODO: review whether type & name specs should be improved
type_spec = r'[a-zA-Z]'*
name_spec = r'[a-zA-Z]'*
binder = ":"
"""


def _get_arpeggio_parser(debug=False) -> ArpeggioParserWrapper:
    """
    Prepares the Arpeggio-based schema parser.

    Returns:
        ArpeggioParserWrapper: a wrapped Arpeggio parser

    Examples:
        TODO: add examples
    """
    return wrap_arpeggio_parser(SCHEMA_GRAMMAR_PEG, "schema", debug=debug)


def get_default_parser(**kwargs) -> ArpeggioParserWrapper:
    return _get_arpeggio_parser(**kwargs)


def parse(text: str, parser: GenericParserWrapper) -> GenericParserResult:
    return parser.parse(text)


def translate(parse_result: GenericParserResult) -> ChaiSqlSchema:
    raise NotImplementedError()


def check_semantics(schema: ChaiSqlSchema) -> ChaiSqlSchema:
    raise NotImplementedError()


def load(schema: str) -> ChaiSqlSchema:
    raise NotImplementedError()
