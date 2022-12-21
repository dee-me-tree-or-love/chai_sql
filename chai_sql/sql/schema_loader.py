from dataclasses import dataclass
from typing import Any, Generic, Protocol, TypeVar

from arpeggio.cleanpeg import ParserPEG

from chai_sql.models import (
    ChaiSqlAttribute,
    ChaiSqlRelation,
    ChaiSqlSchema,
    ChaiSqlType,
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

# TODO: make this a part of a separate shared package!
# FIXME: solve the duplication

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


def _get_arpeggio_parser(debug=False) -> ParserWrapper[ParserPEG, Any]:
    """
    Prepares the Arpeggio-based type info parser.

    Returns:
        ParserPEG: an Arpeggio parser
    """
    # TODO: define a better approach for DEBUG settings
    parser = ParserPEG(SCHEMA_GRAMMAR_PEG, "schema", debug=debug)
    return ParserWrapper(parser)


def get_default_parser(**kwargs) -> ParserWrapper[ParserPEG, Any]:
    return _get_arpeggio_parser(**kwargs)


def parse(text: str, parser: ParserWrapper[P, R]) -> R:
    return parser.parse(text)


def construct(parse_result: R) -> ChaiSqlSchema:
    raise NotImplementedError()


def check_semantics(schema: ChaiSqlSchema) -> ChaiSqlSchema:
    raise NotImplementedError()


def load(schema: str) -> ChaiSqlSchema:
    raise NotImplementedError()
