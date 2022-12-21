from dataclasses import dataclass
from enum import Enum
from typing import Any, Generic, Iterator, Protocol, Sequence, Tuple, TypeAlias, TypeVar

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


GenericParserWrapper: TypeAlias = ParserWrapper[P, R]
GenericParserResult: TypeAlias = R


# TODO: this should become a RawSqlAstNode with references to parents
@dataclass
class RawSqlToken:
    position: Tuple[int, int]
    # TODO: add an enum for Known Token Options
    option: str
    value: str


class KnownSqlOptions(Enum):
    NOT_SUPPORTED = 0
    SELECT = 1


@dataclass
class RawSqlAst(Generic[T]):
    option: KnownSqlOptions
    origin: T

    # TODO: make it a property?
    def tokens(self) -> Iterator[RawSqlToken]:
        # TODO: make this an abstract function
        raise NotImplementedError()


class ChaiSqlAst:
    pass


@dataclass
class ChaiSqlType:
    name: str


@dataclass
class ChaiSqlAttribute:
    name: str
    type_spec: ChaiSqlType


@dataclass
class ChaiSqlRelation:
    name: str
    attributes: Sequence[ChaiSqlAttribute]


@dataclass
class ChaiSqlSchema:
    types: Sequence[ChaiSqlType]
    relations: Sequence[ChaiSqlRelation]
