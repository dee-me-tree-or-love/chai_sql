from dataclasses import dataclass
from enum import Enum
from typing import Generic, Iterator, Sequence, Tuple, TypeVar

T = TypeVar("T")


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
