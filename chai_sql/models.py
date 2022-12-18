from dataclasses import dataclass
from enum import Enum
from typing import Generic, Tuple, TypeVar, Iterator

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
