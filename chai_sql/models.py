from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from itertools import chain
from typing import (
    Any,
    Generic,
    Iterator,
    List,
    Protocol,
    Sequence,
    Tuple,
    TypeAlias,
    TypeVar,
)

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


class KnownSqlCommand(Enum):
    NOT_SUPPORTED = 0
    SELECT = 1


@dataclass
class SqlAstNode:
    children: Iterator[SqlAstNode]
    nr_children: int
    value: str
    # TODO: restrict the token family
    family: str


@dataclass
class SqlAst(Generic[T]):
    source: T
    command: KnownSqlCommand

    def roots(self) -> Iterator[SqlAstNode]:
        # TODO: make this an abstract function
        raise NotImplementedError()

    def dfs_nodes(self) -> Iterator[SqlAstNode]:
        return chain.from_iterable(self.__dfs_iter(node) for node in self.roots())

    def __dfs_iter(self, node: SqlAstNode) -> Iterator[SqlAstNode]:
        yield node
        for child in node.children:
            for c_node in self.__dfs_iter(child):
                yield c_node


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
