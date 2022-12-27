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


S = TypeVar("S")
N = TypeVar("N")


@dataclass
class RoseTree(Generic[S, N]):
    source: S
    node: N | None
    nr_children: int
    children: List[RoseTree[S, N]]

    def dfs_iter(self) -> Iterator[RoseTree[S, N]]:
        return chain.from_iterable(map(self.__dfs_iter, self.children))

    def __dfs_iter(self, subtree: RoseTree[S, N]) -> Iterator[RoseTree[S, N]]:
        yield subtree
        for child in subtree.children:
            for c_subtree in self.__dfs_iter(child):
                yield c_subtree


# Vanilla SQL
# -----------


class SqlCommand(Enum):
    NOT_SUPPORTED = 0
    SELECT = 1


@dataclass
class SqlAstNode:
    # TODO: restrict the node kinds
    kind: str
    value: str


@dataclass
class SqlAst(Generic[S]):
    # FIXME: rename to `kind`
    command: SqlCommand
    tree: RoseTree[S, SqlAstNode]


# ChaiSQL
# -------


@dataclass
class ChaiSqlAstNode(SqlAstNode):
    type_tree: TypeCommandAst


@dataclass
class ChaiSqlAst(Generic[S]):
    command: SqlCommand
    tree: RoseTree[S, ChaiSqlAstNode]


# Type Info
# ---------


@dataclass
class TypeCommandAstNode:
    # TODO: restrict the node kinds
    kind: str
    value: str


@dataclass
class TypeCommandAst(Generic[S]):
    tree: RoseTree[S, TypeCommandAstNode]


# Schema loading
# --------------


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
