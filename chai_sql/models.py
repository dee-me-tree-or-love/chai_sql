from dataclasses import dataclass
from typing import Generic, TypeVar

T = TypeVar("T")


@dataclass
class RawSqlAst(Generic[T]):
    origin: T


class ChaiSqlAst:
    pass
