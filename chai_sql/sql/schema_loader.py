from arpeggio.cleanpeg import ParserPEG

from chai_sql.models import (
    ChaiSqlAttribute,
    ChaiSqlRelation,
    ChaiSqlSchema,
    ChaiSqlType,
)


def load(schema: str) -> ChaiSqlSchema:
    raise NotImplementedError()
