from chai_sql.models import ChaiSqlAst, RawSqlAst


def annotate(sql: RawSqlAst) -> ChaiSqlAst:
    raise NotImplementedError()
