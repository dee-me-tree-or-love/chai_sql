from unittest.mock import MagicMock

mocker = MagicMock()


class TypeCheckAction:
    def execute(self, raw_sql: str) -> bool:
        # FIXME: implement
        sql_ast = mocker.sql_parser.parse(raw_sql)
        sql_annotated_ast = mocker.type_parser.parse(sql_ast)
        type_check = mocker.type_checker.check(sql_annotated_ast)
        return type_check.succes
