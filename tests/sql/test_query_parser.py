from chai_sql.sql.query_parser import parse


def test_parse_sql_2_ast():
    assert parse("Select 1;") == False
