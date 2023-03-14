module Language.MainSpec (spec) where

import qualified Language.Ast    as LAST
import qualified Language.Main   as LM
import qualified Language.Tokens as LT
import qualified Test.Hspec      as TS

checkTokens :: String -> [LT.Token] -> TS.Expectation
checkTokens s = TS.shouldBe $ LM.getTokens s

getTokenTestCase :: String -> String -> [LT.Token] -> TS.SpecWith ()
getTokenTestCase mp s ts = TS.it (mp ++ s) $ do checkTokens s ts

checkAst :: String -> LAST.StackSqlStatement -> TS.Expectation
checkAst s = TS.shouldBe $ LM.getAst . LM.getTokens $ s

getAstTestCase :: String -> String -> LAST.StackSqlStatement -> TS.SpecWith ()
getAstTestCase mp s ts = TS.it (mp ++ s) $ do checkAst s ts

getAstTestCase' :: String -> String -> LAST.StackSqlStatement -> TS.SpecWith ()
getAstTestCase' mp s ts = TS.it mp $ do checkAst s ts

spec :: TS.Spec
spec = do
  TS.describe "MainSpec" $ do

    TS.describe "MainSpec.getTokens" $ do
        getTokenTestCase
            "can tokenize simple input: "
            "(2)"
            [LT.TLeftBrace, LT.TNumber 2, LT.TRightBrace]

        getTokenTestCase
            "can tokenize string inputs: "
            "foo \"foo bar\" 'foo bar'"
            [LT.TTerm "foo", LT.TDoubleQuoted "\"foo bar\"", LT.TSingleQuoted "'foo bar'"]

        getTokenTestCase
            "can tokenize simple SQL input (caps): "
            "SELECT foo FROM bar;"
            [LT.TSelect, LT.TTerm "foo", LT.TFrom, LT.TTerm "bar", LT.TSemicolon]

        getTokenTestCase
            "can tokenize simple SQL input (lower): "
            "select foo from bar;"
            [LT.TSelect, LT.TTerm "foo", LT.TFrom, LT.TTerm "bar", LT.TSemicolon]

        getTokenTestCase
            "can tokenize simple SQL input (lower): "
            "select 1;"
            [LT.TSelect, LT.TNumber 1, LT.TSemicolon]

        getTokenTestCase
            "can tokenize SQL special keywords (lower): "
            "all ; distinct ; select ; from ; *"
            [
                LT.TAll,
                LT.TSemicolon,
                LT.TDistinct,
                LT.TSemicolon,
                LT.TSelect,
                LT.TSemicolon,
                LT.TFrom,
                LT.TSemicolon,
                LT.TStar
            ]

        getTokenTestCase
            "can tokenize simple SQL comment: "
            "-- comment"
            [LT.TComment]

        getTokenTestCase
            "can tokenize ChaiSQL comment: "
            "-- @chaisql"
            [LT.TChaiComment]

        getTokenTestCase
            "can tokenize ChaiSQL comment with trigger: "
            "-- @chaisql:check"
            [LT.TChaiComment, LT.TColon, LT.TTerm "check"]

        getTokenTestCase
            "can tokenize ChaiSQL comment with epxression: "
            "-- @chaisql:newtype Foo = Bar"
            [LT.TChaiComment, LT.TColon, LT.TTerm "newtype", LT.TTerm "Foo", LT.TOperator "=", LT.TTerm "Bar"]

        getTokenTestCase
            "can tokenize ChaiSQL comment with compound return: "
            "-- @chaisql:newtype DbView <set> {foo: Foo, bar: Bar}"
            [LT.TChaiComment, LT.TColon, LT.TTerm "newtype", LT.TTerm "DbView", LT.TLeftAngle, LT.TTerm "set", LT.TRightAngle, LT.TLeftCurl, LT.TTerm "foo", LT.TColon, LT.TTerm "Foo", LT.TComma, LT.TTerm "bar", LT.TColon, LT.TTerm "Bar", LT.TRightCurl]

        getTokenTestCase
            "can tokenize punctuation: "
            "; , : ."
            [
                LT.TSemicolon,
                LT.TComma,
                LT.TColon,
                LT.TDot
            ]

        getTokenTestCase
            "can tokenize ChaiSQL operators (plus star): "
            "- + = / * ; ++ == // **"
            [
                LT.TOperator "-",
                LT.TOperator "+",
                LT.TOperator "=",
                LT.TOperator "/",
                LT.TStar,
                LT.TSemicolon,
                LT.TOperator "++",
                LT.TOperator "==",
                LT.TOperator "//",
                LT.TStar,
                LT.TStar
            ]

    TS.describe "MainSpec.getAst" $ do
        getAstTestCase
            "can parse simple select: "
            "select 1"
            [
                LAST.SSelectStatement $
                    LAST.SSelectStatementAtom Nothing [
                        LAST.SSelectAccessConstant $ LAST.SNumberConstant 1
                    ]
            ]

        getAstTestCase
            "can parse simple select (with semicolon): "
            "select 1;"
            [
                LAST.SSelectStatement $
                    LAST.SSelectStatementAtom Nothing [
                        LAST.SSelectAccessConstant $ LAST.SNumberConstant 1
                    ]
            ]

        getAstTestCase
            "can parse multiple simple selects: "
            "select 1; select 2; select 3"
            -- NB: since we are using left recursion, the parsed list is reverse
            (reverse [
                LAST.SSelectStatement $
                    LAST.SSelectStatementAtom Nothing [
                        LAST.SSelectAccessConstant $ LAST.SNumberConstant 1
                    ]
                ,
                LAST.SSelectStatement $
                    LAST.SSelectStatementAtom Nothing [
                        LAST.SSelectAccessConstant $ LAST.SNumberConstant 2
                    ]
                ,
                LAST.SSelectStatement $
                    LAST.SSelectStatementAtom Nothing [
                        LAST.SSelectAccessConstant $ LAST.SNumberConstant 3
                    ]
            ])

        getAstTestCase
            "can parse simple select-from: "
            "select X from Y"
            [
                LAST.SSelectStatement $
                    LAST.SSelectStatementWithFrom
                        Nothing
                        [LAST.SSelectAccessColumn $ LAST.STerm "X"]
                        (LAST.SSelectFromTable $ LAST.STerm "Y")
            ]

        getAstTestCase
            "can parse simple select-from (with many columns): "
            "select A, B from C"
            [
                LAST.SSelectStatement $
                    LAST.SSelectStatementWithFrom
                        Nothing
                        [
                            LAST.SSelectAccessColumn $ LAST.STerm "B",
                            LAST.SSelectAccessColumn $ LAST.STerm "A"
                        ]
                        (LAST.SSelectFromTable $ LAST.STerm "C")
            ]

        getAstTestCase
            "can parse a ChaiSQL trigger: "
            "-- @chaisql:check"
            [LAST.SSqlComment (LAST.SCommentChai (LAST.SChaiTrigger (LAST.STerm "check")))]

        getAstTestCase
            "can parse a ChaiSQL expression: "
            "-- @chaisql:newtype Foo = Bar"
            [LAST.SSqlComment (LAST.SCommentChai (LAST.SChaiExpression (LAST.STerm "newtype") (LAST.STerm "Foo") (LAST.SOperator "=") (LAST.STerm "Bar")))]

        getAstTestCase
            "can parse a ChaiSQL compound return: "
            "-- @chaisql:returns DbView <set> {foo: Foo, bar: Bar}"
            [LAST.SSqlComment (LAST.SCommentChai (LAST.SChaiCompound (LAST.STerm "returns") (LAST.STerm "DbView") (LAST.STerm "set") [LAST.SChaiAttributePair (LAST.STerm "bar") (LAST.STerm "Bar"), LAST.SChaiAttributePair (LAST.STerm "foo") (LAST.STerm "Foo")]))]


        getAstTestCase'
            "can parse a SQL statement with ChaiSQL comment: (compound returns and select)"
            (
                unlines [
                    "", -- empty line
                    "-- @chaisql:returns DbView <set> {foo: Foo, bar: Bar}",
                    "SELECT foo, bar FROM foobar;"
                ]
            )
            [
                LAST.SSelectStatement (LAST.SSelectStatementWithFrom Nothing [LAST.SSelectAccessColumn (LAST.STerm "bar"),LAST.SSelectAccessColumn (LAST.STerm "foo")] (LAST.SSelectFromTable (LAST.STerm "foobar"))),
                LAST.SSqlComment (LAST.SCommentChai (LAST.SChaiCompound (LAST.STerm "returns") (LAST.STerm "DbView") (LAST.STerm "set") [LAST.SChaiAttributePair (LAST.STerm "bar") (LAST.STerm "Bar"), LAST.SChaiAttributePair (LAST.STerm "foo") (LAST.STerm "Foo")]))
            ]