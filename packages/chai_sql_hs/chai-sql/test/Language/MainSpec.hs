module Language.MainSpec (spec) where

import           Language.Ast
import           Language.Main
import           Language.Tokens
import qualified Test.Hspec      as TS

checkTokens :: String -> [Token] -> TS.Expectation
checkTokens s = TS.shouldBe $ getTokens s

getTokenTestCase :: String -> String -> [Token] -> TS.SpecWith ()
getTokenTestCase mp s ts = TS.it (mp ++ s) $ do checkTokens s ts

checkAst :: String -> StackSqlStatement NaiveMaybeTypedAstContext -> TS.Expectation
checkAst s = TS.shouldBe $ getAst . getTokens $ s

getAstTestCase :: String -> String -> StackSqlStatement NaiveMaybeTypedAstContext -> TS.SpecWith ()
getAstTestCase mp s ts = TS.it (mp ++ s) $ do checkAst s ts

getAstTestCase' :: String -> String -> StackSqlStatement NaiveMaybeTypedAstContext -> TS.SpecWith ()
getAstTestCase' mp s ts = TS.it mp $ do checkAst s ts

spec :: TS.Spec
spec = do
  TS.describe "MainSpec" $ do

    TS.describe "MainSpec.getTokens" $ do
        getTokenTestCase
            "can tokenize simple input: "
            "(2)"
            [TLeftBrace, TNumber 2, TRightBrace]

        getTokenTestCase
            "can tokenize string inputs: "
            "foo \"foo bar\" 'foo bar'"
            [TTerm "foo", TDoubleQuoted "\"foo bar\"", TSingleQuoted "'foo bar'"]

        getTokenTestCase
            "can tokenize simple SQL input (caps): "
            "SELECT foo FROM bar;"
            [TSelect, TTerm "foo", TFrom, TTerm "bar", TSemicolon]

        getTokenTestCase
            "can tokenize simple SQL input (lower): "
            "select foo from bar;"
            [TSelect, TTerm "foo", TFrom, TTerm "bar", TSemicolon]

        getTokenTestCase
            "can tokenize simple SQL input (lower): "
            "select 1;"
            [TSelect, TNumber 1, TSemicolon]

        getTokenTestCase
            "can tokenize SQL special keywords (lower): "
            "all ; distinct ; select ; from ; *"
            [
                TAll,
                TSemicolon,
                TDistinct,
                TSemicolon,
                TSelect,
                TSemicolon,
                TFrom,
                TSemicolon,
                TStar
            ]

        getTokenTestCase
            "can tokenize simple SQL comment: "
            "-- comment"
            [TComment]

        getTokenTestCase
            "can tokenize ChaiSQL comment: "
            "-- @chaisql"
            [TChaiComment]

        getTokenTestCase
            "can tokenize ChaiSQL comment with trigger: "
            "-- @chaisql:check"
            [TChaiComment, TColon, TTerm "check"]

        getTokenTestCase
            "can tokenize ChaiSQL comment with epxression: "
            "-- @chaisql:newtype Foo = Bar"
            [TChaiComment, TColon, TTerm "newtype", TTerm "Foo", TOperator "=", TTerm "Bar"]

        getTokenTestCase
            "can tokenize ChaiSQL comment with compound return: "
            "-- @chaisql:newtype DbView <set> {foo: Foo, bar: Bar}"
            [TChaiComment, TColon, TTerm "newtype", TTerm "DbView", TLeftAngle, TTerm "set", TRightAngle, TLeftCurl, TTerm "foo", TColon, TTerm "Foo", TComma, TTerm "bar", TColon, TTerm "Bar", TRightCurl]

        getTokenTestCase
            "can tokenize punctuation: "
            "; , : ."
            [
                TSemicolon,
                TComma,
                TColon,
                TDot
            ]

        getTokenTestCase
            "can tokenize ChaiSQL operators (plus star): "
            "- + = / * ; ++ == // **"
            [
                TOperator "-",
                TOperator "+",
                TOperator "=",
                TOperator "/",
                TStar,
                TSemicolon,
                TOperator "++",
                TOperator "==",
                TOperator "//",
                TStar,
                TStar
            ]

    TS.describe "MainSpec.getAst" $ do
        getAstTestCase
            "can parse simple select: "
            "select 1"
            [
                SSelectStatement
                    (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) 
                    (SSelectStatementAtom 
                        (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) 
                        Nothing 
                        [SSelectAccessConstant (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SNumberConstant (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) 1)]
                    )
            ]

        getAstTestCase
            "can parse simple select (with semicolon): "
            "select 1;"
            [
                SSelectStatement (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SSelectStatementAtom (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) Nothing [SSelectAccessConstant (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SNumberConstant (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) 1)])
            ]

        getAstTestCase
            "can parse multiple simple selects: "
            "select 1; select 2; select 3"
            -- NB: since we are using left recursion, the parsed list is reverse
            (reverse
                [ SSelectStatement (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SSelectStatementAtom (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) Nothing [SSelectAccessConstant (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SNumberConstant (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) 1)])
                , SSelectStatement (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SSelectStatementAtom (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) Nothing [SSelectAccessConstant (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SNumberConstant (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) 2)])
                , SSelectStatement (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SSelectStatementAtom (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) Nothing [SSelectAccessConstant (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SNumberConstant (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) 3)])
                ]
            )

        getAstTestCase
            "can parse simple select-from: "
            "select X from Y"
            [
                SSelectStatement
                    (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing})
                    (SSelectStatementWithFrom
                        (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing})
                        Nothing
                        [
                            SSelectAccessColumn (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "X")
                        ]
                        (SSelectFromTable
                            (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing})
                            (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "Y")
                        )
                    )
            ]

        getAstTestCase
            "can parse simple select-from (with many columns): "
            "select A, B from C"
            [
                SSelectStatement
                    (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing})
                    (SSelectStatementWithFrom
                        (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing})
                        Nothing
                        [
                            SSelectAccessColumn (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "B"),
                            SSelectAccessColumn (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "A")
                        ]
                        (SSelectFromTable
                            (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing})
                            (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "C")
                        )
                    )
            ]

        getAstTestCase
            "can parse a ChaiSQL trigger: "
            "-- @chaisql:check"
            [
                SSqlComment (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SCommentChai (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SChaiTrigger (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "check")))
            ]

        getAstTestCase
            "can parse a ChaiSQL expression: "
            "-- @chaisql:newtype Foo = Bar"
            [
                SSqlComment (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SCommentChai (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SChaiExpression (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "newtype") (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "Foo") (SOperator (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "=") (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "Bar")))
            ]

        getAstTestCase
            "can parse a ChaiSQL compound return: "
            "-- @chaisql:returns DbView <set> {foo: Foo, bar: Bar}"
            [
                SSqlComment (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SCommentChai (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SChaiCompound (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "returns") (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "DbView") (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "set") [SChaiAttributePair (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "bar") (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "Bar"),SChaiAttributePair (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "foo") (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "Foo")]))
            ]

        getAstTestCase'
            "can parse a SQL statement with ChaiSQL comment: (compound returns and select)"
            (
                unlines [
                    "", -- empty line
                    "-- @chaisql:returns DbView <set> {name: String, age: Number}",
                    "SELECT DISTINCT name, age FROM cat;"
                ]
            )
            [
                SSelectStatement (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SSelectStatementWithFrom (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (Just (SSelectDistinct (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}))) [SSelectAccessColumn (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "age"),
                SSelectAccessColumn (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "name")] (SSelectFromTable (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "cat"))),SSqlComment (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SCommentChai (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (SChaiCompound (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "returns") (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "DbView") (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "set") [SChaiAttributePair (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "age") (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "Number"),SChaiAttributePair (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "name") (STerm (AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}) "String")]))
            ]
