module Language.MainSpec (spec) where

import qualified Test.Hspec as TS
import qualified Language.Main as LM
import qualified Language.Tokens as LT

checkTokens :: String -> [LT.Token] -> TS.Expectation
checkTokens s = TS.shouldBe (LM.getTokens s)

getTokenTestCase :: String -> String -> [LT.Token] -> TS.SpecWith ()
getTokenTestCase mp s ts = TS.it (mp ++ s) $ do checkTokens s ts

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
            [LT.TText "foo", LT.TDoubleQuoted "\"foo bar\"", LT.TSingleQuoted "'foo bar'"]

        getTokenTestCase 
            "can tokenize simple SQL input (caps): "
            "SELECT foo FROM bar;"
            [LT.TSelect, LT.TText "foo", LT.TFrom, LT.TText "bar", LT.TSemicolon]

        getTokenTestCase
            "can tokenize simple SQL input (lower): "
            "select foo from bar;"
            [LT.TSelect, LT.TText "foo", LT.TFrom, LT.TText "bar", LT.TSemicolon]

        getTokenTestCase
            "can tokenize simple SQL input (lower): "
            "select 1;" 
            [LT.TSelect, LT.TNumber 1, LT.TSemicolon]

        getTokenTestCase
            "can tokenize simple SQL comment: "
            "-- comment"
            [LT.TComment]

        getTokenTestCase
            "can tokenize ChaiSQL trigger: "
            "-- @chaisql"
            [LT.TChaiComment]

        getTokenTestCase
            "can tokenize ChaiSQL trigger with command: "
            "-- @chaisql:check"
            [LT.TChaiComment, LT.TColon, LT.TText "check"]

        getTokenTestCase
            "can tokenize ChaiSQL operators: "
            "- + = / * ; ++ == // **"
            [
                LT.TOperator "-",
                LT.TOperator "+",
                LT.TOperator "=",
                LT.TOperator "/",
                LT.TOperator "*",
                LT.TSemicolon,
                LT.TOperator "++",
                LT.TOperator "==",
                LT.TOperator "//",
                LT.TOperator "**"
            ]
