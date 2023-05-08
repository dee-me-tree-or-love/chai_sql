module ChaiMicroSql.ASTSpec (
    spec
    ) where

import qualified ChaiMicroSql.AST         as AST
import qualified ChaiMicroSql.CommonUtils as CU
import qualified Test.Hspec               as THS

none :: ()
none = ()

spec :: THS.Spec
spec = do
    THS.describe "AST of" $ do
        THS.describe "`SELECT * FROM Foo;`" $ do
            THS.it "parses" $ do
                let a = AST.GASTSelectQueryTyped none
                        [AST.GASTSelectAttributeTypedStar none AST.ASTSelectAttributeStarTotalRecord :: AST.GASTSelectAttributeTyped () ()]
                        [AST.GASTFromTableTypedReference none (AST.ASTVariable "Foo")] :: AST.GASTSelectQueryTyped () () () ()
                let p = id -- TODO(backlog): define a parse function
                THS.shouldBe (p a) a

    THS.describe "toString" $ do
        THS.describe "of Variable" $ do
            THS.it "returns variable name" $ do
                let n = "foo"
                let v = AST.ASTVariable n
                let f = CU.toString
                THS.shouldBe (f v) n

        THS.describe "of Alias" $ do
            THS.it "returns alias name" $ do
                let n = "foo"
                let v = AST.ASTSimpleAlias n
                let f = CU.toString
                THS.shouldBe (f v) n
