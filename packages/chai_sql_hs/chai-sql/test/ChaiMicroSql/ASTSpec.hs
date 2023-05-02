module ChaiMicroSql.ASTSpec (
    spec
    ) where

import qualified ChaiMicroSql.AST as AST
import qualified Test.Hspec       as THS


spec :: THS.Spec
spec = do
    THS.describe "AST of" $ do
        THS.describe "`SELECT * FROM Foo;`" $ do
            THS.it "parses" $ do
                THS.shouldBe (p a) a
                where
                    a = AST.ASTSelectQuery
                        [AST.ASTSelectAttributeStar AST.ASTSelectAttributeStarTotalRecord]
                        [AST.ASTFromTableReference $ AST.ASTVariable "Foo"]
                    p = id -- TODO(backlog): define a parse function
