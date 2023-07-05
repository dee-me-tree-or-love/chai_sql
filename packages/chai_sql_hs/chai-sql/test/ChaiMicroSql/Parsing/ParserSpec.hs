module ChaiMicroSql.Parsing.ParserSpec (
    spec
    ) where

import qualified ChaiMicroSql.AST            as AST
import qualified ChaiMicroSql.Parsing.Lexer  as CPL
import qualified ChaiMicroSql.Parsing.Parser as CPP
import qualified Test.Hspec                  as THS

spec :: THS.Spec
spec = do
    let p = CPP.parse . CPL.tokenize
    THS.describe "Parser" $ do
        THS.describe "on standard SQL syntax" $ do
            THS.describe "parses" $ do
                let i = "SELECT *;"
                THS.it ("`" ++ i ++ "` into a correct AST") $ do
                    let e = AST.AstSelectQuery Nothing [AST.AstSelectAttributeAccessStar AST.AstSelectAttributeStarTotalRecord] []
                    THS.shouldBe (p i) [e]

            THS.describe "parses" $ do
                let i = "SELECT * FROM Foo;"
                THS.it ("`" ++ i ++ "` into a correct AST") $ do
                    let e = AST.AstSelectQuery Nothing [AST.AstSelectAttributeAccessStar AST.AstSelectAttributeStarTotalRecord] [AST.AstFromAccessReference (AST.AstVariable "Foo")]
                    THS.shouldBe (p i) [e]

            THS.describe "parses" $ do
                let i = "SELECT * FROM Foo; SELECT * FROM Bar;"
                THS.it ("`" ++ i ++ "` into a correct AST") $ do
                    let e1 = AST.AstSelectQuery Nothing [AST.AstSelectAttributeAccessStar AST.AstSelectAttributeStarTotalRecord] [AST.AstFromAccessReference (AST.AstVariable "Foo")]
                    let e2 = AST.AstSelectQuery Nothing [AST.AstSelectAttributeAccessStar AST.AstSelectAttributeStarTotalRecord] [AST.AstFromAccessReference (AST.AstVariable "Bar")]
                    -- Parsed queries are returned as a stack
                    THS.shouldBe (p i) [e2, e1]

            THS.describe "parses" $ do
                let i = "SELECT * FROM Foo"
                THS.it ("`" ++ i ++ "` into a correct AST") $ do
                    let e = AST.AstSelectQuery Nothing [AST.AstSelectAttributeAccessStar AST.AstSelectAttributeStarTotalRecord] [AST.AstFromAccessReference (AST.AstVariable "Foo")]
                    THS.shouldBe (p i) [e]

            THS.describe "parses" $ do
                let i = "SELECT * FROM Foo, Bar"
                THS.it ("`" ++ i ++ "` into a correct AST") $ do
                    let e = AST.AstSelectQuery Nothing [AST.AstSelectAttributeAccessStar AST.AstSelectAttributeStarTotalRecord] [AST.AstFromAccessReference (AST.AstVariable "Bar"), AST.AstFromAccessReference (AST.AstVariable "Foo")]
                    THS.shouldBe (p i) [e]

            THS.describe "parses" $ do
                let i = "SELECT Bar FROM Foo"
                THS.it ("`" ++ i ++ "` into a correct AST") $ do
                    let e = AST.AstSelectQuery Nothing [AST.AstSelectAttributeAccessReference $ AST.AstSelectAttributeReferenceUnqualified (AST.AstVariable "Bar")] [AST.AstFromAccessReference (AST.AstVariable "Foo")]
                    THS.shouldBe (p i) [e]

            THS.describe "parses" $ do
                let i = "SELECT Bar, Qua FROM Foo"
                THS.it ("`" ++ i ++ "` into a correct AST") $ do
                    let e = AST.AstSelectQuery Nothing [AST.AstSelectAttributeAccessReference $ AST.AstSelectAttributeReferenceUnqualified (AST.AstVariable "Qua"), AST.AstSelectAttributeAccessReference $ AST.AstSelectAttributeReferenceUnqualified (AST.AstVariable "Bar")] [AST.AstFromAccessReference (AST.AstVariable "Foo")]
                    THS.shouldBe (p i) [e]

            THS.describe "parses" $ do
                let i = "SELECT Bar.Qua FROM Foo"
                THS.it ("`" ++ i ++ "` into a correct AST") $ do
                    let e = AST.AstSelectQuery Nothing [AST.AstSelectAttributeAccessReference $ AST.AstSelectAttributeReferenceQualified (AST.AstVariable "Bar") (AST.AstVariable "Qua")] [AST.AstFromAccessReference (AST.AstVariable "Foo")]
                    THS.shouldBe (p i) [e]
