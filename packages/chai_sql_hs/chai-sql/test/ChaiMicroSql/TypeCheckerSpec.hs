module ChaiMicroSql.TypeCheckerSpec (spec) where

import qualified ChaiMicroSql.AST         as AST
import qualified ChaiMicroSql.TAST        as TAST
import qualified ChaiMicroSql.TypeChecker as TC
import qualified ChaiMicroSql.TypeContext as TCX
import qualified Test.Hspec               as THS


spec :: THS.Spec
spec = do
    THS.describe "Type checker" $ do
        THS.describe "with inferVar" $ do
            THS.describe "with unknown variable" $ do
                THS.it "returns Left <error text>" $ do
                    let __v = "foo"
                    let av = AST.ASTVariable __v
                    let f = TC.inferVar TCX.freshContext
                    let e = Left $ TC.__varNotKnownError __v
                    THS.shouldBe (f av) e

            THS.describe "with known variable" $ do
                THS.it "returns Right <variable type>" $ do
                    let __v = "foo"
                    let __k = TCX.TCSimpleTypeContextKey __v
                    let __kv = TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool
                    let __c = TCX.extend __k __kv TCX.freshContext
                    let av = AST.ASTVariable __v
                    let f = TC.inferVar __c
                    let r = Right (TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool)
                    THS.shouldBe (f av) r

        THS.describe "with inferTotalRecord" $ do
            THS.describe "with a total record" $ do
                THS.it "returns Right <total record type>" $ do
                    let a = AST.ASTSelectAttributeStarTotalRecord
                    let f = TC.inferTotalRecord TCX.freshContext
                    let r = Right TAST.TASTSimpleTypeRecordTotal
                    THS.shouldBe (f a) r

        THS.describe "with inferAttributeReference" $ do
            THS.describe "with a non-qualified access" $ do
                THS.describe "with known attribute" $ do
                    THS.it "returns Right <inference result>" $ do
                        let __v = "foo"
                        let av = AST.ASTSelectAttributeReferenceUnqualified (AST.ASTVariable __v)
                        let __k = TCX.TCSimpleTypeContextKey __v
                        let __vt = TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool
                        let __c = TCX.extend __k __vt TCX.freshContext
                        let f = TC.inferAttributeReference __c
                        let r = Right $ TAST.TASTSimpleTypeBasicIndexKeyValue (TAST.TASTSimpleTypeBasicIndexKey __v) __vt
                        THS.shouldBe (f av) r

                THS.describe "with unknown attribute" $ do
                    THS.it "returns Left <error result>" $ do
                        let __v = "foo"
                        let __k = TCX.TCSimpleTypeContextKey __v
                        let __vt = TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool
                        let __c = TCX.freshContext
                        let av = AST.ASTSelectAttributeReferenceUnqualified (AST.ASTVariable __v)
                        let f = TC.inferAttributeReference __c
                        let r = Left $ TC.__varNotKnownError __v
                        THS.shouldBe (f av) r

            THS.describe "with a qualified access" $ do
                THS.describe "with a non-record base" $ do
                    THS.it "returns Left <error text>" $ do
                        let __v = "foo"
                        let __av = AST.ASTVariable __v
                        let __k = TCX.TCSimpleTypeContextKey __v
                        let __vt = TAST.TASTSimpleTypeBasicBool
                        let __vt' = TAST.TASTSimpleTypeBasic __vt
                        let __c = TCX.extend __k __vt' TCX.freshContext
                        let __v' = "bar"
                        let __av' = AST.ASTVariable __v'
                        let av = AST.ASTSelectAttributeReferenceQualified __av __av'
                        let f = TC.inferAttributeReference __c
                        let e = Left $ TC.__baseNotRecordError __av __av' __vt
                        THS.shouldBe (f av) e

                THS.describe "with a total record base" $ do
                    THS.it "returns Left <error text>" $ do
                        let __v = "foo"
                        let __v' = "bar"
                        let __av = AST.ASTVariable __v
                        let __av' = AST.ASTVariable __v'
                        let av = AST.ASTSelectAttributeReferenceQualified __av __av'
                        let __k = TCX.TCSimpleTypeContextKey __v
                        let __vt' = TAST.TASTSimpleTypeRecordTotal
                        let __c = TCX.extend __k __vt' TCX.freshContext
                        let f = TC.inferAttributeReference __c
                        let e = Left $ TC.__baseTotalRecordError __av __av'
                        THS.shouldBe (f av) e

                THS.describe "with a simple record base" $ do
                    THS.describe "with an unknown attribute" $ do
                        THS.it "returns Left <error text>" $ do
                            -- attribute type
                            let __v = "bar"
                            let __av = AST.ASTVariable __v
                            let __vt = TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool
                            -- base type
                            let __b = "foo"
                            let __ab = AST.ASTVariable __b
                            let __bt = TAST.TASTSimpleTypeRecord $ TAST.make []
                            -- context stuff
                            let __k = TCX.TCSimpleTypeContextKey __b
                            let __c = TCX.extend __k __bt TCX.freshContext
                            -- abstract syntax nodes
                            let abv = AST.ASTSelectAttributeReferenceQualified __ab __av
                            let f = TC.inferAttributeReference __c
                            let e = Left $ TC.__recordUnknownAttributeError __ab __av
                            THS.shouldBe (f abv) e

                THS.describe "with a simple record base" $ do
                    THS.describe "with an known attribute" $ do
                        THS.it "returns Right <record index type>" $ do
                            -- attribute type
                            let __v = "bar"
                            let __av = AST.ASTVariable __v
                            let __vt = TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool
                            -- base type
                            let __b = "foo"
                            let __ab = AST.ASTVariable __b
                            let __bvk = TAST.TASTSimpleTypeBasicIndexKeyValue (TAST.TASTSimpleTypeBasicIndexKey __v) __vt
                            let __bt = TAST.TASTSimpleTypeRecord $ TAST.make [__bvk]
                            -- context stuff
                            let __k = TCX.TCSimpleTypeContextKey __b
                            let __c = TCX.extend __k __bt TCX.freshContext
                            -- abstract syntax nodes
                            let abv = AST.ASTSelectAttributeReferenceQualified __ab __av
                            let f = TC.inferAttributeReference __c
                            let r = Right __bvk
                            THS.shouldBe (f abv) r

        THS.describe "with inferAttribute" $ do
            THS.describe "with a star access" $ do
                THS.it "returns Right <total record type>" $ do
                    let a = AST.ASTSelectAttributeStar AST.ASTSelectAttributeStarTotalRecord
                    let f = TC.inferAttribute TCX.freshContext
                    let r = Right TAST.TASTSimpleTypeRecordTotal
                    THS.shouldBe (f a) r

            THS.describe "with a direct access" $ do
                THS.describe "with a known variable" $ do
                    THS.it "returns Right <inferred type>" $ do
                        -- foo is an attribute of type Bool
                        let __v = "foo"
                        let __k = TCX.TCSimpleTypeContextKey __v
                        let __vt = TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool
                        -- it is known to the context
                        let __c = TCX.extend __k __vt TCX.freshContext
                        -- we are directly accessing foo
                        let a = AST.ASTSelectAttributeReference $ AST.ASTSelectAttributeReferenceUnqualified (AST.ASTVariable __v)
                        let f = TC.inferAttribute __c
                        let r = Right $ TAST.TASTSimpleTypeBasic $ TAST.TASTSimpleTypeBasicIndex $ TAST.TASTSimpleTypeBasicIndexKeyValue (TAST.TASTSimpleTypeBasicIndexKey __v) __vt
                        THS.shouldBe (f a) r

                THS.describe "with an unknown variable" $ do
                    THS.it "returns Left <error message>" $ do
                        -- foo is an attribute of type Bool
                        let __v = "foo"
                        let __k = TCX.TCSimpleTypeContextKey __v
                        let __vt = TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool
                        -- it is known to the context
                        let __c = TCX.freshContext
                        -- we are directly accessing foo
                        let a = AST.ASTSelectAttributeReference $ AST.ASTSelectAttributeReferenceUnqualified (AST.ASTVariable __v)
                        let f = TC.inferAttribute __c
                        let r = Left $ TC.__varNotKnownError __v
                        THS.shouldBe (f a) r

            THS.describe "with a alias access" $ do
                THS.describe "with a known variable and alias" $ do
                    THS.it "returns Right <inferred type> with renamed index" $ do
                        -- foo is an attribute of type Bool
                        let __v = "foo"
                        let __u = "bar"
                        let __k = TCX.TCSimpleTypeContextKey __v
                        let __vt = TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool
                        -- it is known to the context
                        let __c = TCX.extend __k __vt TCX.freshContext
                        -- we are directly accessing foo
                        let a = AST.ASTSelectAttributeReferenceAlias (AST.ASTSelectAttributeReferenceUnqualified (AST.ASTVariable __v)) (AST.ASTSimpleAlias __u)
                        let f = TC.inferAttribute __c
                        let r = Right $ TAST.TASTSimpleTypeBasic $ TAST.TASTSimpleTypeBasicIndex $ TAST.TASTSimpleTypeBasicIndexKeyValue (TAST.TASTSimpleTypeBasicIndexKey __u) __vt
                        THS.shouldBe (f a) r

        THS.describe "with inferSelectList" $ do
            THS.describe "with at least one error" $ do
                THS.it "returns Left <all error messages combined>" $ do
                    let __v = "foo"
                    let av = AST.ASTSelectAttributeReference $ AST.ASTSelectAttributeReferenceUnqualified (AST.ASTVariable __v)
                    let f = TC.inferSelectList TCX.freshContext
                    let e = Left $ foldl TC.joinErrors TC.emptyError [TC.__varNotKnownError __v]
                    THS.shouldBe (f [av]) e
