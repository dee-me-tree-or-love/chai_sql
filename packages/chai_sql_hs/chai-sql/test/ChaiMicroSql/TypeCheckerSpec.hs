module ChaiMicroSql.TypeCheckerSpec (spec) where

import qualified ChaiMicroSql.AST         as AST
import qualified ChaiMicroSql.TAST        as TAST
import qualified ChaiMicroSql.TypeChecker as TC
import qualified ChaiMicroSql.TypeContext as TCX
import qualified Data.Map                 as M
import qualified Test.Hspec               as THS



spec :: THS.Spec
spec = do
    THS.describe "Type checker" $ do
        THS.describe "with inferVar" $ do
            THS.describe "with unknown variable" $ do
                THS.it "returns Left <error text>" $ do
                    let __v = "foo"
                    let av = AST.ASTVariable __v
                    let e = Left (TC.__varError __v)
                    let f = TC.inferVar TCX.freshContext
                    THS.shouldBe (f av) e

            THS.describe "with known variable" $ do
                THS.it "returns Right <variable type>" $ do
                    let __v = "foo"
                    let av = AST.ASTVariable __v
                    let r = Right (TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool)
                    let __k = TCX.TCSimpleTypeContextKey __v
                    let __kv = TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool
                    let __c = TCX.extend __k __kv TCX.freshContext
                    let f = TC.inferVar __c
                    THS.shouldBe (f av) r

        THS.describe "with inferTotalRecord" $ do
            THS.describe "with a total record" $ do
                THS.it "returns Right <total record type>" $ do
                    let a = AST.ASTSelectAttributeStarTotalRecord
                    let r = Right (TAST.TASTSimpleTypeRecord TAST.TASTSimpleTypeRecordTotal)
                    let f = TC.inferTotalRecord TCX.freshContext
                    THS.shouldBe (f a) r


        THS.describe "with inferAttributeReference" $ do
            THS.describe "with a non-qualified access" $ do
                THS.it "returns inference TASTSimpleTypeBasicIndex <attribute> <attribute type>" $ do
                    let __v = "foo"
                    let av = AST.ASTSelectAttributeReferenceUnqualified (AST.ASTVariable __v)
                    let __k = TCX.TCSimpleTypeContextKey __v
                    let __vt = TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool
                    let __c = TCX.extend __k __vt TCX.freshContext
                    let r = Right (TAST.TASTSimpleTypeBasic $ TAST.TASTSimpleTypeBasicIndex $ TAST.TASTSimpleTypeBasicIndexKeyValue (TAST.TASTSimpleTypeBasicIndexKey __v) __vt )
                    let f = TC.inferAttributeReference __c
                    THS.shouldBe (f av) r

            THS.describe "with a qualified access" $ do
                THS.describe "with with a non-record base" $ do
                    THS.it "returns Left <error text>" $ do
                        let __v = "foo"
                        let __v' = "bar"
                        let __av = AST.ASTVariable __v
                        let __av' = AST.ASTVariable __v'
                        let av = AST.ASTSelectAttributeReferenceQualified __av __av'
                        let __k = TCX.TCSimpleTypeContextKey __v
                        let __vt = TAST.TASTSimpleTypeBasicBool
                        let __vt' = TAST.TASTSimpleTypeBasic __vt
                        let __c = TCX.extend __k __vt' TCX.freshContext
                        let e = Left $ TC.__baseNotRecordError __av __av' __vt
                        let f = TC.inferAttributeReference __c
                        THS.shouldBe (f av) e


