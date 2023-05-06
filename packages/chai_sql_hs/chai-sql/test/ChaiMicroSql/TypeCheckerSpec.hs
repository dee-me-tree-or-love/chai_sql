module ChaiMicroSql.TypeCheckerSpec (spec) where

import qualified ChaiMicroSql.AST         as AST
import qualified ChaiMicroSql.TAST        as TAST
import qualified ChaiMicroSql.TypeChecker as TC
import qualified ChaiMicroSql.TypeContext as TCX
import qualified ChaiMicroSql.TypeErrors  as TE
import qualified Test.Hspec               as THS


spec :: THS.Spec
spec = do
    THS.describe "Type checker" $ do

        -- FIXME: disable the test once it is covered
        THS.describe "implementation" $ do
            THS.it "should be tested" $ do
                THS.shouldBe "not ready" "ready"

        THS.describe "with inferVar" $ do
            THS.describe "with unknown variable" $ do
                THS.it "returns Left <error text>" $ do
                    let __v = "foo"
                    let av = AST.ASTVariable __v
                    let f = TC.inferVar TCX.freshContext :: (AST.ASTVariable -> Either TC.TCInferenceError TAST.TASTAtomicType)
                    let e = Left $ TC.__varNotKnownError __v
                    THS.shouldBe (f av) e

            THS.describe "with known variable with record type" $ do
                THS.it "returns Right <record type>" $ do
                    let __v = "foo"
                    let __avt = TAST.emptyTypeRecord
                    let av = AST.ASTVariable __v
                    let k = TCX.TCXSimpleTypeContextKey __v
                    let c = TCX.extend k (TCX.contextualize __avt) TCX.freshContext
                    let f = TC.inferVar c
                    let e = Right __avt
                    THS.shouldBe (f av) e

            THS.describe "with known variable with atomic type" $ do
                THS.it "returns Right <atomic type>" $ do
                    let __v = "foo"
                    let __avt = TAST.TASTAtomicTypeBool
                    let av = AST.ASTVariable __v
                    let k = TCX.TCXSimpleTypeContextKey __v
                    let c = TCX.extend k (TCX.contextualize __avt) TCX.freshContext
                    let f = TC.inferVar c
                    let e = Right __avt
                    THS.shouldBe (f av) e

        THS.describe "with inferTotalRecord" $ do
            THS.describe "with a total record" $ do
                THS.it "returns Right <total record type>" $ do
                    let a = AST.ASTSelectAttributeStarTotalRecord
                    let f = TC.inferTotalRecord TCX.freshContext
                    let e = Right TAST.TASTSimpleTypeRecordTotal
                    THS.shouldBe (f a) e

        THS.describe "with inferAttributeReference" $ do
            THS.describe "with a non-qualified access" $ do
                THS.describe "with known attribute" $ do
                    THS.it "returns Right <inference result>" $ do
                        let __v = "foo"
                        let __k = TCX.TCXSimpleTypeContextKey __v
                        let __vt = TAST.TASTAtomicTypeBool
                        let __c = TCX.extend __k (TCX.contextualize __vt) TCX.freshContext
                        let av = AST.ASTSelectAttributeReferenceUnqualified (AST.ASTVariable __v)
                        let f = TC.inferAttributeReference __c
                        let e = Right $ TAST.TASTSimpleAtomicIndexKeyValue (TAST.TASTSimpleAtomicIndexKey __v) __vt
                        THS.shouldBe (f av) e

                THS.describe "with unknown attribute" $ do
                    THS.it "returns Left <error result>" $ do
                        let __v = "foo"
                        let __k = TCX.TCXSimpleTypeContextKey __v
                        let __vt = TAST.TASTAtomicTypeBool
                        let __c = TCX.freshContext
                        let av = AST.ASTSelectAttributeReferenceUnqualified (AST.ASTVariable __v)
                        let f = TC.inferAttributeReference __c
                        let e = Left $ TC.__varNotKnownError __v
                        THS.shouldBe (f av) e

            THS.describe "with a qualified access" $ do
                THS.describe "with a simple record base" $ do
                    THS.describe "with an unknown attribute" $ do
                        THS.it "returns Left <error text>" $ do
                            -- attribute type
                            let __v = "bar"
                            let __av = AST.ASTVariable __v
                            let __vt = TAST.TASTAtomicTypeBool
                            -- base type
                            let __b = "foo"
                            let __ab = AST.ASTVariable __b
                            let __bt = TAST.emptyTypeRecord
                            -- context stuff
                            let __k = TCX.TCXSimpleTypeContextKey __b
                            let __c = TCX.extend __k (TCX.contextualize __bt) TCX.freshContext
                            -- abstract syntax nodes
                            let abv = AST.ASTSelectAttributeReferenceQualified __ab __av
                            let f = TC.inferAttributeReference __c
                            let e = Left $ TC.__recordUnknownAttributeError __ab __av
                            THS.shouldBe (f abv) e

                    THS.describe "with an known attribute" $ do
                        THS.it "returns Right <record index type>" $ do
                            -- attribute type
                            let __v = "bar"
                            let __av = AST.ASTVariable __v
                            let __vt = TAST.TASTAtomicTypeBool
                            -- base type
                            let __b = "foo"
                            let __ab = AST.ASTVariable __b
                            let __bvk = TAST.TASTSimpleAtomicIndexKeyValue (TAST.TASTSimpleAtomicIndexKey __v) __vt
                            let __bt = TAST.makeRecord [__bvk]
                            -- context stuff
                            let __k = TCX.TCXSimpleTypeContextKey __b
                            let __c = TCX.extend __k (TCX.contextualize __bt) TCX.freshContext
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

            THS.describe "with direct access" $ do
                THS.describe "with a known variable" $ do
                    THS.it "returns Right <inferred type>" $ do
                        -- foo is an attribute of type Bool
                        let __v = "foo"
                        let __k = TCX.TCXSimpleTypeContextKey __v
                        let __vt = TAST.TASTAtomicTypeBool
                        -- it is known to the context
                        let __c = TCX.extend __k (TCX.contextualize __vt) TCX.freshContext
                        -- we are directly accessing foo
                        let a = AST.ASTSelectAttributeReference $ AST.ASTSelectAttributeReferenceUnqualified (AST.ASTVariable __v)
                        let f = TC.inferAttribute __c
                        let r = Right $ TAST.TASTSimpleAtomicIndexPair $ TAST.TASTSimpleAtomicIndexKeyValue (TAST.TASTSimpleAtomicIndexKey __v) __vt
                        THS.shouldBe (f a) r

                THS.describe "with an unknown variable" $ do
                    THS.it "returns Left <error message>" $ do
                        -- foo is an attribute of type Bool
                        let __v = "foo"
                        let __k = TCX.TCXSimpleTypeContextKey __v
                        let __vt = TAST.TASTAtomicTypeBool
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
                        let __k = TCX.TCXSimpleTypeContextKey __v
                        let __vt = TAST.TASTAtomicTypeBool
                        -- it is known to the context
                        let __c = TCX.extend __k (TCX.contextualize __vt) TCX.freshContext
                        -- we are directly accessing foo
                        let a = AST.ASTSelectAttributeReferenceAlias (AST.ASTSelectAttributeReferenceUnqualified (AST.ASTVariable __v)) (AST.ASTSimpleAlias __u)
                        let f = TC.inferAttribute __c
                        let r = Right $ TAST.TASTSimpleAtomicIndexPair $ TAST.TASTSimpleAtomicIndexKeyValue (TAST.TASTSimpleAtomicIndexKey __u) __vt
                        THS.shouldBe (f a) r

        THS.describe "with inferSelectList" $ do
            THS.describe "with at least one error" $ do
                THS.it "returns Left <all error messages combined>" $ do
                    let __v = "foo"
                    let av = AST.ASTSelectAttributeReference $ AST.ASTSelectAttributeReferenceUnqualified (AST.ASTVariable __v)
                    let f = TC.inferSelectList TCX.freshContext
                    let e = Left $ TE.combineErrors [TC.__varNotKnownError __v]
                    THS.shouldBe (f [av]) e

            THS.describe "with no errors" $ do
                THS.it "returns Right <all types combined>" $ do
                    -- foo is an attribute of type Bool
                    let __v = "foo"
                    let __k = TCX.TCXSimpleTypeContextKey __v
                    let __vt = TAST.TASTAtomicTypeBool
                    -- it is known to the context
                    let __c = TCX.extend __k (TCX.contextualize __vt) TCX.freshContext
                    -- we are retrieving the types
                    let av = AST.ASTSelectAttributeReference $ AST.ASTSelectAttributeReferenceUnqualified (AST.ASTVariable __v)
                    let f = TC.inferSelectList __c
                    let __rk = TAST.TASTSimpleAtomicIndexKey __v
                    let __rki = TAST.TASTSimpleAtomicIndexPair $ TAST.TASTSimpleAtomicIndexKeyValue __rk __vt
                    let r = Right [__rki]
                    THS.shouldBe (f [av]) r

    --     THS.describe "with inferFromTable" $ do
    --         THS.describe "with a simple table access" $ do
    --             THS.describe "with known table name" $ do
    --                 THS.it "returns Right <inference result>" $ do
    --                     let __v = "foo"
    --                     let __k = TCX.TCSimpleTypeContextKey __v
    --                     let __vt = TAST.TASTSimpleTypeRecord TAST.emptyTypeRecord
    --                     let __c = TCX.extend __k __vt TCX.freshContext
    --                     let av = AST.ASTFromTableReference $ AST.ASTVariable __v
    --                     let f = TC.inferFromTable __c
    --                     let __rki = TAST.TASTSimpleAtomicIndexKeyValue (TAST.TASTSimpleAtomicIndexKey __v) __vt
    --                     let r = Right __rki
    --                     THS.shouldBe (f av) r

    --             THS.describe "with unknown table name" $ do
    --                 THS.it "returns Left <error result>" $ do
    --                     let __v = "foo"
    --                     let __k = TCX.TCSimpleTypeContextKey __v
    --                     let __vt = TAST.TASTSimpleTypeRecord TAST.emptyTypeRecord
    --                     let __c = TCX.freshContext
    --                     let av = AST.ASTFromTableReference $ AST.ASTVariable __v
    --                     let f = TC.inferFromTable __c
    --                     let r = Left $ TC.__varNotKnownError __v
    --                     THS.shouldBe (f av) r

    --         THS.describe "with a aliased table access" $ do
    --             THS.describe "with known table name" $ do
    --                 THS.it "returns Right <inference result>" $ do
    --                     let __v = "foo"
    --                     let __k = TCX.TCSimpleTypeContextKey __v
    --                     let __vt = TAST.TASTSimpleTypeRecord TAST.emptyTypeRecord
    --                     let __c = TCX.extend __k __vt TCX.freshContext
    --                     let __u = "bar"
    --                     let av = AST.ASTFromTableReferenceAlias (AST.ASTVariable __v) (AST.ASTSimpleAlias __u)
    --                     let f = TC.inferFromTable __c
    --                     let __rk = TAST.TASTSimpleAtomicIndexKey __u
    --                     let __rki = TAST.TASTSimpleAtomicIndexKeyValue __rk __vt
    --                     let r = Right __rki
    --                     THS.shouldBe (f av) r

    --     THS.describe "with inferFromList" $ do
    --         THS.describe "with at least one error" $ do
    --             THS.it "returns Left <all error messages combined>" $ do
    --                 let __v = "foo"
    --                 let av = AST.ASTFromTableReference $ AST.ASTVariable __v
    --                 let f = TC.inferFromList TCX.freshContext
    --                 let e = Left $ TC.combineErrors [TC.__varNotKnownError __v]
    --                 THS.shouldBe (f [av]) e

    --         THS.describe "with no errors" $ do
    --             THS.describe "without any aliases" $ do
    --                 THS.it "returns Right <all types combined>" $ do
    --                     let __v = "foo"
    --                     let __k = TCX.TCSimpleTypeContextKey __v
    --                     let __vt = TAST.TASTSimpleTypeRecord TAST.emptyTypeRecord
    --                     let __c = TCX.extend __k __vt TCX.freshContext
    --                     let av = AST.ASTFromTableReference $ AST.ASTVariable __v
    --                     let f = TC.inferFromList __c
    --                     let __rk = TAST.TASTSimpleAtomicIndexKey __v
    --                     let __rki = TAST.TASTSimpleAtomicIndexKeyValue __rk __vt
    --                     let r = Right [__rki]
    --                     THS.shouldBe (f [av]) r

    --             THS.describe "with an aliases" $ do
    --                 THS.it "returns Right <all types combined>" $ do
    --                     let __v = "foo"
    --                     let __k = TCX.TCSimpleTypeContextKey __v
    --                     let __vt = TAST.TASTSimpleTypeRecord TAST.emptyTypeRecord
    --                     let __c = TCX.extend __k __vt TCX.freshContext
    --                     let __u = "bar"
    --                     let av = AST.ASTFromTableReferenceAlias (AST.ASTVariable __v) (AST.ASTSimpleAlias __u)
    --                     let f = TC.inferFromList __c
    --                     let __rk = TAST.TASTSimpleAtomicIndexKey __u
    --                     let __rki = TAST.TASTSimpleAtomicIndexKeyValue __rk __vt
    --                     let r = Right [__rki]
    --                     THS.shouldBe (f [av]) r
