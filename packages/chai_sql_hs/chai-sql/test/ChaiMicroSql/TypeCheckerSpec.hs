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

        THS.describe "with inferVar" $ do
            THS.describe "with unknown variable" $ do
                THS.it "returns Left <error text>" $ do
                    let __v = "foo"
                    let av = AST.AstVariable __v
                    let f = TC.infer TCX.freshContext :: (AST.AstVariable -> Either TC.TcInferenceError TAST.TAstAtomicType)
                    let e = Left $ TC.__varNotKnownError __v
                    THS.shouldBe (f av) e

            THS.describe "with known variable with record type" $ do
                THS.it "returns Right <record type>" $ do
                    let __v = "foo"
                    let __avt = TAST.emptyRecord
                    let av = AST.AstVariable __v
                    let k = TCX.TCXSimpleTypeContextKey __v
                    let c = TCX.extend k (TCX.contextualize __avt) TCX.freshContext
                    let f = TC.infer c
                    let e = Right __avt
                    THS.shouldBe (f av) e

            THS.describe "with known variable with atomic type" $ do
                THS.it "returns Right <atomic type>" $ do
                    let __v = "foo"
                    let __avt = TAST.TAstAtomicTypeBool
                    let av = AST.AstVariable __v
                    let k = TCX.TCXSimpleTypeContextKey __v
                    let c = TCX.extend k (TCX.contextualize __avt) TCX.freshContext
                    let f = TC.infer c
                    let e = Right __avt
                    THS.shouldBe (f av) e

        THS.describe "with inferTotalRecord" $ do
            THS.describe "with a total record" $ do
                THS.it "returns Right <total record type>" $ do
                    let a = AST.AstSelectAttributeStarTotalRecord
                    let f = TC.infer TCX.freshContext
                    let e = Right TAST.TAstSimpleTypeRecordTotal
                    THS.shouldBe (f a) e

        THS.describe "with inferAttributeReference" $ do
            THS.describe "with a non-qualified access" $ do
                THS.describe "with known attribute" $ do
                    THS.it "returns Right <inference result>" $ do
                        let __v = "foo"
                        let __k = TCX.TCXSimpleTypeContextKey __v
                        let __vt = TAST.TAstAtomicTypeBool
                        let __c = TCX.extend __k (TCX.contextualize __vt) TCX.freshContext
                        let av = AST.AstSelectAttributeReferenceUnqualified  $ AST.AstVariable __v
                        let f = TC.infer __c
                        let e = Right $ TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey __v) __vt
                        THS.shouldBe (f av) e

                THS.describe "with unknown attribute" $ do
                    THS.it "returns Left <error result>" $ do
                        let __v = "foo"
                        let __k = TCX.TCXSimpleTypeContextKey __v
                        let __vt = TAST.TAstAtomicTypeBool
                        let __c = TCX.freshContext
                        let av = AST.AstSelectAttributeReferenceUnqualified  $ AST.AstVariable __v
                        let f = TC.infer __c
                        let e = Left $ TC.__varNotKnownError __v
                        THS.shouldBe (f av) e

            THS.describe "with a qualified access" $ do
                THS.describe "with a simple record base" $ do
                    THS.describe "with an unknown attribute" $ do
                        THS.it "returns Left <error text>" $ do
                            -- attribute type
                            let __v = "bar"
                            let __av = AST.AstVariable __v
                            let __vt = TAST.TAstAtomicTypeBool
                            -- base type
                            let __b = "foo"
                            let __ab = AST.AstVariable __b
                            let __bt = TAST.emptyRecord
                            -- context stuff
                            let __k = TCX.TCXSimpleTypeContextKey __b
                            let __c = TCX.extend __k (TCX.contextualize __bt) TCX.freshContext
                            -- abstract syntax nodes
                            let abv = AST.AstSelectAttributeReferenceQualified  __ab __av
                            let f = TC.infer __c
                            let e = Left $ TC.__recordUnknownAttributeError __ab __av
                            THS.shouldBe (f abv) e

                    THS.describe "with an known attribute" $ do
                        THS.it "returns Right <record index type>" $ do
                            -- attribute type
                            let __v = "bar"
                            let __av = AST.AstVariable __v
                            let __vt = TAST.TAstAtomicTypeBool
                            -- base type
                            let __b = "foo"
                            let __ab = AST.AstVariable __b
                            let __bvk = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey __v) __vt
                            let __bt = TAST.makeRecord [__bvk]
                            -- context stuff
                            let __k = TCX.TCXSimpleTypeContextKey __b
                            let __c = TCX.extend __k (TCX.contextualize __bt) TCX.freshContext
                            -- abstract syntax nodes
                            let abv = AST.AstSelectAttributeReferenceQualified  __ab __av
                            let f = TC.infer __c
                            let r = Right __bvk
                            THS.shouldBe (f abv) r

        THS.describe "with inferAttribute" $ do
            THS.describe "with a star access" $ do
                THS.it "returns Right <total record type>" $ do
                    let a = AST.AstSelectAttributeAccessStar  AST.AstSelectAttributeStarTotalRecord
                    let f = TC.infer TCX.freshContext
                    let r = Right TAST.TAstSimpleTypeRecordTotal
                    THS.shouldBe (f a) r

            THS.describe "with direct access" $ do
                THS.describe "with a known variable" $ do
                    THS.it "returns Right <inferred type>" $ do
                        -- foo is an attribute of type Bool
                        let __v = "foo"
                        let __k = TCX.TCXSimpleTypeContextKey __v
                        let __vt = TAST.TAstAtomicTypeBool
                        -- it is known to the context
                        let __c = TCX.extend __k (TCX.contextualize __vt) TCX.freshContext
                        -- we are directly accessing foo
                        let a = AST.AstSelectAttributeAccessReference  $ AST.AstSelectAttributeReferenceUnqualified  (AST.AstVariable __v)
                        let f = TC.infer __c
                        let r = Right $ TAST.TAstSimpleAtomicIndexPair $ TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey __v) __vt
                        THS.shouldBe (f a) r

                THS.describe "with an unknown variable" $ do
                    THS.it "returns Left <error message>" $ do
                        -- foo is an attribute of type Bool
                        let __v = "foo"
                        let __k = TCX.TCXSimpleTypeContextKey __v
                        let __vt = TAST.TAstAtomicTypeBool
                        -- it is known to the context
                        let __c = TCX.freshContext
                        -- we are directly accessing foo
                        let a = AST.AstSelectAttributeAccessReference  $ AST.AstSelectAttributeReferenceUnqualified  (AST.AstVariable __v)
                        let f = TC.infer __c
                        let r = Left $ TC.__varNotKnownError __v
                        THS.shouldBe (f a) r

            THS.describe "with a alias access" $ do
                THS.describe "with a known variable and alias" $ do
                    THS.it "returns Right <inferred type> with renamed index" $ do
                        -- foo is an attribute of type Bool
                        let __v = "foo"
                        let __u = "bar"
                        let __k = TCX.TCXSimpleTypeContextKey __v
                        let __vt = TAST.TAstAtomicTypeBool
                        -- it is known to the context
                        let __c = TCX.extend __k (TCX.contextualize __vt) TCX.freshContext
                        -- we are directly accessing foo
                        let a = AST.AstSelectAttributeAccessReferenceAlias  (AST.AstSelectAttributeReferenceUnqualified  (AST.AstVariable __v)) (AST.AstSimpleAlias __u)
                        let f = TC.infer __c
                        let r = Right $ TAST.TAstSimpleAtomicIndexPair $ TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey __u) __vt
                        THS.shouldBe (f a) r

        THS.describe "with inferSelectList" $ do
            THS.describe "with at least one error" $ do
                THS.it "returns Left <all error messages combined>" $ do
                    let __v = "foo"
                    let av = AST.AstSelectAttributeAccessReference  $ AST.AstSelectAttributeReferenceUnqualified  (AST.AstVariable __v)
                    let f = TC.infer TCX.freshContext
                    let e = Left $ TE.combineErrors [TC.__varNotKnownError __v]
                    THS.shouldBe (f [av]) e

            THS.describe "with no errors" $ do
                THS.it "returns Right <all types combined>" $ do
                    -- foo is an attribute of type Bool
                    let __v = "foo"
                    let __k = TCX.TCXSimpleTypeContextKey __v
                    let __vt = TAST.TAstAtomicTypeBool
                    -- it is known to the context
                    let __c = TCX.extend __k (TCX.contextualize __vt) TCX.freshContext
                    -- we are retrieving the types
                    let av = AST.AstSelectAttributeAccessReference  $ AST.AstSelectAttributeReferenceUnqualified  (AST.AstVariable __v)
                    let f = TC.infer __c
                    let __rk = TAST.TAstSimpleIndexKey __v
                    let __rki = TAST.TAstSimpleAtomicIndexPair $ TAST.TAstSimpleAtomicIndexKeyValue __rk __vt
                    let r = Right [__rki]
                    THS.shouldBe (f [av]) r

        THS.describe "with inferFromTable" $ do
            THS.describe "with a simple table access" $ do
                THS.describe "with known table name" $ do
                    THS.it "returns Right <inference result>" $ do
                        let __v = "foo"
                        let __k = TCX.TCXSimpleTypeContextKey __v
                        let __vt = TAST.emptyRecord
                        let __c = TCX.extend __k (TCX.contextualize __vt) TCX.freshContext
                        let av = AST.AstFromAccessReference  $ AST.AstVariable __v
                        let f = TC.infer __c
                        let __rki = TAST.TAstSimpleRecordIndexKeyValue (TAST.TAstSimpleIndexKey __v) __vt
                        let r = Right __rki
                        THS.shouldBe (f av) r

                THS.describe "with unknown table name" $ do
                    THS.it "returns Left <error result>" $ do
                        let __v = "foo"
                        let __k = TCX.TCXSimpleTypeContextKey __v
                        let __c = TCX.freshContext
                        let av = AST.AstFromAccessReference  $ AST.AstVariable __v
                        let f = TC.infer __c
                        let r = Left $ TC.__varNotKnownError __v
                        THS.shouldBe (f av) r

            THS.describe "with a aliased table access" $ do
                THS.describe "with known table name" $ do
                    THS.it "returns Right <inference result>" $ do
                        let __v = "foo"
                        let __k = TCX.TCXSimpleTypeContextKey __v
                        let __vt = TAST.emptyRecord
                        let __c = TCX.extend __k (TCX.contextualize __vt) TCX.freshContext
                        let __u = "bar"
                        let av = AST.AstFromAccessReferenceAlias  (AST.AstVariable __v) (AST.AstSimpleAlias __u)
                        let f = TC.infer __c
                        let __rk = TAST.TAstSimpleIndexKey __u
                        let __rki = TAST.TAstSimpleRecordIndexKeyValue __rk __vt
                        let r = Right __rki
                        THS.shouldBe (f av) r

            THS.describe "with an aliased sub-query" $ do
                THS.describe "with working query" $ do
                    THS.it "returns Right <inference result>" $ do
                        -- construct the context
                        let __f = "foos"
                        let __a = "id"
                        let __fIdT = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey __a) TAST.TAstAtomicTypeBool
                        let __fNameT= TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "name") TAST.TAstAtomicTypeBool
                        let __fas = [__fIdT, __fNameT]
                        let __ft = TAST.makeRecord __fas
                        -- construct a query from SELECT * FROM foos;
                        let qsl = [AST.AstSelectAttributeAccessStar  AST.AstSelectAttributeStarTotalRecord]
                        let qfl = [AST.AstFromAccessReference  (AST.AstVariable __f)]
                        let q = AST.AstSelectQuery  qsl qfl
                        -- populate starting context
                        let c = TCX.extend (TCX.makeKey __f) (TCX.contextualize __ft) TCX.freshContext
                        let __u = "bar"
                        let av = AST.AstFromAccessNestedQueryAlias q (AST.AstSimpleAlias __u)
                        let f = TC.infer c
                        let __rk = TAST.TAstSimpleIndexKey __u
                        let __rki = TAST.TAstSimpleRecordIndexKeyValue __rk $ TAST.makeRecord [__fIdT, __fNameT]
                        let r = Right __rki
                        THS.shouldBe (f av) r

                THS.describe "with working query with duplicate columns" $ do
                    THS.it "returns deduplicated record" $ do
                        -- construct the context
                        let __f = "foos"
                        let __a = "id"
                        let __n = "name"
                        let __fIdT = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey __a) TAST.TAstAtomicTypeBool
                        let __fNameT= TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey __n) TAST.TAstAtomicTypeBool
                        let __fas = [__fIdT, __fNameT]
                        let __ft = TAST.makeRecord __fas
                        -- construct a query from SELECT id, id, name FROM foos;
                        let __qslId = AST.AstSelectAttributeAccessReference  $ AST.AstSelectAttributeReferenceUnqualified  $ AST.AstVariable __a
                        let __qslName = AST.AstSelectAttributeAccessReference  $ AST.AstSelectAttributeReferenceUnqualified  $ AST.AstVariable __n
                        let qsl = [__qslId, __qslId, __qslName]
                        let qfl = [AST.AstFromAccessReference  $ AST.AstVariable __f]
                        let q = AST.AstSelectQuery  qsl qfl
                        -- populate starting context
                        let c = TCX.extend (TCX.makeKey __f) (TCX.contextualize __ft) TCX.freshContext
                        let __u = "bar"
                        let av = AST.AstFromAccessNestedQueryAlias q (AST.AstSimpleAlias __u)
                        let f = TC.infer c
                        let __rk = TAST.TAstSimpleIndexKey __u
                        -- prepare a de-duplicated result
                        let __fIdT1 = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey $ __a ++ ":1") TAST.TAstAtomicTypeBool
                        let __rki = TAST.TAstSimpleRecordIndexKeyValue __rk $ TAST.makeRecord [__fIdT, __fIdT1, __fNameT]
                        let r = Right __rki
                        THS.shouldBe (f av) r

        THS.describe "with inferFromList" $ do
            THS.describe "with at least one error" $ do
                THS.it "returns Left <all error messages combined>" $ do
                    let __v = "foo"
                    let av = AST.AstFromAccessReference  $ AST.AstVariable __v
                    let f = TC.infer TCX.freshContext
                    let e = Left $ TE.combineErrors [TC.__varNotKnownError __v]
                    THS.shouldBe (f [av]) e

            THS.describe "with no errors" $ do
                THS.describe "without any aliases" $ do
                    THS.it "returns Right <all types combined>" $ do
                        let __v = "foo"
                        let __k = TCX.TCXSimpleTypeContextKey __v
                        let __vt = TAST.emptyRecord
                        let __c = TCX.extend __k (TCX.contextualize __vt) TCX.freshContext
                        let av = AST.AstFromAccessReference  $ AST.AstVariable __v
                        let f = TC.infer __c
                        let __rk = TAST.TAstSimpleIndexKey __v
                        let __rki = TAST.TAstSimpleRecordIndexKeyValue __rk __vt
                        let r = Right [__rki]
                        THS.shouldBe (f [av]) r

                THS.describe "with an aliases" $ do
                    THS.it "returns Right <all types combined>" $ do
                        let __v = "foo"
                        let __k = TCX.TCXSimpleTypeContextKey __v
                        let __vt = TAST.emptyRecord
                        let __c = TCX.extend __k (TCX.contextualize __vt) TCX.freshContext
                        let __u = "bar"
                        let av = AST.AstFromAccessReferenceAlias  (AST.AstVariable __v) (AST.AstSimpleAlias __u)
                        let f = TC.infer __c
                        let __rk = TAST.TAstSimpleIndexKey __u
                        let __rki = TAST.TAstSimpleRecordIndexKeyValue __rk __vt
                        let r = Right [__rki]
                        THS.shouldBe (f [av]) r

        THS.describe "with inferSelectQuery" $ do
            THS.describe "with star selection" $ do
                THS.it "returns all attributes from the from list" $ do
                    -- construct the context
                    let __f = "foos"
                    let __fIdT = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "id") TAST.TAstAtomicTypeBool
                    let __fNameT= TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "name") TAST.TAstAtomicTypeBool
                    let __fas = [__fIdT, __fNameT]
                    let __ft = TAST.makeRecord __fas
                    -- construct a query from SELECT * FROM foos;
                    let qsl = [AST.AstSelectAttributeAccessStar  AST.AstSelectAttributeStarTotalRecord]
                    let qfl = [AST.AstFromAccessReference  $ AST.AstVariable __f]
                    let q = AST.AstSelectQuery  qsl qfl
                    -- populate starting context
                    let c = TCX.extend (TCX.makeKey __f) (TCX.contextualize __ft) TCX.freshContext
                    -- infer query
                    let f = TC.infer c
                    -- check
                    let e = Right [__fIdT, __fNameT]
                    THS.shouldBe (f q) e

            THS.describe "with star selection and cross product" $ do
                THS.it "returns all attributes from the from list" $ do
                    -- construct the context
                    let __f = "foos"
                    let __fIdT = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "id") TAST.TAstAtomicTypeBool
                    let __fNameT= TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "name") TAST.TAstAtomicTypeBool
                    let __fas = [__fIdT, __fNameT]
                    let __ft = TAST.makeRecord __fas
                    -- construct the context
                    let __b = "bars"
                    let __bIdT = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "id") TAST.TAstAtomicTypeBool
                    let __bLocationT= TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "location") TAST.TAstAtomicTypeBool
                    let __bas = [__bIdT, __bLocationT]
                    let __bt = TAST.makeRecord __bas
                    -- construct a query from SELECT * FROM foos;
                    let qsl = [AST.AstSelectAttributeAccessStar  AST.AstSelectAttributeStarTotalRecord]
                    let qfl = [AST.AstFromAccessReference  $ AST.AstVariable __f, AST.AstFromAccessReference  $ AST.AstVariable __b]
                    let q = AST.AstSelectQuery  qsl qfl
                    -- populate starting context
                    let __c = TCX.extend (TCX.makeKey __f) (TCX.contextualize __ft) TCX.freshContext
                    let c = TCX.extend (TCX.makeKey __b) (TCX.contextualize __bt) __c
                    -- infer query
                    let f = TC.infer c
                    -- check
                    let e = Right [__fIdT, __fNameT, __bIdT, __bLocationT]
                    THS.shouldBe (f q) e

            THS.describe "with qualified selection and cross product" $ do
                THS.it "returns the required attribute" $ do
                    -- construct the context
                    let __f = "foos"
                    let __fIdT = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "id") TAST.TAstAtomicTypeBool
                    let __fNameT= TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "name") TAST.TAstAtomicTypeBool
                    let __fas = [__fIdT, __fNameT]
                    let __ft = TAST.makeRecord __fas
                    -- construct the context
                    let __b = "bars"
                    let __ba = "b"
                    let __bIdT = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "id") TAST.TAstAtomicTypeBool
                    let __l = "location"
                    let __bLocationT= TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey __l) TAST.TAstAtomicTypeBool
                    let __bas = [__bIdT, __bLocationT]
                    let __bt = TAST.makeRecord __bas
                    -- construct a query from SELECT * FROM foos;
                    let qsl = [AST.AstSelectAttributeAccessReference  $ AST.AstSelectAttributeReferenceQualified  (AST.AstVariable __ba) (AST.AstVariable __l) ]
                    let qfl = [AST.AstFromAccessReference  $ AST.AstVariable __f, AST.AstFromAccessReferenceAlias  (AST.AstVariable __b) (AST.AstSimpleAlias __ba)]
                    let q = AST.AstSelectQuery  qsl qfl
                    -- populate starting context
                    let __c = TCX.extend (TCX.makeKey __f) (TCX.contextualize __ft) TCX.freshContext
                    let c = TCX.extend (TCX.makeKey __b) (TCX.contextualize __bt) __c
                    -- infer query
                    let f = TC.infer c
                    -- check
                    let e = Right [__bLocationT]
                    THS.shouldBe (f q) e

            THS.describe "with attribute selection" $ do
                THS.it "returns specific attributes" $ do
                    -- construct the context
                    let __f = "foos"
                    let __a = "id"
                    let __fIdT = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey __a) TAST.TAstAtomicTypeBool
                    let __fNameT= TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "name") TAST.TAstAtomicTypeBool
                    let __fas = [__fIdT, __fNameT]
                    let __ft = TAST.makeRecord __fas
                    -- construct a query from SELECT id FROM foos;
                    let qsl = [AST.AstSelectAttributeAccessReference  $ AST.AstSelectAttributeReferenceUnqualified  $ AST.AstVariable __a]
                    let qfl = [AST.AstFromAccessReference  $ AST.AstVariable __f]
                    let q = AST.AstSelectQuery  qsl qfl
                    -- populate starting context
                    let c = TCX.extend (TCX.makeKey __f) (TCX.contextualize __ft) TCX.freshContext
                    -- infer query
                    let f = TC.infer c
                    -- check
                    let e = Right [__fIdT]
                    THS.shouldBe (f q) e

            THS.describe "with invalid attribute selection" $ do
                THS.it "fails" $ do
                    -- construct the context
                    let __f = "foos"
                    let __a = "id"
                    let __fIdT = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey __a) TAST.TAstAtomicTypeBool
                    let __fNameT= TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "name") TAST.TAstAtomicTypeBool
                    let __fas = [__fIdT, __fNameT]
                    let __ft = TAST.makeRecord __fas
                    -- construct a query from SELECT thisIsNotAnId FROM foos;
                    let __na = "thisIsNotAnId"
                    let qsl = [AST.AstSelectAttributeAccessReference  $ AST.AstSelectAttributeReferenceUnqualified  $ AST.AstVariable __na]
                    let qfl = [AST.AstFromAccessReference  $ AST.AstVariable __f]
                    let q = AST.AstSelectQuery  qsl qfl
                    -- populate starting context
                    let c = TCX.extend (TCX.makeKey __f) (TCX.contextualize __ft) TCX.freshContext
                    -- infer query
                    let f = TC.infer c
                    -- check that it reports all the failures
                    let e = Left $ TE.combineErrors [TC.__varNotKnownError __na]
                    THS.shouldBe (f q) e

            THS.describe "with mistaken table name for attribute selection" $ do
                THS.it "fails" $ do
                    -- construct the context
                    let __f = "foos"
                    let __a = "id"
                    let __fIdT = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey __a) TAST.TAstAtomicTypeBool
                    let __fNameT= TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "name") TAST.TAstAtomicTypeBool
                    let __fas = [__fIdT, __fNameT]
                    let __ft = TAST.makeRecord __fas
                    -- construct a query from SELECT thisIsNotAnId FROM foos;
                    let __na = "foos"
                    let qsl = [AST.AstSelectAttributeAccessReference  $ AST.AstSelectAttributeReferenceUnqualified  $ AST.AstVariable __na]
                    let qfl = [AST.AstFromAccessReference  $ AST.AstVariable __f]
                    let q = AST.AstSelectQuery  qsl qfl
                    -- populate starting context
                    let c = TCX.extend (TCX.makeKey __f) (TCX.contextualize __ft) TCX.freshContext
                    -- infer query
                    let f = TC.infer c
                    -- check that it reports all the failures
                    let e = Left $ TE.combineErrors [TCX.__recordNotAtomError]
                    THS.shouldBe (f q) e

            THS.describe "with atrribute not in soiurce but in context" $ do
                THS.it "fails" $ do
                    -- construct the context
                    let __f = "foos"
                    let __a = "id"
                    let __fIdT = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey __a) TAST.TAstAtomicTypeBool
                    let __fNameT= TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "name") TAST.TAstAtomicTypeBool
                    let __fas = [__fIdT, __fNameT]
                    let __ft = TAST.makeRecord __fas
                    -- construct a query from SELECT thisIsNotAnId FROM foos;
                    let __na = "ghost-attribute"
                    let __nat = TAST.TAstAtomicTypeBool
                    let qsl = [AST.AstSelectAttributeAccessReference  $ AST.AstSelectAttributeReferenceUnqualified  $ AST.AstVariable __na]
                    let qfl = [AST.AstFromAccessReference  $ AST.AstVariable __f]
                    let q = AST.AstSelectQuery  qsl qfl
                    -- populate starting context
                    let __c = TCX.extend (TCX.makeKey __f) (TCX.contextualize __ft) TCX.freshContext
                    -- populate starting context with a ghost attribute
                    let c = TCX.extend (TCX.makeKey __na) (TCX.contextualize __nat) __c
                    -- infer query
                    let f = TC.infer c
                    -- check that it reports all the failures
                    let e = Left $ TE.combineErrors [TC.__attributeNotInSourceError $ TAST.TAstSimpleAtomicIndexKeyValue (TAST.makeKey __na) __nat]
                    THS.shouldBe (f q) e
