module ChaiMicroSql.TASTSpec (spec) where

import qualified ChaiMicroSql.CommonUtils as CU
import qualified ChaiMicroSql.TAST        as TAST
import qualified Data.Map                 as M
import qualified Test.Hspec               as THS


spec :: THS.Spec
spec = do
    THS.describe "Type AST of" $ do
        THS.describe "a simple record" $ do
            THS.it "parses" $ do
                let a = M.fromList [(TAST.TAstSimpleIndexKey "foo", TAST.TAstAtomicTypeBool)] :: TAST.TAstSimpleTypeRecord
                let p = id -- TODO(backlog): define a parse function
                THS.shouldBe (p a) a

    THS.describe "toString" $ do
        THS.describe "of IndexKey" $ do
            THS.it "returns key name" $ do
                let n = "foo"
                let v = TAST.TAstSimpleIndexKey n
                let f = CU.toString
                THS.shouldBe (f v) n

    THS.describe "Type records" $ do
        THS.describe "equal records" $ do
            THS.it "are equal" $ do
                let a = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "id") TAST.TAstAtomicTypeBool
                let b = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "name") TAST.TAstAtomicTypeBool
                let t = TAST.makeRecord [a, b]
                let a' = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "id") TAST.TAstAtomicTypeBool
                let b' = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "name") TAST.TAstAtomicTypeBool
                let t' = TAST.makeRecord [a', b']
                THS.shouldBe t t'

        THS.describe "non-equal records" $ do
            THS.it "are not equal" $ do
                let a = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "id") TAST.TAstAtomicTypeBool
                let b = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "name") TAST.TAstAtomicTypeBool
                let t = TAST.makeRecord [a, b]
                let a' = TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "id") TAST.TAstAtomicTypeBool
                let t' = TAST.makeRecord [a']
                THS.shouldBe (t /= t') True
