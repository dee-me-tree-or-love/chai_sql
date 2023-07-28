module ChaiMicroSql.TypeContextSpec (spec) where

import qualified ChaiMicroSql.TAST        as TAST
import qualified ChaiMicroSql.TypeContext as TCX
import qualified Data.Map                 as M
import qualified Test.Hspec               as THS



spec :: THS.Spec
spec = do
    THS.describe "Type context" $ do
        THS.describe "with contextualize" $ do
            THS.describe "with atomic" $ do
                THS.it "is returns contextualized atom" $ do
                    let a = TAST.TAstAtomicTypeBool
                    let f = TCX.contextualize
                    let e = TCX.TCXSimpleTypeContextValueAtomic a
                    THS.shouldBe (f a) e

            THS.describe "with record" $ do
                THS.it "is returns contextualized record" $ do
                    let a = TAST.emptyRecord
                    let f = TCX.contextualize
                    let e = TCX.TCXSimpleTypeContextValueRecord a
                    THS.shouldBe (f a) e

        THS.describe "with decontextualize" $ do
            THS.describe "with contextualized atomic" $ do
                THS.it "is returns an atom" $ do
                    let __a = TAST.TAstAtomicTypeBool
                    let a = TCX.TCXSimpleTypeContextValueAtomic __a
                    let f = TCX.decontextualize
                    let e = Right __a
                    THS.shouldBe (f a) e

                THS.describe "but expected record" $ do
                    THS.it "is returns an error" $ do
                        let __a = TAST.TAstAtomicTypeBool
                        let a = TCX.TCXSimpleTypeContextValueAtomic __a
                        let f = TCX.decontextualize :: (TCX.TCXSimpleTypeContextValue -> Either TCX.TCXContextError TAST.TAstSimpleTypeRecord)
                        let e = Left TCX.__atomNotRecordError
                        THS.shouldBe (f a) e

            THS.describe "with contextualized record" $ do
                THS.it "is returns a record" $ do
                    let __a = TAST.emptyRecord
                    let a = TCX.TCXSimpleTypeContextValueRecord __a
                    let f = TCX.decontextualize
                    let e = Right __a
                    THS.shouldBe (f a) e


                THS.describe "but expected atom" $ do
                    THS.it "is returns an error" $ do
                        let __a = TAST.emptyRecord
                        let a = TCX.TCXSimpleTypeContextValueRecord __a
                        let f = TCX.decontextualize :: (TCX.TCXSimpleTypeContextValue -> Either TCX.TCXContextError TAST.TAstAtomicType)
                        let e = Left TCX.__recordNotAtomError
                        THS.shouldBe (f a) e

        THS.describe "with freshContext" $ do
            THS.it "is empty" $ do
                let a = TCX.freshContext
                let p = M.toList
                THS.shouldBe (p a) []

        THS.describe "with extend" $ do
            THS.it "receives new items" $ do
                let k = TCX.TCXSimpleTypeContextKey "foo"
                let v = TCX.contextualize TAST.TAstAtomicTypeBool
                let a = TCX.freshContext
                let f = TCX.extend k v
                let p = M.toList
                THS.shouldBe (p $ f a) [(k, v)]

        THS.describe "with lookup" $ do
            THS.describe "when not found" $ do
                THS.it "returns Nothing" $ do
                    let k = TCX.TCXSimpleTypeContextKey "foo"
                    THS.shouldBe (TCX.get k TCX.freshContext) Nothing

            THS.describe "when found" $ do
                THS.it "returns Just <item>" $ do
                    let k = TCX.TCXSimpleTypeContextKey "foo"
                    let v = TCX.contextualize TAST.TAstAtomicTypeBool
                    let a = TCX.extend k v TCX.freshContext
                    THS.shouldBe (TCX.get k a) (Just v)

        THS.describe "with makeKey" $ do
            THS.it "constructs a new key" $ do
                let a = "foo"
                let f = TCX.makeKey
                let e = TCX.TCXSimpleTypeContextKey "foo"
                THS.shouldBe (f a) e
