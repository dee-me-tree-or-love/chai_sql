module ChaiMicroSql.TypeContextSpec (spec) where

import qualified ChaiMicroSql.TAST        as TAST
import qualified ChaiMicroSql.TypeContext as TCX
import qualified Data.Map                 as M
import qualified Test.Hspec               as THS



spec :: THS.Spec
spec = do
    THS.describe "Type context" $ do
        THS.describe "with freshContext" $ do
            THS.it "is empty" $ do
                let a = TCX.freshContext
                let p = M.toList
                THS.shouldBe (p a) []

        THS.describe "with extend" $ do
            THS.it "receives new items" $ do
                let k = TCX.TCSimpleTypeContextKey "foo"
                let v = TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool
                let a = TCX.freshContext
                let f = TCX.extend k v
                let p = M.toList
                THS.shouldBe (p $ f a) [(k, v)]

        THS.describe "with lookup" $ do
            THS.describe "when not found" $ do
                THS.it "returns Nothing" $ do
                    let k = TCX.TCSimpleTypeContextKey "foo"
                    THS.shouldBe (TCX.get k TCX.freshContext) Nothing

            THS.describe "when found" $ do
                THS.it "returns Just <item>" $ do
                    let k = TCX.TCSimpleTypeContextKey "foo"
                    let v = TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool
                    let a = TCX.extend k v TCX.freshContext
                    THS.shouldBe (TCX.get k a) (Just v)
