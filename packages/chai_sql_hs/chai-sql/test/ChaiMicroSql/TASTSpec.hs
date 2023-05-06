module ChaiMicroSql.TASTSpec (spec) where

import qualified ChaiMicroSql.TAST as TAST
import qualified Data.Map          as M
import qualified Test.Hspec        as THS



spec :: THS.Spec
spec = do
    THS.describe "Type AST of" $ do
        THS.describe "a simple record" $ do
            THS.it "parses" $ do
                let a = M.fromList [(TAST.TASTSimpleIndexKey "foo", TAST.TASTAtomicTypeBool)] :: TAST.TASTSimpleTypeRecord
                let p = id -- TODO(backlog): define a parse function
                THS.shouldBe (p a) a
