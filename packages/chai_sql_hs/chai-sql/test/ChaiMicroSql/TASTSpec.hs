module ChaiMicroSql.TASTSpec (spec) where

import qualified ChaiMicroSql.TAST as TAST
import qualified Data.Map          as M
import qualified Test.Hspec        as THS



spec :: THS.Spec
spec = do
    THS.describe "Type AST of" $ do
        THS.describe "a simple record" $ do
            THS.it "parses" $ do
                THS.shouldBe (p a) a
                where
                    a = TAST.TASTSimpleTypeRecord
                        $ M.fromList
                            [(TAST.TASTSimpleTypeBasicIndexKey "foo", TAST.TASTAtomicTypeBool)]
                    p = id -- TODO(backlog): define a parse function
