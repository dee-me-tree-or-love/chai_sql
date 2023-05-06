module ChaiMicroSql.TypeErrorsSpec (spec) where

import qualified ChaiMicroSql.TypeErrors as TE
import qualified Test.Hspec               as THS


spec :: THS.Spec
spec = do
    THS.describe "Type errors" $ do

        THS.describe "emptyError" $ do
            THS.it "returns an empty error" $ do
                let a = TE.emptyError
                let e = TE.makeError ""
                THS.shouldBe a e

        THS.describe "joinErrors" $ do
            THS.describe "one empty and one with content" $ do
                THS.it "returns a bullet point" $ do
                    let a = TE.emptyError
                    let b = TE.makeError "oups"
                    let f = TE.joinErrors
                    let e = TE.makeError "- oups"
                    THS.shouldBe (f a b) e

            THS.describe "Two with content" $ do
                THS.it "returns first followed by a bullet pointed second" $ do
                    let a = TE.makeError "ouch"
                    let b = TE.makeError "oups"
                    let f = TE.joinErrors
                    let e = TE.makeError "ouch\n- oups"
                    THS.shouldBe (f a b) e

        THS.describe "combineErrors" $ do
            THS.it "joins all in a single large error" $ do
                let a = TE.makeError "oh"
                let b = TE.makeError "oups"
                let c = TE.makeError "ouch"
                let f = TE.combineErrors
                let e = TE.makeError "- oh\n- oups\n- ouch"
                THS.shouldBe (f [a, b, c]) e