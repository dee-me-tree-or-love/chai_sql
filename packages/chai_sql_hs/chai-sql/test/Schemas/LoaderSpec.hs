{-# Language TypeSynonymInstances #-}
module Schemas.LoaderSpec (spec) where

import qualified Schemas.Loader as SL
import qualified Test.Hspec      as TS


instance Eq SL.ParseException


joinMessage :: String -> String -> String
joinMessage = (++)

leftMessage :: String -> String -> String
leftMessage s _ = s

getTestCase ::
    String -> String -> 
        (String -> String -> String) -> 
            (String -> SL.ConfigReadOutput) -> 
                SL.ConfigReadOutput -> 
                    TS.SpecWith ()
getTestCase mp s mf tf e = TS.it (mf mp s) $ do TS.shouldBe (tf s) e


spec :: TS.Spec
spec = do
  TS.describe "LoaderSpec" $ do

    TS.describe "LoaderSpec.readConfigSchemaString" $ do

        getTestCase
            "reads simple packed config"
            (
                unlines [
                    "types: [String]", 
                    "tables: [{title: Pastry, attributes: [{name: ID, spec: String}, {name: name, spec: String}]}]"
                ]
            )
            leftMessage
            SL.readConfigSchemaString
            (
                Right (
                    SL.ConfigSchema {
                        SL.types = ["String"], 
                        SL.tables = [
                            SL.ConfigTableDefinition {
                                SL.title = "Pastry",
                                SL.attributes = [
                                    SL.ConfigAttributeDefinition {
                                        SL.name = "ID",
                                        SL.spec = "String"
                                    },
                                    SL.ConfigAttributeDefinition {
                                        SL.name = "name",
                                        SL.spec = "String"
                                    }
                                ]
                            }
                        ]}
                ) :: SL.ConfigReadOutput
            )