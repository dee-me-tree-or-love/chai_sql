module TypeChecker.ContextSpec (spec) where

import qualified Data.Map              as M
import qualified Schemas.Loader        as SL
import qualified TypeChecker.Context   as TCC
import qualified Language.Types.System as LTS
import qualified Test.Hspec            as TS


placeholderSchema :: SL.ConfigSchema
placeholderSchema
    = (SL.ConfigSchema
        {
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
            ]
        })


getTestCase :: (Show b, Eq b) => String -> a -> (a -> b) -> b -> TS.SpecWith ()
getTestCase m i tf e = TS.it m $ do TS.shouldBe (tf i) e

spec :: TS.Spec
spec = do
  TS.describe "ContextSpec" $ do

    TS.describe "ContextSpec.fromConfigSchema" $ do

        getTestCase
            "loads type context from a simple config"
            placeholderSchema
            (M.toList . (TCC.fromConfigSchema :: SL.ConfigSchema -> TCC.DbTypeContext))
            [(TCC.DbTypeName "String", LTS.TVTypeTerm "String")]

        getTestCase
            "loads table context from a simple config"
            placeholderSchema
            (M.toList . (TCC.fromConfigSchema :: SL.ConfigSchema -> TCC.DbTableContext))
            [( 
                TCC.DbTableName "Pastry",
                TCC.TypedDbTable
                    (TCC.DbTableName "Pastry")
                    (M.fromList [(TCC.DbAttributeName "ID", LTS.TVTypeTerm "String"), (TCC.DbAttributeName "name", LTS.TVTypeTerm "String")]) 
            )]