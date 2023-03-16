{-# LANGUAGE DeriveGeneric #-}

{-| Provides functionality for DB schema loading.


== YAML support

Example YAML schema config:

```yaml
types:
    - Number
    - String
    - Boolean

tables:
    - title: Pastry
      attributes:
        - name: ID
          spec: String
        - name: name
          spec: String
        - name: rarity
          spec: Number
```
 -}
module Schemas.Loader
    (
        module Schemas.Loader
    )
    where

import qualified Data.ByteString.Char8 as B (ByteString, pack)
import qualified Data.Yaml             as Y (FromJSON, ParseException,
                                             decodeEither', decodeFileEither)
import           GHC.Generics          (Generic)


-- Serializable configs
-- ~~~~~~~~~~~~~~~~~~~~

data ConfigSchema
    = ConfigSchema {
        types  :: ConfigTypes,
        tables :: ConfigTables
    }
    deriving (Show, Eq, Generic)

type ConfigTypes = [String]

type ConfigTables = [ConfigTableDefinition]

data ConfigTableDefinition
    = ConfigTableDefinition {
        title      :: String,
        attributes :: ConfigAttributes
    }
    deriving (Show, Eq, Generic)

type ConfigAttributes = [ConfigAttributeDefinition]

data ConfigAttributeDefinition
    = ConfigAttributeDefinition {
        name :: String,
        spec :: String
    }
    deriving  (Show, Eq, Generic)


-- YAML loading
-- ~~~~~~~~~~~~

instance Y.FromJSON ConfigSchema
instance Y.FromJSON ConfigTableDefinition
instance Y.FromJSON ConfigAttributeDefinition


type ParseException = Y.ParseException
type ConfigReadOutput = Either ParseException ConfigSchema

-- | Safely reads the schema from a YAML file.
readConfigSchemaFile :: FilePath -> IO ConfigReadOutput
readConfigSchemaFile = Y.decodeFileEither

-- | Safely reads the schema from a YAML byte string.
readConfigSchema :: B.ByteString -> ConfigReadOutput
readConfigSchema = Y.decodeEither'

-- | Safely reads the schema from a YAML string.
readConfigSchemaString :: String -> ConfigReadOutput
readConfigSchemaString = Y.decodeEither' . B.pack
