module ChaiMicroSql.Loaders.YamlContextLoader () where

import qualified ChaiMicroSql.TypeContext as TCX
import qualified ChaiMicroSql.TypeErrors  as TE
import           Data.Either              (isLeft, lefts)
import           Data.List                (intercalate)
import qualified Data.Yaml                as Y
import           GHC.Generics             (Generic)



{- TODO:

- Take in a YAML file describing the DB schema
- Parse the YAML file into the Context

Example YAML:

```yaml
tables:
    - title: Foo
      columns:
        - name: bar
          spec: Number
        - name: quack
          spec: String
        - name: peep
          spec: Boolean
```

 -}

newtype DbSchema = DbSchema { tables :: [TableSchema] }
    deriving (Eq, Show, Generic)

data TableSchema = TableSchema { title :: String, columns :: [ColumnSchema] }
    deriving (Eq, Show, Generic)

data ColumnSchema = ColumnSchema { name :: String, spec :: String }
    deriving (Eq, Show, Generic)

instance Y.FromJSON DbSchema
instance Y.FromJSON TableSchema
instance Y.FromJSON ColumnSchema

-- | Simple alias for a type checker's check error.
type YclLoaderError = TE.TEBaseError

-- | Application-level validation of the parsed schema.
class Validator a where
    validate :: a -> Either YclLoaderError a


-- |Validates the schema of the database.
--
-- Examples:
--
-- >>> validate (DbSchema [(TableSchema "foo" [ColumnSchema "bar" "Number"])])
-- Right (DbSchema {tables = [TableSchema {title = "foo", columns = [ColumnSchema {name = "bar", spec = "Number"}]}]})
--
-- >>> validate (DbSchema [(TableSchema "foo" [ColumnSchema "bar" "WrongSpecTypeValue!!!!", ColumnSchema "quack" "Number"])])
-- Left (TEBaseError ["Table `foo` contains invalid columns.","Unexpected `spec` value `WrongSpecTypeValue!!!!` provided for column `bar`. Accepting only `Number | String | Boolean`."])
--
instance Validator DbSchema where
    validate :: DbSchema -> Either YclLoaderError DbSchema
    validate v@(DbSchema ts) = do
        let vts = map validate ts
        if any isLeft vts
            then Left $ TE.combineErrors $ lefts vts
            else pure v

-- |Validates the schema of a table.
--
-- Examples:
--
-- >>> validate (TableSchema "foo" [ColumnSchema "bar" "Number"])
-- Right (TableSchema {title = "foo", columns = [ColumnSchema {name = "bar", spec = "Number"}]})
--
-- >>> validate (TableSchema "foo" [ColumnSchema "bar" "WrongSpecTypeValue!!!!", ColumnSchema "quack" "Number"])
-- Left (TEBaseError ["Table `foo` contains invalid columns.","Unexpected `spec` value `WrongSpecTypeValue!!!!` provided for column `bar`. Accepting only `Number | String | Boolean`."])
--
instance Validator TableSchema where
    validate :: TableSchema -> Either YclLoaderError TableSchema
    validate v@(TableSchema t cs) = do
        let vcs = map validate cs
        if any isLeft vcs
            then Left $ TE.combineErrors $ __tableInvalidError t : lefts vcs
            else pure v

__tableInvalidError :: String -> YclLoaderError
__tableInvalidError t = TE.makeError $ "Table `" ++ t ++ "` contains invalid columns."

-- |Validates the schema of a column.
--
-- Examples:
--
-- >>> validate (ColumnSchema "bar" "Number")
-- Right (ColumnSchema {name = "bar", spec = "Number"})
--
-- >>> validate (ColumnSchema "bar" "WrongSpecTypeValue")
-- Left (TEBaseError ["Unexpected `spec` value `WrongSpecTypeValue` provided for column `bar`. Accepting only `Number | String | Boolean`."])
--
instance Validator ColumnSchema where
    validate :: ColumnSchema -> Either YclLoaderError ColumnSchema
    validate v@(ColumnSchema _ "Number")  = pure v
    validate v@(ColumnSchema _ "String")  = pure v
    validate v@(ColumnSchema _ "Boolean") = pure v
    validate (ColumnSchema n s)         = Left $ __specInvalidError n s ["Number", "String", "Boolean"]

-- |Produce an error for an unexpected spec provided for column
--
-- Examples:
--
-- >>> __specInvalidError "foo" "Quacks" ["Foos", "Bars"]
-- TEBaseError ["Unexpected `spec` value `Quacks` provided for column `foo`. Accepting only `Foos | Bars`."]
--
__specInvalidError :: String -> String -> [String] -> YclLoaderError
__specInvalidError n s ts = TE.makeError $ "Unexpected `spec` value `" ++ s ++ "` provided for column `" ++ n ++ "`. Accepting only `" ++ intercalate " | " ts ++ "`."
