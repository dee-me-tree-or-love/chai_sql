{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module ChaiMicroSql.Loaders.YamlContextLoader (load) where

import qualified ChaiMicroSql.TAST        as TAST
import qualified ChaiMicroSql.TypeContext as TCX
import qualified ChaiMicroSql.TypeErrors  as TE
import           Data.ByteString          (ByteString)
import           Data.Either              (isLeft, lefts, rights)
import           Data.List                (intercalate)
import qualified Data.Yaml                as Y
import           GHC.Generics             (Generic)

{- FIXME: refine the documentation

Example YAML:

```yaml
tables:
    - title: Foo
      columns:
        - name: bar
          spec: Number
        - name: quack
          spec: Text
        - name: peep
          spec: Bool
```
 -}

-- YAML parsing of raw schemas
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~

newtype DbSchema = DbSchema {tables :: [TableSchema]}
  deriving (Eq, Show, Generic)

data TableSchema = TableSchema {title :: String, columns :: [ColumnSchema]}
  deriving (Eq, Show, Generic)

data ColumnSchema = ColumnSchema {name :: String, spec :: String}
  deriving (Eq, Show, Generic)

instance Y.FromJSON DbSchema

instance Y.FromJSON TableSchema

instance Y.FromJSON ColumnSchema

-- Application mapping of raw schemas
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Simple alias for a type checker's check error.
type YclLoaderError = TE.TEBaseError

-- | Application-level validation of the parsed schema.
class Validator a where
  validate :: a -> Either YclLoaderError a

-- | Validates the schema of the database.
--
--  Examples:
--
--  >>> validate (DbSchema [(TableSchema "foo" [ColumnSchema "bar" "Number"])])
--  Right (DbSchema {tables = [TableSchema {title = "foo", columns = [ColumnSchema {name = "bar", spec = "Number"}]}]})
--
--  >>> validate (DbSchema [(TableSchema "foo" [ColumnSchema "bar" "WrongSpecTypeValue!!!!", ColumnSchema "quack" "Number"])])
--  Left (TEBaseError ["Table `foo` contains invalid columns.","Unexpected `spec` value `WrongSpecTypeValue!!!!` provided for column `bar`. Accepting only `Number | Text | Bool`."])
instance Validator DbSchema where
  validate :: DbSchema -> Either YclLoaderError DbSchema
  validate v@(DbSchema ts) = do
    let vts = map validate ts
    if any isLeft vts
      then Left $ TE.combineErrors $ lefts vts
      else pure v

-- | Validates the schema of a table.
--
--  Examples:
--
--  >>> validate (TableSchema "foo" [ColumnSchema "bar" "Number"])
--  Right (TableSchema {title = "foo", columns = [ColumnSchema {name = "bar", spec = "Number"}]})
--
--  >>> validate (TableSchema "foo" [ColumnSchema "bar" "WrongSpecTypeValue!!!!", ColumnSchema "quack" "Number"])
--  Left (TEBaseError ["Table `foo` contains invalid columns.","Unexpected `spec` value `WrongSpecTypeValue!!!!` provided for column `bar`. Accepting only `Number | Text | Bool`."])
instance Validator TableSchema where
  validate :: TableSchema -> Either YclLoaderError TableSchema
  validate v@(TableSchema t cs) = do
    let vcs = map validate cs
    if any isLeft vcs
      then Left $ TE.combineErrors $ __tableInvalidError t : lefts vcs
      else pure v

__tableInvalidError :: String -> YclLoaderError
__tableInvalidError t = TE.makeError $ "Table `" ++ t ++ "` contains invalid columns."

-- TODO(tech-debt): find a more elegant solution to declare supported types.

-- | Validates the schema of a column.
--
--  Examples:
--
--  >>> validate (ColumnSchema "bar" "Number")
--  Right (ColumnSchema {name = "bar", spec = "Number"})
--
--  >>> validate (ColumnSchema "bar" "WrongSpecTypeValue")
--  Left (TEBaseError ["Unexpected `spec` value `WrongSpecTypeValue` provided for column `bar`. Accepting only `Number | Text | Bool`."])
instance Validator ColumnSchema where
  validate :: ColumnSchema -> Either YclLoaderError ColumnSchema
  validate v@(ColumnSchema _ "Number") = pure v
  validate v@(ColumnSchema _ "Text") = pure v
  validate v@(ColumnSchema _ "Bool") = pure v
  validate (ColumnSchema n s) = Left $ __specInvalidError n s ["Number", "Text", "Bool"]

-- | Produce an error for an unexpected spec provided for column
--
--  Examples:
--
--  >>> __specInvalidError "foo" "Quacks" ["Foos", "Bars"]
--  TEBaseError ["Unexpected `spec` value `Quacks` provided for column `foo`. Accepting only `Foos | Bars`."]
__specInvalidError :: String -> String -> [String] -> YclLoaderError
__specInvalidError n s ts = TE.makeError $ "Unexpected `spec` value `" ++ s ++ "` provided for column `" ++ n ++ "`. Accepting only `" ++ intercalate " | " ts ++ "`."

class Translator a b where
  translate :: a -> Either YclLoaderError b

-- | Translator from db schema to TypeAST elements.
--
-- Examples:
--
-- >>> translate (DbSchema [(TableSchema "foo" [ColumnSchema "bar" "Number"])]) :: Either YclLoaderError [TAST.TAstSimpleRecordIndexPair]
-- Right [TAstSimpleRecordIndexKeyValue (TAstSimpleIndexKey "foo") (fromList [(TAstSimpleIndexKey "bar",TAstAtomicTypeNumber)])]
--
-- >>> translate (DbSchema [(TableSchema "foo" [ColumnSchema "bar" "WrongSpecTypeValue!!!!", ColumnSchema "quack" "Number"])]) :: Either YclLoaderError [TAST.TAstSimpleRecordIndexPair]
-- Left (TEBaseError ["Unexpected context source encountered in schema.","Unexpected context source encountered in table: `foo`.","Unexpected context source encountered in column: `bar: WrongSpecTypeValue!!!!`."])
--
instance Translator DbSchema [TAST.TAstSimpleRecordIndexPair] where
  translate :: DbSchema -> Either YclLoaderError [TAST.TAstSimpleRecordIndexPair]
  translate (DbSchema ts) = do
    let tcs = map translate ts :: [Either YclLoaderError TAST.TAstSimpleRecordIndexPair]
    if any isLeft tcs
      then Left $ TE.combineErrors $ TE.makeError "Unexpected context source encountered in schema." : lefts tcs
      else pure $ rights tcs

-- | Translator from table schema to TypeAST elements.
--
-- Examples:
--
-- >>> translate (TableSchema "foo" [ColumnSchema "bar" "Number"]) :: Either YclLoaderError TAST.TAstSimpleRecordIndexPair
-- Right (TAstSimpleRecordIndexKeyValue (TAstSimpleIndexKey "foo") (fromList [(TAstSimpleIndexKey "bar",TAstAtomicTypeNumber)]))
--
-- >>> translate (TableSchema "foo" [ColumnSchema "bar" "WrongSpecTypeValue!!!!", ColumnSchema "quack" "Number"]) :: Either YclLoaderError TAST.TAstSimpleRecordIndexPair
-- Left (TEBaseError ["Unexpected context source encountered in table: `foo`.","Unexpected context source encountered in column: `bar: WrongSpecTypeValue!!!!`."])
--
instance Translator TableSchema TAST.TAstSimpleRecordIndexPair where
  translate :: TableSchema -> Either YclLoaderError TAST.TAstSimpleRecordIndexPair
  translate (TableSchema n cs) = do
    let tcs = map translate cs :: [Either YclLoaderError TAST.TAstSimpleAtomicIndexPair]
    if any isLeft tcs
      then Left $ TE.combineErrors $ TE.makeError ("Unexpected context source encountered in table: `" ++ n ++ "`.") : lefts tcs
      else pure $ TAST.TAstSimpleRecordIndexKeyValue (TAST.makeKey n) (TAST.makeRecord $ rights tcs)

-- | Translator from column schema to TypeAST elements.
--
-- Examples:
--
-- >>> translate (ColumnSchema "foo" "Number") :: Either YclLoaderError TAST.TAstSimpleAtomicIndexPair
-- Right (TAstSimpleAtomicIndexKeyValue (TAstSimpleIndexKey "foo") TAstAtomicTypeNumber)
--
-- >>> translate (ColumnSchema "foo" "WrongSpecType!!!!") :: Either YclLoaderError TAST.TAstSimpleAtomicIndexPair
-- Left (TEBaseError ["Unexpected context source encountered in column: `foo: WrongSpecType!!!!`."])
--
instance Translator ColumnSchema TAST.TAstSimpleAtomicIndexPair where
  translate :: ColumnSchema -> Either YclLoaderError TAST.TAstSimpleAtomicIndexPair
  translate (ColumnSchema n "Number") = pure $ TAST.TAstSimpleAtomicIndexKeyValue (TAST.makeKey n) TAST.TAstAtomicTypeNumber
  translate (ColumnSchema n "Text") = pure $ TAST.TAstSimpleAtomicIndexKeyValue (TAST.makeKey n) TAST.TAstAtomicTypeText
  translate (ColumnSchema n "Bool") = pure $ TAST.TAstSimpleAtomicIndexKeyValue (TAST.makeKey n) TAST.TAstAtomicTypeBool
  translate (ColumnSchema n s) = Left $ TE.makeError $ "Unexpected context source encountered in column: `" ++ n ++ ": " ++ s ++ "`."

-- Raw schemas to context
-- ~~~~~~~~~~~~~~~~~~~~~~

addToContext :: TAST.TAstSimpleRecordIndexPair -> TCX.TCXSimpleTypeContext -> TCX.TCXSimpleTypeContext
addToContext (TAST.TAstSimpleRecordIndexKeyValue k v) = TCX.extendContext (TCX.makeKey . TAST.unKey $ k) v

-- | Constructs a context from separate TAST elements.
--
-- Examples:
--
-- >>> buildContext [TAST.TAstSimpleRecordIndexKeyValue (TAST.TAstSimpleIndexKey "foo") (TAST.makeRecord [TAST.TAstSimpleAtomicIndexKeyValue (TAST.TAstSimpleIndexKey "bar") TAST.TAstAtomicTypeNumber])]
-- fromList [(TCXSimpleTypeContextKey "foo",TCXSimpleTypeContextValueRecord (fromList [(TAstSimpleIndexKey "bar",TAstAtomicTypeNumber)]))]
--
buildContext :: [TAST.TAstSimpleRecordIndexPair] -> TCX.TCXSimpleTypeContext
buildContext = foldr addToContext TCX.freshContext

wrappedDecode :: ByteString -> Either YclLoaderError DbSchema
wrappedDecode s = case Y.decodeEither' s of
  Left e  -> Left $ __decodeFailedError e
  Right v -> Right v

__decodeFailedError :: Y.ParseException -> YclLoaderError
__decodeFailedError e = TE.makeError $ "Error occurred when decoding the scheme: " ++ show e

-- | Safely loads the provided YAML schema into a context
--
-- Examples:
--
-- >>> load ("tables: []" :: ByteString)
-- Right (fromList [])
--
-- >>> load ("tables: [{\"title\": \"Foo\", \"columns\": []}]" :: ByteString)
-- Right (fromList [(TCXSimpleTypeContextKey "Foo",TCXSimpleTypeContextValueRecord (fromList []))])
--
-- >>> load ("tables: [{\"title\": \"Foo\", \"columns\": [{\"name\": \"bar\", \"spec\": \"Number\"}]}]" :: ByteString)
-- Right (fromList [(TCXSimpleTypeContextKey "Foo",TCXSimpleTypeContextValueRecord (fromList [(TAstSimpleIndexKey "bar",TAstAtomicTypeNumber)]))])
--
-- >>> load ("foo: {}" :: ByteString)
-- Left (TEBaseError ["Error occurred when decoding the scheme: AesonException \"Error in $: parsing ChaiMicroSql.Loaders.YamlContextLoader.DbSchema(DbSchema) failed, key \\\"tables\\\" not found\""])
--
load :: ByteString -> Either YclLoaderError TCX.TCXSimpleTypeContext
load s = do
  schema <- wrappedDecode s
  validated <- validate schema
  translated <- translate validated :: Either YclLoaderError [TAST.TAstSimpleRecordIndexPair]
  return $ buildContext translated
