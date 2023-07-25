{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module ChaiMicroSql.Loaders.YamlContextLoader (YclLoaderError, load, loads) where

import qualified ChaiMicroSql.TAST        as TAST
import qualified ChaiMicroSql.TypeContext as TCX
import qualified ChaiMicroSql.TypeErrors  as TE
import           Data.ByteString          (ByteString)
import           Data.ByteString.Char8    (pack)
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

-- TODO(tech-debt): make the spec more strict by using a summation type
data ColumnSchema = ColumnSchema {name :: String, spec :: SpecSchema}
  deriving (Eq, Show, Generic)

data SpecSchema = Number | Bool | Text
  deriving (Eq, Show, Generic)


instance Y.FromJSON DbSchema

instance Y.FromJSON TableSchema

instance Y.FromJSON ColumnSchema

instance Y.FromJSON SpecSchema where
  parseJSON :: Y.Value -> Y.Parser SpecSchema
  parseJSON = Y.withText "UserType" $ \case {
      "Number"             -> return Number;
      "Bool"            -> return Bool;
      "Text" -> return Text;
      _                  -> fail "Provided `spec` value is not one of supported types: Number | Bool | Text";
  }

-- Application mapping of raw schemas
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Simple alias for a type checker's check error.
type YclLoaderError = TE.TEBaseError

class Translator a b where
  translate :: a -> b

-- | Translator from db schema to TypeAST elements.
--
-- Examples:
--
-- >>> translate (DbSchema [(TableSchema "foo" [ColumnSchema "bar" Number])]) :: [TAST.TAstSimpleRecordIndexPair]
-- [TAstSimpleRecordIndexKeyValue (TAstSimpleIndexKey "foo") (fromList [(TAstSimpleIndexKey "bar",TAstAtomicTypeNumber)])]
--
instance Translator DbSchema [TAST.TAstSimpleRecordIndexPair] where
  translate :: DbSchema -> [TAST.TAstSimpleRecordIndexPair]
  translate (DbSchema ts) = map translate ts

-- | Translator from table schema to TypeAST elements.
--
-- Examples:
--
-- >>> translate (TableSchema "foo" [ColumnSchema "bar" Number]) :: TAST.TAstSimpleRecordIndexPair
-- TAstSimpleRecordIndexKeyValue (TAstSimpleIndexKey "foo") (fromList [(TAstSimpleIndexKey "bar",TAstAtomicTypeNumber)])
--
instance Translator TableSchema TAST.TAstSimpleRecordIndexPair where
  translate :: TableSchema -> TAST.TAstSimpleRecordIndexPair
  translate (TableSchema n cs) = do
    let tcs = map translate cs
    TAST.TAstSimpleRecordIndexKeyValue (TAST.makeKey n) (TAST.makeRecord tcs)

-- | Translator from column schema to TypeAST elements.
--
-- Examples:
--
-- >>> translate (ColumnSchema "foo" Number) :: TAST.TAstSimpleAtomicIndexPair
-- TAstSimpleAtomicIndexKeyValue (TAstSimpleIndexKey "foo") TAstAtomicTypeNumber
--
instance Translator ColumnSchema TAST.TAstSimpleAtomicIndexPair where
  translate :: ColumnSchema -> TAST.TAstSimpleAtomicIndexPair
  translate (ColumnSchema n Number) = TAST.TAstSimpleAtomicIndexKeyValue (TAST.makeKey n) TAST.TAstAtomicTypeNumber
  translate (ColumnSchema n Text) = TAST.TAstSimpleAtomicIndexKeyValue (TAST.makeKey n) TAST.TAstAtomicTypeText
  translate (ColumnSchema n Bool) = TAST.TAstSimpleAtomicIndexKeyValue (TAST.makeKey n) TAST.TAstAtomicTypeBool

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

-- | Safely loads the provided YAML schema into a context from a ByteString
--
-- Examples:
--
-- >>> load (pack "tables: [{\"title\": \"Foo\", \"columns\": []}]" :: ByteString)
-- Right (fromList [(TCXSimpleTypeContextKey "Foo",TCXSimpleTypeContextValueRecord (fromList []))])
--
-- >>> load (pack "tables: [{\"title\": \"Foo\", \"columns\": [{\"name\": \"bar\", \"spec\": \"Number\"}]}]" :: ByteString)
-- Right (fromList [(TCXSimpleTypeContextKey "Foo",TCXSimpleTypeContextValueRecord (fromList [(TAstSimpleIndexKey "bar",TAstAtomicTypeNumber)]))])
--
-- >>> load (pack "foo: {}" :: ByteString)
-- Left (TEBaseError ["Error occurred when decoding the scheme: AesonException \"Error in $: parsing ChaiMicroSql.Loaders.YamlContextLoader.DbSchema(DbSchema) failed, key \\\"tables\\\" not found\""])
--
load :: ByteString -> Either YclLoaderError TCX.TCXSimpleTypeContext
load s = do
  schema <- wrappedDecode s
  return $ buildContext $ translate schema

-- | Safely loads the provided YAML schema into a context from a String
--
-- Examples:
--
-- >>> loads ("tables: [{\"title\": \"Foo\", \"columns\": []}]")
-- Right (fromList [(TCXSimpleTypeContextKey "Foo",TCXSimpleTypeContextValueRecord (fromList []))])
--
-- >>> loads ("tables: [{\"title\": \"Foo\", \"columns\": [{\"name\": \"bar\", \"spec\": \"Number\"}]}]")
-- Right (fromList [(TCXSimpleTypeContextKey "Foo",TCXSimpleTypeContextValueRecord (fromList [(TAstSimpleIndexKey "bar",TAstAtomicTypeNumber)]))])
--
-- >>> loads ("foo: {}")
-- Left (TEBaseError ["Error occurred when decoding the scheme: AesonException \"Error in $: parsing ChaiMicroSql.Loaders.YamlContextLoader.DbSchema(DbSchema) failed, key \\\"tables\\\" not found\""])
--
loads :: String -> Either YclLoaderError TCX.TCXSimpleTypeContext
loads = load . pack
