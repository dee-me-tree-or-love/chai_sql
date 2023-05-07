{-# LANGUAGE InstanceSigs #-}
module ChaiMicroSql.TAST ( module ChaiMicroSql.TAST ) where

import qualified ChaiMicroSql.CommonUtils as CU
import qualified Data.Map                 as M

-- Basic Type language AST
-- ~~~~~~~~~~~~~~~~~~~~~~~

-- -- | A collection of various types
-- type TASTSimpleTypeList = [TASTSimpleType]

-- | All supported basic types.
data TASTSimpleTypeBasic
    = TASTSimpleTypeBasicAtomic TASTAtomicType              -- ^ An atomic type @Bool | Number | Text@
    | TASTSimpleAtomicIndex TASTSimpleAtomicIndex     -- ^ An index tuple @key: type-value@
    deriving (Show, Eq)

-- | Basic atomic types: @Bool | Number | Text@
data TASTAtomicType
    = TASTAtomicTypeBool    -- ^ @Bool@
    | TASTAtomicTypeNumber  -- ^ @Number@
    | TASTAtomicTypeText    -- ^ @Text@
    deriving (Show, Eq, Ord)

-- | A type used to index type collections.
data TASTSimpleAtomicIndex
    = TASTSimpleTypeRecordTotal                                   -- ^ A special total record.
    | TASTSimpleAtomicIndexPair TASTSimpleAtomicIndexPair   -- ^ A key-value pari for an atomic type.
    deriving (Show, Eq, Ord)

-- | A type used to construct key-value index pairs for atomic type collections.
data TASTSimpleAtomicIndexPair = TASTSimpleAtomicIndexKeyValue TASTSimpleIndexKey TASTAtomicType   -- ^ A key-value pari for an atomic type.
    deriving (Show, Eq, Ord)

-- | A type key-value type record map.
type TASTSimpleTypeRecord = (M.Map TASTSimpleIndexKey TASTAtomicType)

-- | A default empty record.
emptyTypeRecord :: TASTSimpleTypeRecord
emptyTypeRecord = M.empty

-- | A type used to construct key-value index pairs for atomic type collections.
data TASTSimpleRecordIndexPair = TASTSimpleRecordIndexKeyValue TASTSimpleIndexKey TASTSimpleTypeRecord   -- ^ A key-value pari for a record type.
        deriving (Show, Eq, Ord)

-- | A specialized type construct representing DB query results.
--
-- Corresponds to a view type with possible duplicate keys.
type TASTDbView = [TASTSimpleAtomicIndexPair]

-- Common utilities
-- ----------------

-- | Simple wrapper for Record type indexing.
newtype TASTSimpleIndexKey = TASTSimpleIndexKey String deriving (Show, Eq, Ord)

-- | Simpler key maker
makeKey :: String -> TASTSimpleIndexKey
makeKey = TASTSimpleIndexKey

instance CU.ToStringable TASTSimpleIndexKey where
    toString :: TASTSimpleIndexKey -> String
    toString (TASTSimpleIndexKey s) = s

-- | Creates a new record
makeRecord :: [TASTSimpleAtomicIndexPair] -> TASTSimpleTypeRecord
makeRecord is = M.fromList $ map tuplify is

-- | Tuplify the TASTSimpleAtomicIndex.
tuplify :: TASTSimpleAtomicIndexPair -> (TASTSimpleIndexKey, TASTAtomicType)
tuplify (TASTSimpleAtomicIndexKeyValue k v) = (k,v)

-- | Type record retrieval
get :: TASTSimpleIndexKey -> TASTSimpleTypeRecord -> Maybe TASTAtomicType
get = M.lookup

-- | Indexes from the record elements
indexes :: TASTSimpleTypeRecord -> [TASTSimpleAtomicIndexPair]
indexes = map (uncurry TASTSimpleAtomicIndexKeyValue) . pairs

-- | Type record elements retrieval
pairs :: TASTSimpleTypeRecord -> [(TASTSimpleIndexKey, TASTAtomicType)]
pairs = M.assocs
