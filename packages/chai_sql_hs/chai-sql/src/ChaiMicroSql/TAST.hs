{-# LANGUAGE InstanceSigs #-}
module ChaiMicroSql.TAST ( module ChaiMicroSql.TAST ) where

import qualified ChaiMicroSql.CommonUtils as CU
import qualified Data.Map                 as M

-- Basic Type language AST
-- ~~~~~~~~~~~~~~~~~~~~~~~

-- -- | A collection of various types
-- type TAstSimpleTypeList = [TAstSimpleType]

-- | All supported basic types.
data TAstSimpleTypeBasic
    = TAstSimpleTypeBasicAtomic TAstAtomicType              -- ^ An atomic type @Bool | Number | Text@
    | TAstSimpleAtomicIndex TAstSimpleAtomicIndex     -- ^ An index tuple @key: type-value@
    deriving (Show, Eq)

-- | Basic atomic types: @Bool | Number | Text@
data TAstAtomicType
    = TAstAtomicTypeBool    -- ^ @Bool@
    | TAstAtomicTypeNumber  -- ^ @Number@
    | TAstAtomicTypeText    -- ^ @Text@
    deriving (Show, Eq, Ord)

-- | A type used to index type collections.
data TAstSimpleAtomicIndex
    = TAstSimpleTypeRecordTotal                                   -- ^ A special total record.
    | TAstSimpleAtomicIndexPair TAstSimpleAtomicIndexPair   -- ^ A key-value pari for an atomic type.
    deriving (Show, Eq, Ord)

-- | A type used to construct key-value index pairs for atomic type collections.
data TAstSimpleAtomicIndexPair = TAstSimpleAtomicIndexKeyValue TAstSimpleIndexKey TAstAtomicType   -- ^ A key-value pari for an atomic type.
    deriving (Show, Eq, Ord)

-- | A type key-value type record map.
type TAstSimpleTypeRecord = (M.Map TAstSimpleIndexKey TAstAtomicType)

-- | A default empty record.
emptyTypeRecord :: TAstSimpleTypeRecord
emptyTypeRecord = M.empty

-- | A type used to construct key-value index pairs for atomic type collections.
data TAstSimpleRecordIndexPair = TAstSimpleRecordIndexKeyValue TAstSimpleIndexKey TAstSimpleTypeRecord   -- ^ A key-value pari for a record type.
        deriving (Show, Eq, Ord)

-- | A specialized type construct representing DB query results.
--
-- Corresponds to a view type with possible duplicate keys.
type TAstDbView = [TAstSimpleAtomicIndexPair]

-- Common utilities
-- ----------------

-- | Simple wrapper for Record type indexing.
newtype TAstSimpleIndexKey = TAstSimpleIndexKey String deriving (Show, Eq, Ord)

-- | Simpler key maker
makeKey :: String -> TAstSimpleIndexKey
makeKey = TAstSimpleIndexKey

instance CU.ToStringable TAstSimpleIndexKey where
    toString :: TAstSimpleIndexKey -> String
    toString (TAstSimpleIndexKey s) = s

-- | Creates a new record
makeRecord :: [TAstSimpleAtomicIndexPair] -> TAstSimpleTypeRecord
makeRecord is = M.fromList $ map tuplify is

-- | Tuplify the TAstSimpleAtomicIndex.
tuplify :: TAstSimpleAtomicIndexPair -> (TAstSimpleIndexKey, TAstAtomicType)
tuplify (TAstSimpleAtomicIndexKeyValue k v) = (k,v)

-- | Type record retrieval
get :: TAstSimpleIndexKey -> TAstSimpleTypeRecord -> Maybe TAstAtomicType
get = M.lookup

-- | Indexes from the record elements
indexes :: TAstSimpleTypeRecord -> [TAstSimpleAtomicIndexPair]
indexes = map (uncurry TAstSimpleAtomicIndexKeyValue) . pairs

-- | Type record elements retrieval
pairs :: TAstSimpleTypeRecord -> [(TAstSimpleIndexKey, TAstAtomicType)]
pairs = M.assocs
