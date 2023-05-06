module ChaiMicroSql.TAST ( module ChaiMicroSql.TAST ) where

import qualified Data.Map as M

-- Basic Type language AST
-- ~~~~~~~~~~~~~~~~~~~~~~~

-- TODO: deprecate?
-- -- | A basic simple type representation.
-- --
-- -- Supported features are:
-- --
-- -- - 3 atomic types: @TASTAtomicType@ -> @Bool, Number, Text@
-- -- - A record type without duplicate keys: e.g. @{key<String>: value<TASTAtomicType>}@
-- -- - A special total record: e.g. @TOT@
-- -- - A list of all above mentioned types
-- data TASTSimpleType
--     = TASTSimpleTypeBasic TASTSimpleTypeBasic   -- ^ 3 base types: @Bool, Number, Text@
--     | TASTSimpleTypeRecord TASTSimpleTypeRecord -- ^ A recursive record type: e.g. @[key<String>: value<TASTSimpleType>]@
--     | TASTSimpleTypeList  TASTSimpleTypeList    -- ^ A list of all possible simple types.
--     deriving (Show, Eq)

-- -- | A collection of various types
-- type TASTSimpleTypeList = [TASTSimpleType]

-- | All supported basic types.
data TASTSimpleTypeBasic
    = TASTSimpleTypeBasicAtomic TASTAtomicType              -- ^ An atomic type @Bool | Number | Text@
    | TASTSimpleAtomicIndex TASTSimpleAtomicIndex     -- ^ An index tuple @key<String>: value<TASTSimpleType>@
    deriving (Show, Eq)

-- | Basic atomic types: @Bool | Number | Text@
data TASTAtomicType
    = TASTAtomicTypeBool    -- ^ @Bool@
    | TASTAtomicTypeNumber  -- ^ @Number@
    | TASTAtomicTypeText    -- ^ @Text@
    deriving (Show, Eq)

-- | A type used to index type collections.
data TASTSimpleAtomicIndex
    = TASTSimpleTypeRecordTotal                                   -- ^ A special total record.
    | TASTSimpleAtomicIndexPair TASTSimpleAtomicIndexPair   -- ^ A key-value pari for an atomic type.
    deriving (Show, Eq)

-- | A type used to construct key-value index pairs for atomic type collections.
data TASTSimpleAtomicIndexPair = TASTSimpleAtomicIndexKeyValue TASTSimpleIndexKey TASTAtomicType   -- ^ A key-value pari for an atomic type.
    deriving (Show, Eq)

-- | A type key-value type record map.
type TASTSimpleTypeRecord = (M.Map TASTSimpleIndexKey TASTAtomicType)

-- | A default empty record.
emptyTypeRecord :: TASTSimpleTypeRecord
emptyTypeRecord = M.empty

-- | A type used to construct key-value index pairs for atomic type collections.
data TASTSimpleRecordIndexPair = TASTSimpleRecordIndexKeyValue TASTSimpleIndexKey TASTSimpleTypeRecord   -- ^ A key-value pari for a record type.
    deriving (Show, Eq)

-- | A specialized type construct representing DB query results.
--
-- - A view type with possible duplicate keys: e.g. @[key<String>: value<TASTAtomicType>]@
type TASTDbView = [TASTSimpleAtomicIndex]

-- Common utilities
-- ^^^^^^^^^^^^^^^^

-- | Simple wrapper for Record type indexing.
newtype TASTSimpleIndexKey = TASTSimpleIndexKey String deriving (Show, Eq, Ord)

-- | Creates a new record
makeRecord :: [TASTSimpleAtomicIndexPair] -> TASTSimpleTypeRecord
makeRecord is = M.fromList $ map tuplify is

-- | Tuplify the TASTSimpleAtomicIndex.
tuplify :: TASTSimpleAtomicIndexPair -> (TASTSimpleIndexKey, TASTAtomicType)
tuplify (TASTSimpleAtomicIndexKeyValue k v) = (k,v)

-- | Type record retrieval
get :: TASTSimpleIndexKey -> TASTSimpleTypeRecord -> Maybe TASTAtomicType
get = M.lookup
