module ChaiMicroSql.TAST ( module ChaiMicroSql.TAST ) where
import qualified Data.Map as M

-- Basic Type language AST
-- ~~~~~~~~~~~~~~~~~~~~~~~

-- | A basic simple type representation.
--
-- Supported features are:
--
-- - 3 atomic types: @TASTAtomicType@ -> @Bool, Number, Text@
-- - A record type without duplicate keys: e.g. @{key<String>: value<TASTAtomicType>}@
-- - A special total record: e.g. @TOT@
-- - A list of all above mentioned types
data TASTSimpleType
    = TASTSimpleTypeBasic TASTSimpleTypeBasic   -- ^ 3 base types: @Bool, Number, Text@
    | TASTSimpleTypeRecord TASTSimpleTypeRecord -- ^ A recursive record type: e.g. @[key<String>: value<TASTSimpleType>]@
    | TASTSimpleTypeList  TASTSimpleTypeList    -- ^ A list of all possible simple types.
    deriving (Show, Eq)

-- | A collection of various types
type TASTSimpleTypeList = [TASTSimpleType]

-- | All supported basic types.
data TASTSimpleTypeBasic
    = TASTSimpleTypeBasicAtomic TASTAtomicType              -- ^ An atomic type @Bool | Number | Text@
    | TASTSimpleTypeBasicIndex TASTSimpleTypeBasicIndex     -- ^ An index tuple @key<String>: value<TASTSimpleType>@
    deriving (Show, Eq)

-- | Basic atomic types: @Bool | Number | Text@
data TASTAtomicType
    = TASTAtomicTypeBool    -- ^ @Bool@
    | TASTAtomicTypeNumber  -- ^ @Number@
    | TASTAtomicTypeText    -- ^ @Text@
    deriving (Show, Eq)

-- | A type used to index type collections.
data TASTSimpleTypeBasicIndex
    = TASTSimpleTypeRecordTotal                                   -- ^ A special total record.
    | TASTSimpleTypeBasicIndexPair TASTSimpleTypeBasicIndexPair   -- ^ A key-value pari for an atomic type.
    deriving (Show, Eq)

-- | A type used to construct key-value index pairs for type collections.
data TASTSimpleTypeBasicIndexPair = TASTSimpleTypeBasicIndexKeyValue TASTSimpleTypeBasicIndexKey TASTAtomicType   -- ^ A key-value pari for an atomic type.
    deriving (Show, Eq)

-- | A type key-value type record map.
type TASTSimpleTypeRecord = (M.Map TASTSimpleTypeBasicIndexKey TASTAtomicType)

emptyTypeRecord :: TASTSimpleTypeRecord
emptyTypeRecord = M.empty

-- | A specialized type construct representing DB query results.
--
-- - A view type with possible duplicate keys: e.g. @[key<String>: value<TASTAtomicType>]@
type TASTDbView = [TASTSimpleTypeBasicIndex]

-- Common utilities
-- ^^^^^^^^^^^^^^^^

-- | Simple wrapper for Record type indexing.
newtype TASTSimpleTypeBasicIndexKey = TASTSimpleTypeBasicIndexKey String deriving (Show, Eq, Ord)

-- | Creates a new record
make :: [TASTSimpleTypeBasicIndexPair] -> TASTSimpleTypeRecord
make is = M.fromList $ map tuplify is

-- | Tuplify the TASTSimpleTypeBasicIndex.
tuplify :: TASTSimpleTypeBasicIndexPair -> (TASTSimpleTypeBasicIndexKey, TASTAtomicType)
tuplify (TASTSimpleTypeBasicIndexKeyValue k v) = (k,v)

-- | Type record retrieval
get :: TASTSimpleTypeBasicIndexKey -> TASTSimpleTypeRecord -> Maybe TASTAtomicType
get = M.lookup
