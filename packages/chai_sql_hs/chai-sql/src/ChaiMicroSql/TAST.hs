module ChaiMicroSql.TAST ( module ChaiMicroSql.TAST ) where
import qualified Data.Map as M

-- Basic Type language AST
-- ~~~~~~~~~~~~~~~~~~~~~~~

-- | A basic simple type representation.
-- Supported features are:
-- - 3 base types: @Bool, Number, Text@
-- - A recursive record type: e.g. @[key<String>: value<TASTSimpleType>]@
-- - A special total record: e.g. @TOT@
-- - A list of all above mentioned types
--
data TASTSimpleType
    = TASTSimpleTypeBasic TASTSimpleTypeBasic   -- ^ 3 base types: @Bool, Number, Text@
    | TASTSimpleTypeRecord TASTSimpleTypeRecord -- ^ A recursive record type: e.g. @[key<String>: value<TASTSimpleType>]@
    | TASTSimpleTypeRecordTotal                 -- ^ A special total record.
    | TASTSimpleTypeList  TASTSimpleTypeList    -- ^ A list of all possible simple types.
    deriving (Show, Eq)

-- | A collection of various types
type TASTSimpleTypeList = [TASTSimpleType]

-- | All supported basic types.
data TASTSimpleTypeBasic
    = TASTSimpleTypeBasicBool                               -- ^ @Bool@
    | TASTSimpleTypeBasicNumber                             -- ^ @Number@
    | TASTSimpleTypeBasicText                               -- ^ @Text@
    | TASTSimpleTypeBasicIndex TASTSimpleTypeBasicIndex     -- ^ An index tuple @key<String>: value<TASTSimpleType>@
    deriving (Show, Eq)

-- | A type used to index type records.
data TASTSimpleTypeBasicIndex = TASTSimpleTypeBasicIndexKeyValue TASTSimpleTypeBasicIndexKey TASTSimpleType
    deriving (Show, Eq)

-- | A type key-value type record map.
type TASTSimpleTypeRecord = (M.Map TASTSimpleTypeBasicIndexKey TASTSimpleType)

-- Common utilities
-- ^^^^^^^^^^^^^^^^

-- | Simple wrapper for Record type indexing.
newtype TASTSimpleTypeBasicIndexKey = TASTSimpleTypeBasicIndexKey String deriving (Show, Eq, Ord)

-- | Creates a new record
make :: [TASTSimpleTypeBasicIndex] -> TASTSimpleTypeRecord
make is = M.fromList $ map __tup is

-- | Tuplify the TASTSimpleTypeBasicIndex.
__tup :: TASTSimpleTypeBasicIndex -> (TASTSimpleTypeBasicIndexKey, TASTSimpleType)
__tup (TASTSimpleTypeBasicIndexKeyValue k v) = (k,v)

-- | Type record retrieval
get :: TASTSimpleTypeBasicIndexKey -> TASTSimpleTypeRecord -> Maybe TASTSimpleType
get = M.lookup
