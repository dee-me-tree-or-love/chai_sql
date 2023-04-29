module ChaiMicroSql.TAST ( module ChaiMicroSql.TAST ) where
import qualified Data.Map as M

-- Basic Type language AST
-- ~~~~~~~~~~~~~~~~~~~~~~~

-- | A basic simple type representation.
-- Supported features are:
-- - 3 base types: @Bool, Number, Text@
-- - A recursive record type: e.g. @[key<String>: value<TASTSimpleType>]@
data TASTSimpleType
    = TASTSimpleTypeBasic TASTSimpleTypeBasic   -- ^ 3 base types: @Bool, Number, Text@
    | TASTSimpleTypeRecord TASTSimpleTypeRecord -- ^ A recursive record type: e.g. @[key<String>: value<TASTSimpleType>]@
    deriving (Show, Eq)

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

-- | A type record.
data TASTSimpleTypeRecord
    = TASTSimpleTypeRecordTotal                         -- ^ A total record.
    | TASTSimpleTypeRecordMap TASTSimpleTypeRecordMap   -- ^ A key-value type map record.
    deriving (Show, Eq)

-- | A key-value type map.
type TASTSimpleTypeRecordMap = (M.Map TASTSimpleTypeBasicIndexKey TASTSimpleType)

-- Common utilities

-- | Simple wrapper for Record type indexing.
newtype TASTSimpleTypeBasicIndexKey = TASTSimpleTypeBasicIndexKey String deriving (Show, Eq, Ord)

