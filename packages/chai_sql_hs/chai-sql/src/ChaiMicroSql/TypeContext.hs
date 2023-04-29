{- | The simple type context used for the type inference.

Also known as /`Gamma`/ in common literature.
 -}
module ChaiMicroSql.TypeContext (
        TCSimpleTypeContext,
        TCSimpleTypeContextKey(..),
        freshContext,
        extend,
        get
    ) where

import qualified ChaiMicroSql.TAST as TAST
import qualified Data.Map          as M


-- Typing context (aka. /Gamma/)
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | A key-value type context. (aka. /`Gamma`/)
type TCSimpleTypeContext = (M.Map TCSimpleTypeContextKey TAST.TASTSimpleType)


-- | Simple wrapper for Record type indexing.
newtype TCSimpleTypeContextKey = TCSimpleTypeContextKey String deriving (Show, Eq, Ord)

-- Context operations
-- ~~~~~~~~~~~~~~~~~~

-- | Creates a fresh context
freshContext :: TCSimpleTypeContext
freshContext = M.empty

-- | Extending the context with arbitrary type key-value pair.
--
-- Examples:
--
-- >>> let context' = freshContext
-- >>> "Original context: " ++ (show $ M.toList context')
-- "Original context: []"
-- >>> let key' = TCSimpleTypeContextKey "foo"
-- >>> let value' = (TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool)
-- >>> let context'' = extend key' value' freshContext
-- >>> "Extended context: " ++ (show $ M.toList context'')
-- "Extended context: [(TCSimpleTypeContextKey \"foo\",TASTSimpleTypeBasic TASTSimpleTypeBasicBool)]"
extend :: TCSimpleTypeContextKey -> TAST.TASTSimpleType -> TCSimpleTypeContext -> TCSimpleTypeContext
extend = M.insert

-- | Find a key if present.
get :: TCSimpleTypeContextKey -> TCSimpleTypeContext -> Maybe TAST.TASTSimpleType
get = M.lookup
