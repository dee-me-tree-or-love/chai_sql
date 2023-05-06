{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{- | The simple type context used for the type inference.

Also known as /`Gamma`/ in common literature.
 -}
module ChaiMicroSql.TypeContext (
        TCXSimpleTypeContext,
        TCXSimpleTypeContextKey(..),
        TCXSimpleTypeContextValue(..),
        freshContext,
        extend,
        get,
        TCXContextError,
        Contextable(..),
        __atomNotRecordError,
        __recordNotAtomError,
        makeKey
    ) where

import qualified ChaiMicroSql.TAST as TAST
import qualified ChaiMicroSql.TypeErrors as TE
import qualified Data.Map          as M


-- Typing context (aka. /Gamma/)
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | A key-value type context. (aka. /`Gamma`/)
-- TODO: handle context collisions
type TCXSimpleTypeContext = (M.Map TCXSimpleTypeContextKey TCXSimpleTypeContextValue)

-- | Simple wrapper for Context indexing.
newtype TCXSimpleTypeContextKey = TCXSimpleTypeContextKey String deriving (Show, Eq, Ord)

-- | Simple wrapper for Context contents.
data TCXSimpleTypeContextValue
    = TCXSimpleTypeContextValueAtomic TAST.TASTAtomicType        -- a recorded key points to an atomic type.
    | TCXSimpleTypeContextValueRecord TAST.TASTSimpleTypeRecord  -- a recorded key points to a record type.
    deriving (Show, Eq)

-- Context operations
-- ~~~~~~~~~~~~~~~~~~

-- | Creates a context key.
makeKey :: String -> TCXSimpleTypeContextKey
makeKey = TCXSimpleTypeContextKey

-- | Creates a fresh context
freshContext :: TCXSimpleTypeContext
freshContext = M.empty

-- | Extending the context with arbitrary type key-value pair.
--
-- Examples:
--
-- >>> let context' = freshContext
-- >>> "Original context: " ++ (show $ M.toList context')
-- "Original context: []"
-- >>> let key' = TCXSimpleTypeContextKey "foo"
-- >>> let value' = (TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool)
-- >>> let context'' = extend key' value' freshContext
-- >>> "Extended context: " ++ (show $ M.toList context'')
-- "Extended context: [(TCXSimpleTypeContextKey \"foo\",TASTSimpleTypeBasic TASTSimpleTypeBasicBool)]"
extend :: TCXSimpleTypeContextKey -> TCXSimpleTypeContextValue -> TCXSimpleTypeContext -> TCXSimpleTypeContext
extend = M.insert

-- | Find a key if present.
get :: TCXSimpleTypeContextKey -> TCXSimpleTypeContext -> Maybe TCXSimpleTypeContextValue
get = M.lookup

-- | A wrapper for context errors
type TCXContextError = TE.TEBaseError

-- | For shared context operations
class Contextable a where
    contextualize :: a -> TCXSimpleTypeContextValue
    decontextualize :: TCXSimpleTypeContextValue -> Either TCXContextError a

instance Contextable TAST.TASTAtomicType where
    contextualize ::  TAST.TASTAtomicType -> TCXSimpleTypeContextValue
    contextualize = TCXSimpleTypeContextValueAtomic
    decontextualize :: TCXSimpleTypeContextValue -> Either TCXContextError TAST.TASTAtomicType
    decontextualize (TCXSimpleTypeContextValueAtomic a) = Right a
    decontextualize (TCXSimpleTypeContextValueRecord _) = Left __recordNotAtomError

__recordNotAtomError :: TCXContextError
__recordNotAtomError = TE.makeError "Can not retrieve atomic type from stored record."

instance Contextable TAST.TASTSimpleTypeRecord where
    contextualize ::  TAST.TASTSimpleTypeRecord -> TCXSimpleTypeContextValue
    contextualize = TCXSimpleTypeContextValueRecord
    decontextualize :: TCXSimpleTypeContextValue -> Either TCXContextError TAST.TASTSimpleTypeRecord
    decontextualize (TCXSimpleTypeContextValueRecord r) = Right r
    decontextualize (TCXSimpleTypeContextValueAtomic _) = Left __atomNotRecordError

__atomNotRecordError :: TCXContextError
__atomNotRecordError = TE.makeError "Can not retrieve record type from stored atomic."
