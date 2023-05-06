module ChaiMicroSql.TypeErrors (
        TEBaseError,
        makeError,
        emptyError,
        joinErrors,
        combineErrors
    ) where

-- | A basic error wrapper.
newtype TEBaseError = TEBaseError String deriving (Show, Eq)

-- | Creates a new error.
makeError :: String -> TEBaseError
makeError = TEBaseError

-- | Simple utility to create an error with an empty message.
emptyError :: TEBaseError
emptyError = makeError ""

-- | A utility to join two errors into one.
--
-- TODO(tech debt): change the error data structure to avoid this complexity.
--
-- Examples:
--
-- >>> joinErrors (TEBaseError "Ay, caramba") (TEBaseError "d'oh")
-- TEBaseError "Ay, caramba\n - d'oh"
--
-- >>> foldl joinErrors (TEBaseError "") [(TEBaseError "Ay, caramba"), (TEBaseError "d'oh"), (TEBaseError "Hrmmm....")]
-- TEBaseError "- Ay, caramba\n- d'oh\n- Hrmmm...."
--
joinErrors :: TEBaseError -> TEBaseError -> TEBaseError
joinErrors (TEBaseError "") (TEBaseError b) = TEBaseError $ "- " ++ b
joinErrors (TEBaseError a) (TEBaseError b) = TEBaseError $ a ++ "\n- " ++ b

-- | A utility to combine multiple errors into one.
--
combineErrors :: [TEBaseError] -> TEBaseError
combineErrors = foldl joinErrors emptyError
