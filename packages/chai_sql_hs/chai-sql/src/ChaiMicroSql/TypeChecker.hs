{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{- | The Type Checker (Inference) for a Micro SQL fragment.

* Axioms

- @A1@: -- TODO: ...
- @A2@: -- TODO: ...

* Rules

- @R1@: -- TODO: ...
- @R2@: -- TODO: ...
- @R3@: -- TODO: ...
- @R4@: -- TODO: ...
-}
module ChaiMicroSql.TypeChecker (
        emptyError,
        joinErrors,
        inferVar,
        __varNotKnownError,
        inferTotalRecord,
        inferAttributeReference,
        __baseNotRecordError,
        __baseTotalRecordError,
        __recordUnknownAttributeError,
        inferAttribute,
        inferSelectList
    ) where

import qualified ChaiMicroSql.AST         as AST
import qualified ChaiMicroSql.TAST        as TAST
import qualified ChaiMicroSql.TypeContext as TCX
import           Data.Either              (isLeft, lefts, rights)

-- Type Inference Procedures
-- ~~~~~~~~~~~~~~~~~~~~~~~~~

-- TODO(tech-debt): make it contain a list of error messages.
newtype TCInferenceError = TCInferenceError String deriving (Show, Eq)

-- | Simple utility to create an error with an empty message.
emptyError :: TCInferenceError
emptyError = TCInferenceError ""

-- | A utility to join two errors into one.
-- TODO(tech debt): change the error data structure to avoid this complexity.
--
-- Examples:
--
-- >>> joinErrors (TCInferenceError "Ay, caramba") (TCInferenceError "d'oh")
-- TCInferenceError "Ay, caramba \n - d'oh"
--
-- >>> foldl joinErrors (TCInferenceError "") [(TCInferenceError "Ay, caramba"), (TCInferenceError "d'oh"), (TCInferenceError "Hrmmm....")]
-- TCInferenceError "- Ay, caramba \n - d'oh \n - Hrmmm...."
--
joinErrors :: TCInferenceError -> TCInferenceError -> TCInferenceError
joinErrors (TCInferenceError "") (TCInferenceError b) = TCInferenceError $ "- " ++ b
joinErrors (TCInferenceError a) (TCInferenceError b) = TCInferenceError $ a ++ " \n - " ++ b

-- Axioms
-- ^^^^^^

-- | Variable type inference.(*)
--
-- - Note: Corresponds to the @Axiom A1@.
--
-- Examples
--
-- >>> inferVar (TCX.freshContext) (AST.ASTVariable "foo")
-- Left (TCInferenceError "Could not infer variable type. Variable `foo` is not in context.")
-- >>> let k = TCX.TCSimpleTypeContextKey "foo"
-- >>> let v = TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool
-- >>> inferVar (TCX.extend k v TCX.freshContext) (AST.ASTVariable "foo")
-- Right (TASTSimpleTypeBasic TASTSimpleTypeBasicBool)
--
inferVar :: TCX.TCSimpleTypeContext -> AST.ASTVariable -> Either TCInferenceError TAST.TASTSimpleType
inferVar c (AST.ASTVariable v) = do
    let i = TCX.get (TCX.TCSimpleTypeContextKey v) c
    case i of
        Just t  -> Right t
        Nothing -> Left $ __varNotKnownError v

-- | A utility to construct a variable inference error message.
__varNotKnownError :: String -> TCInferenceError
__varNotKnownError v = TCInferenceError $ "Could not infer variable type. Variable `" ++ v ++ "` is not in context."


-- | Total record type inference.(*)
--
-- - Note: Corresponds to the @Axiom A2@.
--
inferTotalRecord :: TCX.TCSimpleTypeContext -> AST.ASTSelectAttributeStarTotalRecord -> Either TCInferenceError TAST.TASTSimpleType
inferTotalRecord _ _ = Right TAST.TASTSimpleTypeRecordTotal


-- Rules
-- ^^^^^

-- | Attribute reference inference.
--
-- - Note: Corresponds to the @Rule R1@
--
inferAttributeReference :: TCX.TCSimpleTypeContext -> AST.ASTSelectAttributeReference -> Either TCInferenceError TAST.TASTSimpleTypeBasicIndex
inferAttributeReference c (AST.ASTSelectAttributeReferenceUnqualified v) = do
    at <- inferVar c v
    let k = TAST.TASTSimpleTypeBasicIndexKey $ AST.toString v
    Right $ TAST.TASTSimpleTypeBasicIndexKeyValue k at
inferAttributeReference c (AST.ASTSelectAttributeReferenceQualified b v) = do
    bt <- inferVar c b
    case bt of
        TAST.TASTSimpleTypeList ts      -> Left $ __baseNotRecordError b v ts
        TAST.TASTSimpleTypeBasic t     -> Left $ __baseNotRecordError b v t
        TAST.TASTSimpleTypeRecordTotal -> Left $ __baseTotalRecordError b v
        TAST.TASTSimpleTypeRecord r    -> do
            let k = TAST.TASTSimpleTypeBasicIndexKey $ AST.toString v
            let t = TAST.get k r
            case t of
                Just t' -> Right $ TAST.TASTSimpleTypeBasicIndexKeyValue k t'
                Nothing -> Left $ __recordUnknownAttributeError b v

__baseNotRecordError :: Show t => AST.ASTVariable -> AST.ASTVariable -> t -> TCInferenceError
__baseNotRecordError b v t = TCInferenceError $ "Could not infer the type of access `" ++ AST.toString b ++ "." ++ AST.toString v ++ "`. Variable `" ++ AST.toString b ++ "` is a `" ++ show t ++ "` type and is not a Record."

__baseTotalRecordError :: AST.ASTVariable -> AST.ASTVariable -> TCInferenceError
__baseTotalRecordError b v = TCInferenceError $ "Total records do not support attribute access. In expression `" ++ AST.toString b ++ "." ++ AST.toString v ++ "`, reconsider attribute access."

__recordUnknownAttributeError :: AST.ASTVariable -> AST.ASTVariable -> TCInferenceError
__recordUnknownAttributeError b v = TCInferenceError $ "Record `" ++ bs ++ "` does not contain attribute `" ++ vs ++ "`. In expression `" ++ bs ++ "." ++ vs ++ "`, reconsider attribute access."
    where
        bs = AST.toString b
        vs = AST.toString v

-- | Single attribute access inference
--
-- Note: Corresponds to @Rule R2@
--
inferAttribute :: TCX.TCSimpleTypeContext -> AST.ASTSelectAttribute -> Either TCInferenceError TAST.TASTSimpleType
inferAttribute c (AST.ASTSelectAttributeStar s)             = inferTotalRecord c s
inferAttribute c (AST.ASTSelectAttributeReference a)        = do
    at <- inferAttributeReference c a
    Right $ TAST.TASTSimpleTypeBasic $ TAST.TASTSimpleTypeBasicIndex at
inferAttribute c (AST.ASTSelectAttributeReferenceAlias a (AST.ASTSimpleAlias b)) = do
    (TAST.TASTSimpleTypeBasicIndexKeyValue _ v) <- inferAttributeReference c a
    Right $ TAST.TASTSimpleTypeBasic $ TAST.TASTSimpleTypeBasicIndex $ TAST.TASTSimpleTypeBasicIndexKeyValue (TAST.TASTSimpleTypeBasicIndexKey b) v

-- | Attribute list access inference
--
-- Note: Corresponds to @Rule R3@
--
inferSelectList :: TCX.TCSimpleTypeContext -> AST.ASTSelectList -> Either TCInferenceError TAST.TASTSimpleType
inferSelectList c as = do
    let ets = map (inferAttribute c) as
    case any isLeft ets of
        True  -> Left $ foldl joinErrors emptyError $ lefts ets
        False -> Right $ TAST.TASTSimpleTypeList $ rights ets
