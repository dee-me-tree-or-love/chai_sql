{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{- | The Type Checker (Inference) for a Micro SQL fragment.

* Axioms

- @A1@: -- TODO(backlog): describe the axiom...
- @A2@: -- TODO(backlog): describe the axiom...

* Rules

- @R1@: -- TODO(backlog): describe the rule...
- @R2@: -- TODO(backlog): describe the rule...
- @R3@: -- TODO(backlog): describe the rule...
- @R4@: -- TODO(backlog): describe the rule...
-}
module ChaiMicroSql.TypeChecker (
        emptyError,
        joinErrors,
        combineErrors,
        inferVar,
        __varNotKnownError,
        inferTotalRecord,
        inferAttributeReference,
        __baseNotRecordError,
        __baseTotalRecordError,
        __recordUnknownAttributeError,
        inferAttribute,
        inferSelectList,
        inferFromTableReference,
        inferFromTable,
        inferFromList
    ) where

import qualified ChaiMicroSql.AST         as AST
import qualified ChaiMicroSql.TAST        as TAST
import qualified ChaiMicroSql.TypeContext as TCX
import           Data.Either              (isLeft, lefts, rights)

-- Type Inference Procedures
-- ~~~~~~~~~~~~~~~~~~~~~~~~~

-- Base utilities
-- ^^^^^^^^^^^^^^

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

-- | A utility to combine multiple errors into one.
--
combineErrors :: [TCInferenceError] -> TCInferenceError
combineErrors = foldl joinErrors emptyError

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

-- Attribute access
-- ................

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
inferSelectList :: TCX.TCSimpleTypeContext -> AST.ASTSelectList -> Either TCInferenceError TAST.TASTSimpleTypeList
inferSelectList c as = do
    let ets = map (inferAttribute c) as
    case any isLeft ets of
        True  -> Left $ combineErrors $ lefts ets
        False -> Right $ rights ets

-- Table access
-- ............

-- | Table reference inference.
--
-- - Note: Corresponds to the @Axiom A1@ and @Rule R4@
--
inferFromTableReference :: TCX.TCSimpleTypeContext -> AST.ASTFromTableReference -> Either TCInferenceError TAST.TASTSimpleTypeBasicIndex
inferFromTableReference c (AST.ASTFromTableReferenceTableName v)   = do
    vt <- inferVar c v
    let k = TAST.TASTSimpleTypeBasicIndexKey $ AST.toString v
    Right $ TAST.TASTSimpleTypeBasicIndexKeyValue k vt
inferFromTableReference c (AST.ASTFromTableReferenceNestedQuery q) = error "Sub-query is not yet supported!"  -- TODO(backlog!high): fix

-- | Table access inference.
--
-- - Note: Corresponds to the @Axiom A1@ and @Rule R2@
--
inferFromTable :: TCX.TCSimpleTypeContext -> AST.ASTFromTable -> Either TCInferenceError TAST.TASTSimpleType
inferFromTable c (AST.ASTFromTableReference v) = do
    vt <- inferFromTableReference c v
    Right $ TAST.TASTSimpleTypeBasic $ TAST.TASTSimpleTypeBasicIndex vt
inferFromTable c (AST.ASTFromTableReferenceAlias v a) = do
    let k = TAST.TASTSimpleTypeBasicIndexKey $ AST.toString a
    (TAST.TASTSimpleTypeBasicIndexKeyValue _ vt) <- inferFromTableReference c v
    Right $ TAST.TASTSimpleTypeBasic $ TAST.TASTSimpleTypeBasicIndex $ TAST.TASTSimpleTypeBasicIndexKeyValue k vt

-- | Table list access inference.
--
-- Note: Corresponds to @Rule R3@
--
inferFromList :: TCX.TCSimpleTypeContext -> AST.ASTFromList -> Either TCInferenceError TAST.TASTSimpleTypeList
inferFromList c as = do
    let ets = map (inferFromTable c) as
    case any isLeft ets of
        True  -> Left $ combineErrors $ lefts ets
        False -> Right $ rights ets

-- Full SELECT query
-- .................

-- TODO(backlog!high): implement
inferSelectQuery :: TCX.TCSimpleTypeContext -> AST.ASTSelectQuery -> Either TCInferenceError TAST.TASTSimpleTypeRecord
inferSelectQuery c (AST.ASTSelectQuery as fs) = do
    fts <- inferFromList c fs
    ats <- inferSelectList undefined as
    error "not implemented yet"
