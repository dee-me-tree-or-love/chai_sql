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
        TCInferenceError,
        inferVar,
        __varNotKnownError,
        inferTotalRecord,
        inferAttributeReference,
        __recordUnknownAttributeError,
        inferAttribute,
        inferSelectList
    ) where

import qualified ChaiMicroSql.AST         as AST
import qualified ChaiMicroSql.TAST        as TAST
import qualified ChaiMicroSql.TypeContext as TCX
import qualified ChaiMicroSql.TypeErrors  as TE
import           Data.Either              (isLeft, lefts, rights)

-- Type Inference Procedures
-- ~~~~~~~~~~~~~~~~~~~~~~~~~

-- Base utilities
-- ^^^^^^^^^^^^^^

-- | Simple alias for containing a type checker error.
type TCInferenceError = TE.TEBaseError

-- Axioms
-- ^^^^^^

-- | Variable type inference.(*)
--
-- - Note: Corresponds to the @Axiom A1@.
--
inferVar :: TCX.Contextable a => TCX.TCXSimpleTypeContext -> AST.ASTVariable -> Either TCInferenceError a
inferVar c (AST.ASTVariable v) = do
    let i = TCX.get (TCX.makeKey v) c
    case i of
        Just t  -> TCX.decontextualize t
        Nothing -> Left $ __varNotKnownError v

-- | A utility to construct a variable inference error message.
__varNotKnownError :: String -> TCInferenceError
__varNotKnownError v = TE.makeError $ "Could not infer variable type. Variable `" ++ v ++ "` is not in context."

-- | Total record type inference.(*)
--
-- - Note: Corresponds to the @Axiom A2@.
--
inferTotalRecord :: TCX.TCXSimpleTypeContext -> AST.ASTSelectAttributeStarTotalRecord -> Either TCInferenceError TAST.TASTSimpleAtomicIndex
inferTotalRecord _ _ = Right TAST.TASTSimpleTypeRecordTotal

-- Rules
-- ^^^^^

-- Attribute access
-- ................

-- | Attribute reference inference.
--
-- - Note: Corresponds to the @Rule R1@
--
inferAttributeReference :: TCX.TCXSimpleTypeContext -> AST.ASTSelectAttributeReference -> Either TCInferenceError TAST.TASTSimpleAtomicIndexPair
inferAttributeReference c (AST.ASTSelectAttributeReferenceUnqualified v) = do
    at <- inferVar c v
    let k = TAST.TASTSimpleAtomicIndexKey $ AST.toString v
    Right $ TAST.TASTSimpleAtomicIndexKeyValue k at
inferAttributeReference c (AST.ASTSelectAttributeReferenceQualified b v) = do
    bt <- inferVar c b
    let k = TAST.TASTSimpleAtomicIndexKey $ AST.toString v
    let t = TAST.get k bt
    case t of
        Just t' -> Right $ TAST.TASTSimpleAtomicIndexKeyValue k t'
        Nothing -> Left $ __recordUnknownAttributeError b v

__recordUnknownAttributeError :: AST.ASTVariable -> AST.ASTVariable -> TCInferenceError
__recordUnknownAttributeError b v = TE.makeError $ "Record `" ++ bs ++ "` does not contain attribute `" ++ vs ++ "`. In expression `" ++ bs ++ "." ++ vs ++ "`, reconsider attribute access."
    where
        bs = AST.toString b
        vs = AST.toString v

-- | Single attribute access inference
--
-- Note: Corresponds to @Rule R2@
--
inferAttribute :: TCX.TCXSimpleTypeContext -> AST.ASTSelectAttribute -> Either TCInferenceError TAST.TASTSimpleAtomicIndex
inferAttribute c (AST.ASTSelectAttributeStar s)             = inferTotalRecord c s
inferAttribute c (AST.ASTSelectAttributeReference a)        = do
    at <- inferAttributeReference c a
    Right $ TAST.TASTSimpleAtomicIndexPair at
inferAttribute c (AST.ASTSelectAttributeReferenceAlias a (AST.ASTSimpleAlias b)) = do
    (TAST.TASTSimpleAtomicIndexKeyValue _ v) <- inferAttributeReference c a
    Right $ TAST.TASTSimpleAtomicIndexPair $ TAST.TASTSimpleAtomicIndexKeyValue (TAST.TASTSimpleAtomicIndexKey b) v

-- | Attribute list access inference
--
-- Note: Corresponds to @Rule R3@
--
inferSelectList :: TCX.TCXSimpleTypeContext -> AST.ASTSelectList -> Either TCInferenceError [TAST.TASTSimpleAtomicIndex]
inferSelectList c as = do
    let ets = map (inferAttribute c) as
    case any isLeft ets of
        True  -> Left $ TE.combineErrors $ lefts ets
        False -> Right $ rights ets

-- -- Table access
-- -- ............

-- -- | Table access inference.
-- --
-- -- - Note: Corresponds to the @Axiom A1@ and @Rule R2@
-- --
-- inferFromTable :: TCX.TCSimpleTypeContext -> AST.ASTFromTable -> Either TCInferenceError TAST.TASTSimpleAtomicIndex
-- inferFromTable c (AST.ASTFromTableReference v) = do
--     vt <- inferVar c v
--     let k = TAST.TASTSimpleAtomicIndexKey $ AST.toString v
--     Right $ TAST.TASTSimpleAtomicIndexKeyValue k vt
-- inferFromTable c (AST.ASTFromTableReferenceAlias v a) = do
--     vt <- inferVar c v
--     let k = TAST.TASTSimpleAtomicIndexKey $ AST.toString a
--     Right $ TAST.TASTSimpleAtomicIndexKeyValue k vt
-- inferFromTable _ (AST.ASTFromNestedQueryReferenceAlias _ _) = error "sub-queries are not supported yet"

-- __fromNotRecordError :: AST.ASTVariable -> TCInferenceError
-- __fromNotRecordError v = TCInferenceError $ "From target is not a Record. In expression `" ++ AST.toString v ++ "`, reconsider the from clause access."


-- -- | Table list access inference.
-- --
-- -- Note: Corresponds to @Rule R3@
-- --
-- inferFromList :: TCX.TCSimpleTypeContext -> AST.ASTFromList -> Either TCInferenceError [TAST.TASTSimpleAtomicIndex]
-- inferFromList c as = do
--     let ets = map (inferFromTable c) as
--     case any isLeft ets of
--         True  -> Left $ combineErrors $ lefts ets
--         False -> Right $ rights ets

-- -- Full SELECT query
-- -- .................

-- -- TODO(backlog!high): implement
-- inferSelectQuery :: TCX.TCSimpleTypeContext -> AST.ASTSelectQuery -> Either TCInferenceError TAST.TASTSimpleTypeRecord
-- inferSelectQuery c (AST.ASTSelectQuery as fs) = do
--     fts <- inferFromList c fs
--     -- TODO: make fresh context from the indexes in from.
--     -- TODO: select all Records from the list.
--     -- TODO: flatten the records, add each attribute to context, mark collisions
--     ats <- inferSelectList undefined as
--     -- TODO: construct a single record from all the indexes
--     error "not implemented yet"

