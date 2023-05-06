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
        -- inferAttributeReference,
        -- __baseNotRecordError,
        -- __baseTotalRecordError,
        -- __recordUnknownAttributeError,
        -- inferAttribute,
        -- inferSelectList,
        -- inferFromTable,
        -- inferFromList
    ) where

import qualified ChaiMicroSql.AST         as AST
import qualified ChaiMicroSql.TAST        as TAST
import qualified ChaiMicroSql.TypeContext as TCX
import qualified ChaiMicroSql.TypeErrors    as TE
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
inferTotalRecord :: TCX.TCXSimpleTypeContext -> AST.ASTSelectAttributeStarTotalRecord -> Either TCInferenceError TAST.TASTSimpleTypeBasicIndex
inferTotalRecord _ _ = Right TAST.TASTSimpleTypeRecordTotal

-- Rules
-- ^^^^^

-- -- Attribute access
-- -- ................

-- -- | Attribute reference inference.
-- --
-- -- - Note: Corresponds to the @Rule R1@
-- --
-- inferAttributeReference :: TCX.TCSimpleTypeContext -> AST.ASTSelectAttributeReference -> Either TCInferenceError TAST.TASTSimpleTypeBasicIndex
-- inferAttributeReference c (AST.ASTSelectAttributeReferenceUnqualified v) = do
--     at <- inferVar c v
--     let k = TAST.TASTSimpleTypeBasicIndexKey $ AST.toString v
--     Right $ TAST.TASTSimpleTypeBasicIndexKeyValue k at
-- inferAttributeReference c (AST.ASTSelectAttributeReferenceQualified b v) = do
--     bt <- inferVar c b
--     case bt of
--         TAST.TASTSimpleTypeList ts      -> Left $ __baseNotRecordError b v ts
--         TAST.TASTSimpleTypeBasic t     -> Left $ __baseNotRecordError b v t
--         TAST.TASTSimpleTypeRecordTotal -> Left $ __baseTotalRecordError b v
--         TAST.TASTSimpleTypeRecord r    -> do
--             let k = TAST.TASTSimpleTypeBasicIndexKey $ AST.toString v
--             let t = TAST.get k r
--             case t of
--                 Just t' -> Right $ TAST.TASTSimpleTypeBasicIndexKeyValue k t'
--                 Nothing -> Left $ __recordUnknownAttributeError b v

-- __baseNotRecordError :: Show t => AST.ASTVariable -> AST.ASTVariable -> t -> TCInferenceError
-- __baseNotRecordError b v t = TCInferenceError $ "Could not infer the type of access `" ++ AST.toString b ++ "." ++ AST.toString v ++ "`. Variable `" ++ AST.toString b ++ "` is a `" ++ show t ++ "` type and is not a Record."

-- __baseTotalRecordError :: AST.ASTVariable -> AST.ASTVariable -> TCInferenceError
-- __baseTotalRecordError b v = TCInferenceError $ "Total records do not support attribute access. In expression `" ++ AST.toString b ++ "." ++ AST.toString v ++ "`, reconsider attribute access."

-- __recordUnknownAttributeError :: AST.ASTVariable -> AST.ASTVariable -> TCInferenceError
-- __recordUnknownAttributeError b v = TCInferenceError $ "Record `" ++ bs ++ "` does not contain attribute `" ++ vs ++ "`. In expression `" ++ bs ++ "." ++ vs ++ "`, reconsider attribute access."
--     where
--         bs = AST.toString b
--         vs = AST.toString v

-- -- | Single attribute access inference
-- --
-- -- Note: Corresponds to @Rule R2@
-- --
-- inferAttribute :: TCX.TCSimpleTypeContext -> AST.ASTSelectAttribute -> Either TCInferenceError TAST.TASTSimpleType
-- inferAttribute c (AST.ASTSelectAttributeStar s)             = inferTotalRecord c s
-- inferAttribute c (AST.ASTSelectAttributeReference a)        = do
--     at <- inferAttributeReference c a
--     Right $ TAST.TASTSimpleTypeBasic $ TAST.TASTSimpleTypeBasicIndex at
-- inferAttribute c (AST.ASTSelectAttributeReferenceAlias a (AST.ASTSimpleAlias b)) = do
--     (TAST.TASTSimpleTypeBasicIndexKeyValue _ v) <- inferAttributeReference c a
--     Right $ TAST.TASTSimpleTypeBasic $ TAST.TASTSimpleTypeBasicIndex $ TAST.TASTSimpleTypeBasicIndexKeyValue (TAST.TASTSimpleTypeBasicIndexKey b) v

-- -- | Attribute list access inference
-- --
-- -- Note: Corresponds to @Rule R3@
-- --
-- inferSelectList :: TCX.TCSimpleTypeContext -> AST.ASTSelectList -> Either TCInferenceError TAST.TASTSimpleTypeList
-- inferSelectList c as = do
--     let ets = map (inferAttribute c) as
--     case any isLeft ets of
--         True  -> Left $ combineErrors $ lefts ets
--         False -> Right $ rights ets

-- -- Table access
-- -- ............

-- -- | Table access inference.
-- --
-- -- - Note: Corresponds to the @Axiom A1@ and @Rule R2@
-- --
-- inferFromTable :: TCX.TCSimpleTypeContext -> AST.ASTFromTable -> Either TCInferenceError TAST.TASTSimpleTypeBasicIndex
-- inferFromTable c (AST.ASTFromTableReference v) = do
--     vt <- inferVar c v
--     let k = TAST.TASTSimpleTypeBasicIndexKey $ AST.toString v
--     Right $ TAST.TASTSimpleTypeBasicIndexKeyValue k vt
-- inferFromTable c (AST.ASTFromTableReferenceAlias v a) = do
--     vt <- inferVar c v
--     let k = TAST.TASTSimpleTypeBasicIndexKey $ AST.toString a
--     Right $ TAST.TASTSimpleTypeBasicIndexKeyValue k vt
-- inferFromTable _ (AST.ASTFromNestedQueryReferenceAlias _ _) = error "sub-queries are not supported yet"

-- __fromNotRecordError :: AST.ASTVariable -> TCInferenceError
-- __fromNotRecordError v = TCInferenceError $ "From target is not a Record. In expression `" ++ AST.toString v ++ "`, reconsider the from clause access."


-- -- | Table list access inference.
-- --
-- -- Note: Corresponds to @Rule R3@
-- --
-- inferFromList :: TCX.TCSimpleTypeContext -> AST.ASTFromList -> Either TCInferenceError [TAST.TASTSimpleTypeBasicIndex]
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

