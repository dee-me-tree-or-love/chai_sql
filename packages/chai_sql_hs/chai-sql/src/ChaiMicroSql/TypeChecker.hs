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
        inferSelectList,
        inferFromTable,
        inferFromList,
        inferSelectQuery
    ) where

import qualified ChaiMicroSql.AST         as AST
import qualified ChaiMicroSql.CommonUtils as CU
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
    let k = TAST.TASTSimpleIndexKey $ CU.toString v
    Right $ TAST.TASTSimpleAtomicIndexKeyValue k at
inferAttributeReference c (AST.ASTSelectAttributeReferenceQualified b v) = do
    bt <- inferVar c b
    let k = TAST.TASTSimpleIndexKey $ CU.toString v
    let t = TAST.get k bt
    case t of
        Just t' -> Right $ TAST.TASTSimpleAtomicIndexKeyValue k t'
        Nothing -> Left $ __recordUnknownAttributeError b v

__recordUnknownAttributeError :: AST.ASTVariable -> AST.ASTVariable -> TCInferenceError
__recordUnknownAttributeError b v = TE.makeError $ "Record `" ++ bs ++ "` does not contain attribute `" ++ vs ++ "`. In expression `" ++ bs ++ "." ++ vs ++ "`, reconsider attribute access."
    where
        bs = CU.toString b
        vs = CU.toString v

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
    Right $ TAST.TASTSimpleAtomicIndexPair $ TAST.TASTSimpleAtomicIndexKeyValue (TAST.TASTSimpleIndexKey b) v

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

-- Table access
-- ............

-- | Table access inference.
--
-- - Note: Corresponds to the @Axiom A1@ and @Rule R2@
--
inferFromTable :: TCX.TCXSimpleTypeContext -> AST.ASTFromTable -> Either TCInferenceError TAST.TASTSimpleRecordIndexPair
inferFromTable c (AST.ASTFromTableReference v) = do
    vt <- inferVar c v
    let k = TAST.TASTSimpleIndexKey $ CU.toString v
    Right $ TAST.TASTSimpleRecordIndexKeyValue k vt
inferFromTable c (AST.ASTFromTableReferenceAlias v a) = do
    vt <- inferVar c v
    let k = TAST.TASTSimpleIndexKey $ CU.toString a
    Right $ TAST.TASTSimpleRecordIndexKeyValue k vt
inferFromTable _ (AST.ASTFromNestedQueryReferenceAlias _ _) = error "sub-queries are not supported yet"

__fromNotRecordError :: AST.ASTVariable -> TCInferenceError
__fromNotRecordError v = TE.makeError $ "From target is not a Record. In expression `" ++ CU.toString v ++ "`, reconsider the from clause access."


-- | Table list access inference.
--
-- Note: Corresponds to @Rule R3@
--
inferFromList :: TCX.TCXSimpleTypeContext -> AST.ASTFromList -> Either TCInferenceError [TAST.TASTSimpleRecordIndexPair]
inferFromList c as = do
    let ets = map (inferFromTable c) as
    case any isLeft ets of
        True  -> Left $ TE.combineErrors $ lefts ets
        False -> Right $ rights ets

-- Full SELECT query
-- .................

-- | Select query result type inference.
--
-- Note: corresponds to @Rule R4@
--
inferSelectQuery :: TCX.TCXSimpleTypeContext -> AST.ASTSelectQuery -> Either TCInferenceError TAST.TASTDbView
inferSelectQuery c (AST.ASTSelectQuery as fs) = do
    fts <- inferFromList c fs
    let fats = foldl __collectAttributes [] fts
    let fc = foldl __extendFromRecordPair c fts
    let fac = foldl __extendFromRecordPairAttributes fc fts
    ats <- inferSelectList fac as
    pure $ __resolveView fats ats

__resolveView :: [TAST.TASTSimpleAtomicIndexPair] -> [TAST.TASTSimpleAtomicIndex] -> TAST.TASTDbView
__resolveView fs = foldl (__resolveIndexesToView fs) []

__resolveIndexesToView :: [TAST.TASTSimpleAtomicIndexPair] -> TAST.TASTDbView -> TAST.TASTSimpleAtomicIndex -> TAST.TASTDbView
__resolveIndexesToView ps vs TAST.TASTSimpleTypeRecordTotal    = vs ++ ps
__resolveIndexesToView _ vs (TAST.TASTSimpleAtomicIndexPair p) = vs ++ [p]

__collectAttributes :: [TAST.TASTSimpleAtomicIndexPair] -> TAST.TASTSimpleRecordIndexPair -> [TAST.TASTSimpleAtomicIndexPair]
__collectAttributes xs (TAST.TASTSimpleRecordIndexKeyValue _ r) = xs ++ TAST.indexes r

__extendFromRecordPair :: TCX.TCXSimpleTypeContext -> TAST.TASTSimpleRecordIndexPair -> TCX.TCXSimpleTypeContext
__extendFromRecordPair c (TAST.TASTSimpleRecordIndexKeyValue k r) = __extend' c k r

__extendFromRecordPairAttributes :: TCX.TCXSimpleTypeContext -> TAST.TASTSimpleRecordIndexPair -> TCX.TCXSimpleTypeContext
__extendFromRecordPairAttributes c (TAST.TASTSimpleRecordIndexKeyValue _ r) = do
    let ras = TAST.pairs r
    foldl __extendFromAttributes c ras

__extendFromAttributes :: TCX.TCXSimpleTypeContext -> (TAST.TASTSimpleIndexKey, TAST.TASTAtomicType) -> TCX.TCXSimpleTypeContext
__extendFromAttributes c (k, a) = __extend' c k a

__extend' :: (CU.ToStringable p1, TCX.Contextable p2) => TCX.TCXSimpleTypeContext -> p1 -> p2 -> TCX.TCXSimpleTypeContext
__extend' c k r = do
    let k' = TCX.makeKey $ CU.toString k
    let r' = TCX.contextualize r
    TCX.extend k' r' c
