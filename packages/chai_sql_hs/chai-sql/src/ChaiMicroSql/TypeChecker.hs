{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
{- | The Type Checker (Inference-based) for a Micro SQL fragment for ChaiSQL.

= ChaiSQL type checker

This is a naive inference-based type checker.

= Type expressions

    - @x@           - /some variable/ @x@
    - @x : T@       - /variable/ @x@ /has a type/ @T@
    - @{R}@         - /a record type with/ @R@ /elements/
    - @x ! {R}@     - /test if/ @x@ /is an element of/ @{R}@
    - @{R}(x)@      - /retrieve the value of/ @x@ /in/ @{R}@
    - @$x : T$@     - /a detached indexed type/ @T@ /associated with key/ @x@
    - @[L]@         - /a list type with/ @L@ /elements/
    - @[L] ++ [L']@ - /a concatentation of two list types/ @L@ /and/ @L'@
    - @[L] <\ [L']@ - /a resolution of types from/ @[L]@ /out of/ @[L']@
    - @|[L]|@       - /flatten the nested from/ @[L]@ /to a single level/

= Type inference

== Axioms

    - @A1@: ~ /Variable access/ ~     @G |- G(x) == T |= G |- x : T@
    - @A2@: ~ /Total column index/ ~  @G |- |= G |- "*" : TOT@

== Rules

    - @R1@: ~ /Qualifed attribute access/ ~   @G |- x : {R} /\ v ! {R} |= G |- x.v : $v : {R}(v) $@
    - @R2@: ~ /Alias index swap/ ~            @G |- x : $a : T$ |= G |- x as v : $v : T$@
    - @R3@: ~ /List of variables/ ~           @(G |- u : $a : T$ ) /\ (G |- us : [p]) |= G |- u, us : [p] ++ [$a : T$]@
    - @R4@: ~ /Resolve the attributes/ ~      @(G U ([ts] ++ |[ts]|) |- xs : [cs]) /\ (G |- ys : [ts]) |= 'SELECT' xs 'FROM' ys : [cs] <\ [ts]@

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
        inferSelectQuery,
        __attributeNotInSourceError
    ) where

import qualified ChaiMicroSql.AST         as AST
import qualified ChaiMicroSql.CommonUtils as CU
import qualified ChaiMicroSql.TAST        as TAST
import qualified ChaiMicroSql.TypeContext as TCX
import qualified ChaiMicroSql.TypeErrors  as TE
import           Data.Either              (isLeft, lefts, rights)

-- Type Inference Procedures
-- -------------------------

-- Base utilities
-- --------------

-- | Simple alias for containing a type checker error.
type TCInferenceError = TE.TEBaseError

class Inferrable a t where
    infer :: TCX.TCXSimpleTypeContext -> a -> Either TCInferenceError t

instance TCX.Contextable t => (Inferrable AST.AstVariable t) where
    infer :: TCX.TCXSimpleTypeContext -> AST.AstVariable -> Either TCInferenceError t
    infer = inferVar

instance (Inferrable AST.AstSelectAttributeStarTotalRecord TAST.TAstSimpleAtomicIndex) where
    infer :: TCX.TCXSimpleTypeContext -> AST.AstSelectAttributeStarTotalRecord -> Either TCInferenceError TAST.TAstSimpleAtomicIndex
    infer = inferTotalRecord

instance (Inferrable (AST.GAstSelectAttributeReference ()) TAST.TAstSimpleAtomicIndexPair) where
    infer :: TCX.TCXSimpleTypeContext -> AST.GAstSelectAttributeReference () -> Either TCInferenceError TAST.TAstSimpleAtomicIndexPair
    infer = inferAttributeReference

instance (Inferrable (AST.GAstSelectAttributeAccess () ()) TAST.TAstSimpleAtomicIndex) where
    infer :: TCX.TCXSimpleTypeContext -> AST.GAstSelectAttributeAccess () () -> Either TCInferenceError TAST.TAstSimpleAtomicIndex
    infer = inferAttribute

instance (Inferrable [AST.GAstSelectAttributeAccess () ()] [TAST.TAstSimpleAtomicIndex]) where
    infer :: TCX.TCXSimpleTypeContext -> [AST.GAstSelectAttributeAccess () ()] -> Either TCInferenceError [TAST.TAstSimpleAtomicIndex]
    infer = inferSelectList

instance (Inferrable (AST.GAstFromAccess () () () a) TAST.TAstSimpleRecordIndexPair) where
    infer :: TCX.TCXSimpleTypeContext -> AST.GAstFromAccess () () () a -> Either TCInferenceError TAST.TAstSimpleRecordIndexPair
    infer = inferFromTable

instance (Inferrable [AST.GAstFromAccess () () () a] [TAST.TAstSimpleRecordIndexPair]) where
    infer :: TCX.TCXSimpleTypeContext -> [AST.GAstFromAccess () () () a] -> Either TCInferenceError [TAST.TAstSimpleRecordIndexPair]
    infer = inferFromList

instance (Inferrable (AST.GAstSelectQuery () () () a) TAST.TAstDbView) where
    infer :: TCX.TCXSimpleTypeContext -> AST.GAstSelectQuery () () () a -> Either TCInferenceError TAST.TAstDbView
    infer = inferSelectQuery

-- Axioms
-- ------

-- | Variable type inference.
--
--      [Note]: Corresponds to the Axiom @A1@.
--
inferVar :: TCX.Contextable a => TCX.TCXSimpleTypeContext -> AST.AstVariable -> Either TCInferenceError a
inferVar c (AST.AstVariable v) = do
    let i = TCX.get (TCX.makeKey v) c
    case i of
        Just t  -> TCX.decontextualize t
        Nothing -> Left $ __varNotKnownError v

-- | A utility to construct a variable inference error message.
__varNotKnownError :: String -> TCInferenceError
__varNotKnownError v = TE.makeError $ "Could not infer variable type. Variable `" ++ v ++ "` is not in context."

-- | Total record type inference.
--
--      [Note]: Corresponds to the Axiom @A2@.
--
inferTotalRecord :: TCX.TCXSimpleTypeContext -> AST.AstSelectAttributeStarTotalRecord -> Either TCInferenceError TAST.TAstSimpleAtomicIndex
inferTotalRecord _ _ = Right TAST.TAstSimpleTypeRecordTotal

-- Rules
-- -----

-- Attribute access
-- ................

-- | Attribute reference inference.
--
--      [Note]: Corresponds to the Rule @R1@
--
inferAttributeReference :: TCX.TCXSimpleTypeContext -> AST.GAstSelectAttributeReference a -> Either TCInferenceError TAST.TAstSimpleAtomicIndexPair
inferAttributeReference c (AST.GAstSelectAttributeReferenceUnqualified _ v) = do
    at <- inferVar c v
    let k = TAST.makeKey $ CU.toString v
    Right $ TAST.TAstSimpleAtomicIndexKeyValue k at
inferAttributeReference c (AST.GAstSelectAttributeReferenceQualified _ b v) = do
    bt <- inferVar c b
    let k = TAST.makeKey $ CU.toString v
    let t = TAST.get k bt
    case t of
        Just t' -> Right $ TAST.TAstSimpleAtomicIndexKeyValue k t'
        Nothing -> Left $ __recordUnknownAttributeError b v

__recordUnknownAttributeError :: AST.AstVariable -> AST.AstVariable -> TCInferenceError
__recordUnknownAttributeError b v = TE.makeError $ "Record `" ++ bs ++ "` does not contain attribute `" ++ vs ++ "`. In expression `" ++ bs ++ "." ++ vs ++ "`, reconsider attribute access."
    where
        bs = CU.toString b
        vs = CU.toString v

-- | Single attribute access inference
--
--      [Note]: Corresponds to Rule @R2@
--
inferAttribute :: TCX.TCXSimpleTypeContext -> AST.GAstSelectAttributeAccess a b -> Either TCInferenceError TAST.TAstSimpleAtomicIndex
inferAttribute c (AST.GAstSelectAttributeAccessStar _ s)             = inferTotalRecord c s
inferAttribute c (AST.GAstSelectAttributeAccessReference _ a)        = do
    at <- inferAttributeReference c a
    Right $ TAST.TAstSimpleAtomicIndexPair at
inferAttribute c (AST.GAstSelectAttributeAccessReferenceAlias _ a (AST.AstSimpleAlias b)) = do
    (TAST.TAstSimpleAtomicIndexKeyValue _ v) <- inferAttributeReference c a
    Right $ TAST.TAstSimpleAtomicIndexPair $ TAST.TAstSimpleAtomicIndexKeyValue (TAST.makeKey b) v

-- | Attribute list access inference
--
--      [Note]: Corresponds to Rule @R3@
--
inferSelectList :: TCX.TCXSimpleTypeContext -> [AST.GAstSelectAttributeAccess a b] -> Either TCInferenceError [TAST.TAstSimpleAtomicIndex]
inferSelectList c as = do
    let ets = map (inferAttribute c) as
    case any isLeft ets of
        True  -> Left $ TE.combineErrors $ lefts ets
        False -> Right $ rights ets

-- Table access
-- ............

-- | Table access inference.
--
--      [Note]: Corresponds to the Axiom @A1@ and Rule @R2@
--
inferFromTable :: TCX.TCXSimpleTypeContext -> AST.GAstFromAccess a b c d -> Either TCInferenceError TAST.TAstSimpleRecordIndexPair
inferFromTable c (AST.GAstFromAccessReference _ v) = do
    vt <- inferVar c v
    let k = TAST.makeKey $ CU.toString v
    Right $ TAST.TAstSimpleRecordIndexKeyValue k vt
inferFromTable c (AST.GAstFromAccessReferenceAlias _ v a) = do
    vt <- inferVar c v
    let k = TAST.makeKey $ CU.toString a
    Right $ TAST.TAstSimpleRecordIndexKeyValue k vt
inferFromTable c (AST.GAstFromAccessNestedQueryAlias _ (AST.GAstSelectSubQuery q) a) = do
    -- infer the query result
    qt <- inferSelectQuery c q
    -- if query contains duplicate columns, resolve colisions
    let cqt = foldl __getCountLabels [] qt
    -- create a record form the resolved colisions
    let r = TAST.makeRecord $ map __dedup cqt
    -- return the record indexed by key
    let k = TAST.makeKey $ CU.toString a
    Right $ TAST.TAstSimpleRecordIndexKeyValue k r

__getCountLabels :: Eq a => [(a, Int)] -> a -> [(a, Int)]
__getCountLabels xs p = xs ++ [(p, c)]
    where c = length . filter ((== p) . fst) $ xs

__extendKey :: (CU.ToStringable a, Show b) => a -> b -> TAST.TAstSimpleIndexKey
__extendKey k n = TAST.makeKey $ CU.toString k ++ ":" ++ show n

__dedup :: (Eq b, Num b, Show b) => (TAST.TAstSimpleAtomicIndexPair, b) -> TAST.TAstSimpleAtomicIndexPair
__dedup (p@(TAST.TAstSimpleAtomicIndexKeyValue k v), n) = if n == 0 then p else TAST.TAstSimpleAtomicIndexKeyValue (__extendKey k n) v

-- | Table list access inference.
--
--      [Note]: Corresponds to @Rule R3@
--
inferFromList :: TCX.TCXSimpleTypeContext -> [AST.GAstFromAccess a b c d] -> Either TCInferenceError [TAST.TAstSimpleRecordIndexPair]
inferFromList c as = do
    let ets = map (inferFromTable c) as
    case any isLeft ets of
        True  -> Left $ TE.combineErrors $ lefts ets
        False -> Right $ rights ets

-- Full SELECT query
-- .................

-- | Select query result type inference.
--
--      [Note]: Corresponds to Rule @R4@
--
inferSelectQuery :: TCX.TCXSimpleTypeContext -> AST.GAstSelectQuery a b c d -> Either TCInferenceError TAST.TAstDbView
inferSelectQuery c (AST.GAstSelectQuery _ as fs) = do
    fts <- inferFromList c fs
    let fats = foldl __collectAttributes [] fts
    let fc = foldl __extendFromRecordPair c fts
    let fac = foldl __extendFromRecordPairAttributes fc fts
    ats <- inferSelectList fac as
    let rs = __resolveView fats ats
    case any isLeft rs of
        True  -> Left $ TE.combineErrors $ lefts rs
        False -> pure $ rights rs


__resolveView :: [TAST.TAstSimpleAtomicIndexPair] -> [TAST.TAstSimpleAtomicIndex] -> [Either TCInferenceError TAST.TAstSimpleAtomicIndexPair]
__resolveView fs = foldl (__resolveIndexesToView fs) []

__resolveIndexesToView :: [TAST.TAstSimpleAtomicIndexPair] -> [Either TCInferenceError TAST.TAstSimpleAtomicIndexPair] -> TAST.TAstSimpleAtomicIndex ->  [Either TCInferenceError TAST.TAstSimpleAtomicIndexPair]
__resolveIndexesToView ps vs TAST.TAstSimpleTypeRecordTotal    = vs ++ map pure ps
__resolveIndexesToView ps vs (TAST.TAstSimpleAtomicIndexPair p) = if p `elem` ps then vs ++ [Right p] else vs ++ [Left $ __attributeNotInSourceError p ]

__attributeNotInSourceError :: TAST.TAstSimpleAtomicIndexPair -> TCInferenceError
__attributeNotInSourceError (TAST.TAstSimpleAtomicIndexKeyValue k _ ) = TE.makeError $ "The requested attribute is not known in the provided source: `" ++ CU.toString k ++ "`"

__collectAttributes :: [TAST.TAstSimpleAtomicIndexPair] -> TAST.TAstSimpleRecordIndexPair -> [TAST.TAstSimpleAtomicIndexPair]
__collectAttributes xs (TAST.TAstSimpleRecordIndexKeyValue _ r) = xs ++ TAST.indexes r

__extendFromRecordPair :: TCX.TCXSimpleTypeContext -> TAST.TAstSimpleRecordIndexPair -> TCX.TCXSimpleTypeContext
__extendFromRecordPair c (TAST.TAstSimpleRecordIndexKeyValue k r) = __extend' c k r

__extendFromRecordPairAttributes :: TCX.TCXSimpleTypeContext -> TAST.TAstSimpleRecordIndexPair -> TCX.TCXSimpleTypeContext
__extendFromRecordPairAttributes c (TAST.TAstSimpleRecordIndexKeyValue _ r) = do
    let ras = TAST.pairs r
    foldl __extendFromAttributes c ras

__extendFromAttributes :: TCX.TCXSimpleTypeContext -> (TAST.TAstSimpleIndexKey, TAST.TAstAtomicType) -> TCX.TCXSimpleTypeContext
__extendFromAttributes c (k, a) = __extend' c k a

__extend' :: (CU.ToStringable a, TCX.Contextable b) => TCX.TCXSimpleTypeContext -> a -> b -> TCX.TCXSimpleTypeContext
__extend' c k r = do
    let k' = TCX.makeKey $ CU.toString k
    let r' = TCX.contextualize r
    TCX.extend k' r' c
