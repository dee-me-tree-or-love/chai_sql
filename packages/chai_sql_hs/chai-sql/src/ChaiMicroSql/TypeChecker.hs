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

-- Axioms
-- ------

-- | Variable type inference.
--
--      [Note]: Corresponds to the Axiom @A1@.
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

-- | Total record type inference.
--
--      [Note]: Corresponds to the Axiom @A2@.
--
inferTotalRecord :: TCX.TCXSimpleTypeContext -> AST.ASTSelectAttributeStarTotalRecord -> Either TCInferenceError TAST.TASTSimpleAtomicIndex
inferTotalRecord _ _ = Right TAST.TASTSimpleTypeRecordTotal

-- Rules
-- -----

-- Attribute access
-- ................

-- | Attribute reference inference.
--
--      [Note]: Corresponds to the Rule @R1@
--
inferAttributeReference :: TCX.TCXSimpleTypeContext -> AST.ASTSelectAttributeReference -> Either TCInferenceError TAST.TASTSimpleAtomicIndexPair
inferAttributeReference c (AST.GASTSelectAttributeReferenceTypedUnqualified v _) = do
    at <- inferVar c v
    let k = TAST.makeKey $ CU.toString v
    Right $ TAST.TASTSimpleAtomicIndexKeyValue k at
inferAttributeReference c (AST.GASTSelectAttributeReferenceTypedQualified b v _) = do
    bt <- inferVar c b
    let k = TAST.makeKey $ CU.toString v
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
--      [Note]: Corresponds to Rule @R2@
--
inferAttribute :: TCX.TCXSimpleTypeContext -> AST.ASTSelectAttribute -> Either TCInferenceError TAST.TASTSimpleAtomicIndex
inferAttribute c (AST.GASTSelectAttributeTypedStar s _)             = inferTotalRecord c s
inferAttribute c (AST.GASTSelectAttributeTypedReference a _)        = do
    at <- inferAttributeReference c a
    Right $ TAST.TASTSimpleAtomicIndexPair at
inferAttribute c (AST.GASTSelectAttributeTypedReferenceAlias a (AST.ASTSimpleAlias b) _) = do
    (TAST.TASTSimpleAtomicIndexKeyValue _ v) <- inferAttributeReference c a
    Right $ TAST.TASTSimpleAtomicIndexPair $ TAST.TASTSimpleAtomicIndexKeyValue (TAST.makeKey b) v

-- | Attribute list access inference
--
--      [Note]: Corresponds to Rule @R3@
--
inferSelectList :: TCX.TCXSimpleTypeContext -> [AST.ASTSelectAttribute] -> Either TCInferenceError [TAST.TASTSimpleAtomicIndex]
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
inferFromTable :: TCX.TCXSimpleTypeContext -> AST.ASTFromTable -> Either TCInferenceError TAST.TASTSimpleRecordIndexPair
inferFromTable c (AST.GASTFromTableTypedReference v _) = do
    vt <- inferVar c v
    let k = TAST.makeKey $ CU.toString v
    Right $ TAST.TASTSimpleRecordIndexKeyValue k vt
inferFromTable c (AST.GASTFromTableTypedReferenceAlias v a _) = do
    vt <- inferVar c v
    let k = TAST.makeKey $ CU.toString a
    Right $ TAST.TASTSimpleRecordIndexKeyValue k vt
inferFromTable c (AST.GASTFromNestedQueryTypedReferenceAlias (AST.ASTSelectSubQuery q) a _) = do
    -- infer the query result
    qt <- inferSelectQuery c q
    -- if query contains duplicate columns, resolve colisions
    let cqt = foldl __getCountLabels [] qt
    -- create a record form the resolved colisions
    let r = TAST.makeRecord $ map __dedup cqt
    -- return the record indexed by key
    let k = TAST.makeKey $ CU.toString a
    Right $ TAST.TASTSimpleRecordIndexKeyValue k r

__getCountLabels :: Eq a => [(a, Int)] -> a -> [(a, Int)]
__getCountLabels xs p = xs ++ [(p, c)]
    where c = length . filter ((== p) . fst) $ xs

__extendKey :: (CU.ToStringable a, Show b) => a -> b -> TAST.TASTSimpleIndexKey
__extendKey k n = TAST.makeKey $ CU.toString k ++ ":" ++ show n

__dedup :: (Eq b, Num b, Show b) => (TAST.TASTSimpleAtomicIndexPair, b) -> TAST.TASTSimpleAtomicIndexPair
__dedup (p@(TAST.TASTSimpleAtomicIndexKeyValue k v), n) = if n == 0 then p else TAST.TASTSimpleAtomicIndexKeyValue (__extendKey k n) v

-- | Table list access inference.
--
--      [Note]: Corresponds to @Rule R3@
--
inferFromList :: TCX.TCXSimpleTypeContext -> [AST.ASTFromTable] -> Either TCInferenceError [TAST.TASTSimpleRecordIndexPair]
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
inferSelectQuery :: TCX.TCXSimpleTypeContext -> AST.ASTSelectQuery -> Either TCInferenceError TAST.TASTDbView
inferSelectQuery c (AST.GASTSelectQueryTyped as fs _) = do
    fts <- inferFromList c fs
    let fats = foldl __collectAttributes [] fts
    let fc = foldl __extendFromRecordPair c fts
    let fac = foldl __extendFromRecordPairAttributes fc fts
    ats <- inferSelectList fac as
    let rs = __resolveView fats ats
    case any isLeft rs of
        True  -> Left $ TE.combineErrors $ lefts rs
        False -> pure $ rights rs


__resolveView :: [TAST.TASTSimpleAtomicIndexPair] -> [TAST.TASTSimpleAtomicIndex] -> [Either TCInferenceError TAST.TASTSimpleAtomicIndexPair]
__resolveView fs = foldl (__resolveIndexesToView fs) []

__resolveIndexesToView :: [TAST.TASTSimpleAtomicIndexPair] -> [Either TCInferenceError TAST.TASTSimpleAtomicIndexPair] -> TAST.TASTSimpleAtomicIndex ->  [Either TCInferenceError TAST.TASTSimpleAtomicIndexPair]
__resolveIndexesToView ps vs TAST.TASTSimpleTypeRecordTotal    = vs ++ map pure ps
__resolveIndexesToView ps vs (TAST.TASTSimpleAtomicIndexPair p) = if p `elem` ps then vs ++ [Right p] else vs ++ [Left $ __attributeNotInSourceError p ]

__attributeNotInSourceError :: TAST.TASTSimpleAtomicIndexPair -> TCInferenceError
__attributeNotInSourceError (TAST.TASTSimpleAtomicIndexKeyValue k _ ) = TE.makeError $ "The requested attribute is not known in the provided source: `" ++ CU.toString k ++ "`"

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

__extend' :: (CU.ToStringable a, TCX.Contextable b) => TCX.TCXSimpleTypeContext -> a -> b -> TCX.TCXSimpleTypeContext
__extend' c k r = do
    let k' = TCX.makeKey $ CU.toString k
    let r' = TCX.contextualize r
    TCX.extend k' r' c
