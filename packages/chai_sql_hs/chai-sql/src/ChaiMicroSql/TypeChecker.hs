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
module ChaiMicroSql.TypeChecker ( module ChaiMicroSql.TypeChecker ) where

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

class AST.TypeInfoable a => Inferrable a t where
    annotate :: TCX.TCXSimpleTypeContext -> a p -> a (p, Either TCInferenceError t)
    infer :: TCX.TCXSimpleTypeContext -> a p -> Either TCInferenceError t
    infer c v = snd . AST.getTypeInfo $ annotate c v

-- Axioms
-- ------


-- | Variable with inferrable types.
--
--      [Note]: Corresponds to the Axiom @A1@.
--
-- Examples:
--
-- >>> infer TCX.freshContext (AST.GAstVariable () "m") :: Either TCInferenceError TAST.TAstAtomicType
-- Left (TEBaseError "Could not infer variable type. Variable `m` is not in context.")
--
-- >>> annotate (TCX.extend (TCX.makeKey "m") (TCX.contextualize TAST.TAstAtomicTypeBool) TCX.freshContext) (AST.GAstVariable () "m") :: AST.GAstVariable ((), Either TCInferenceError TAST.TAstAtomicType)
-- GAstVariable ((),Right TAstAtomicTypeBool) "m"
--
-- >>> infer (TCX.extend (TCX.makeKey "m") (TCX.contextualize TAST.TAstAtomicTypeBool) TCX.freshContext) (AST.GAstVariable () "m") :: Either TCInferenceError TAST.TAstAtomicType
-- Right TAstAtomicTypeBool
--
instance TCX.Contextable t => (Inferrable AST.GAstVariable t) where
    annotate :: TCX.TCXSimpleTypeContext -> AST.GAstVariable p -> AST.GAstVariable (p, Either TCInferenceError t)
    annotate c (AST.GAstVariable t v) = do
        let i = TCX.get (TCX.makeKey v) c
        case i of
            Just __t  -> do
                let dt = TCX.decontextualize __t
                case dt of
                    Right t' -> AST.GAstVariable (t,pure t') v
                    e        -> AST.GAstVariable (t,e) v
            Nothing -> AST.GAstVariable (t,Left $ __varNotKnownError v) v

-- | A utility to construct a variable inference error message.
__varNotKnownError :: String -> TCInferenceError
__varNotKnownError v = TE.makeError $ "Could not infer variable type. Variable `" ++ v ++ "` is not in context."


-- | Total record type with inferrable type.
--
--      [Note]: Corresponds to the Axiom @A2@.
--
-- Examples:
--
-- >>> annotate (TCX.freshContext) (AST.GAstSelectAttributeStarTotalRecord ()) :: AST.GAstSelectAttributeStarTotalRecord ((), Either TCInferenceError TAST.TAstSimpleAtomicIndex)
-- GAstSelectAttributeStarTotalRecord ((),Right TAstSimpleTypeRecordTotal)
--
-- >>> infer (TCX.freshContext) (AST.GAstSelectAttributeStarTotalRecord ()) :: Either TCInferenceError TAST.TAstSimpleAtomicIndex
-- Right TAstSimpleTypeRecordTotal
--
instance (Inferrable AST.GAstSelectAttributeStarTotalRecord TAST.TAstSimpleAtomicIndex) where
    annotate :: TCX.TCXSimpleTypeContext -> AST.GAstSelectAttributeStarTotalRecord p -> AST.GAstSelectAttributeStarTotalRecord (p, Either TCInferenceError TAST.TAstSimpleAtomicIndex)
    annotate _ (AST.GAstSelectAttributeStarTotalRecord t) = AST.GAstSelectAttributeStarTotalRecord (t, pure TAST.TAstSimpleTypeRecordTotal)


-- -- Rules
-- -- -----

-- -- Attribute access
-- -- ................


-- | Attribute reference with type inference.
--
--      [Note]: Corresponds to the Rule @R1@
--
-- Examples:
--
-- >>> ctx = (TCX.extend (TCX.makeKey "m") (TCX.contextualize TAST.TAstAtomicTypeBool) TCX.freshContext)
-- >>> infer ctx (AST.GAstSelectAttributeReferenceUnqualified () (AST.GAstVariable () "m")) :: Either TCInferenceError TAST.TAstSimpleAtomicIndexPair
-- Right (TAstSimpleAtomicIndexKeyValue (TAstSimpleIndexKey "m") TAstAtomicTypeBool)
--
-- >>> annotate ctx (AST.GAstSelectAttributeReferenceUnqualified () (AST.GAstVariable () "m")) :: AST.GAstSelectAttributeReference () ((), Either TCInferenceError TAST.TAstSimpleAtomicIndexPair)
-- GAstSelectAttributeReferenceUnqualified ((),Right (TAstSimpleAtomicIndexKeyValue (TAstSimpleIndexKey "m") TAstAtomicTypeBool)) (GAstVariable () "m")
--
instance (Inferrable (AST.GAstSelectAttributeReference a) TAST.TAstSimpleAtomicIndexPair) where
    annotate :: TCX.TCXSimpleTypeContext -> AST.GAstSelectAttributeReference a p -> AST.GAstSelectAttributeReference a (p, Either TCInferenceError TAST.TAstSimpleAtomicIndexPair)
    annotate c (AST.GAstSelectAttributeReferenceUnqualified t v) = do
        let vt = infer c v
        let k = TAST.makeKey $ CU.toString v
        let at = TAST.TAstSimpleAtomicIndexKeyValue k <$> vt
        AST.GAstSelectAttributeReferenceUnqualified (t, at) v
    annotate c (AST.GAstSelectAttributeReferenceQualified t b v) = do
        let bt = infer c b
        let k = TAST.makeKey $ CU.toString v
        let vt = TAST.get k <$> bt
        case vt of
            Right (Just at) -> AST.GAstSelectAttributeReferenceQualified (t, Right $ TAST.TAstSimpleAtomicIndexKeyValue k at) b v
            Right Nothing -> AST.GAstSelectAttributeReferenceQualified (t, Left $ __recordUnknownAttributeError b v) b v
            Left e -> AST.GAstSelectAttributeReferenceQualified (t, Left e) b v

__recordUnknownAttributeError :: AST.GAstVariable a -> AST.GAstVariable b -> TCInferenceError
__recordUnknownAttributeError b v = TE.makeError $ "Record `" ++ bs ++ "` does not contain attribute `" ++ vs ++ "`. In expression `" ++ bs ++ "." ++ vs ++ "`, reconsider attribute access."
    where
        bs = CU.toString b
        vs = CU.toString v


-- | Single attribute access with type inference
--
--      [Note]: Corresponds to Rule @R2@
--
-- Examples:
--
-- >>> ctx = (TCX.extend (TCX.makeKey "m") (TCX.contextualize TAST.TAstAtomicTypeBool) TCX.freshContext)
-- >>> infer ctx (AST.GAstSelectAttributeAccessReference () (AST.GAstSelectAttributeReferenceUnqualified () (AST.GAstVariable () "m"))) :: Either TCInferenceError TAST.TAstSimpleAtomicIndex
-- Right (TAstSimpleAtomicIndexPair (TAstSimpleAtomicIndexKeyValue (TAstSimpleIndexKey "m") TAstAtomicTypeBool))
--
-- >>> infer ctx (AST.GAstSelectAttributeAccessStar () (AST.GAstSelectAttributeStarTotalRecord ())) :: Either TCInferenceError TAST.TAstSimpleAtomicIndex
-- Right TAstSimpleTypeRecordTotal
--
instance (Inferrable (AST.GAstSelectAttributeAccess a b c d) TAST.TAstSimpleAtomicIndex) where
    annotate :: TCX.TCXSimpleTypeContext -> AST.GAstSelectAttributeAccess a b c d p -> AST.GAstSelectAttributeAccess a b c d (p, Either TCInferenceError TAST.TAstSimpleAtomicIndex)
    annotate c (AST.GAstSelectAttributeAccessStar t s)             = do
        let at = infer c s
        AST.GAstSelectAttributeAccessStar (t, at) s
    annotate c (AST.GAstSelectAttributeAccessReference t a)        = do
        let at = infer c a
        AST.GAstSelectAttributeAccessReference (t, TAST.TAstSimpleAtomicIndexPair <$> at) a
    annotate c (AST.GAstSelectAttributeAccessReferenceAlias t a l@(AST.GAstSimpleAlias _ b)) = do
        case infer c a of
            Left e -> AST.GAstSelectAttributeAccessReferenceAlias (t, Left e) a l
            Right (TAST.TAstSimpleAtomicIndexKeyValue _ v) -> AST.GAstSelectAttributeAccessReferenceAlias (t,i) a l
                where i = pure $ TAST.TAstSimpleAtomicIndexPair $ TAST.TAstSimpleAtomicIndexKeyValue (TAST.makeKey b) v


-- -- | Attribute list access inference
-- --
-- --      [Note]: Corresponds to Rule @R3@
-- --
-- annotateSelectList :: TCX.TCXSimpleTypeContext -> [AST.GAstSelectAttributeAccess a b c d t] -> [AST.GAstSelectAttributeAccess a' b' c' d' (t,  Either TCInferenceError TAST.TAstSimpleAtomicIndex)]
-- annotateSelectList c = map (annotateAttribute c)


-- -- Table access
-- -- ............


-- | Table access with type  inference.
--
--      [Note]: Corresponds to the Axiom @A1@ and Rule @R2@
--
-- Examples:
--
-- >>> ctx = (TCX.extend (TCX.makeKey "m") (TCX.contextualize TAST.emptyRecord) TCX.freshContext)
-- >>> infer ctx (AST.GAstFromAccessReference () (AST.GAstVariable () "m")) :: Either TCInferenceError TAST.TAstSimpleRecordIndexPair
-- Right (TAstSimpleRecordIndexKeyValue (TAstSimpleIndexKey "m") (fromList []))
--
-- >>> annotate ctx (AST.GAstFromAccessReference () (AST.GAstVariable () "m")) :: AST.GAstFromAccess () () () () () () ((), Either TCInferenceError TAST.TAstSimpleRecordIndexPair)
-- GAstFromAccessReference ((),Right (TAstSimpleRecordIndexKeyValue (TAstSimpleIndexKey "m") (fromList []))) (GAstVariable () "m")
--
instance (Inferrable (AST.GAstFromAccess a b c d e f) TAST.TAstSimpleRecordIndexPair) where
    annotate :: TCX.TCXSimpleTypeContext -> AST.GAstFromAccess a b c d e f p -> AST.GAstFromAccess a b c d e f (p, Either TCInferenceError TAST.TAstSimpleRecordIndexPair)
    annotate c (AST.GAstFromAccessReference t v) = do
        let vt = infer c v
        let k = TAST.makeKey $ CU.toString v
        AST.GAstFromAccessReference (t, TAST.TAstSimpleRecordIndexKeyValue k <$> vt) v
    annotate c (AST.GAstFromAccessReferenceAlias t v a) = do
        let vt = infer c v
        let k = TAST.makeKey $ CU.toString a
        AST.GAstFromAccessReferenceAlias (t, TAST.TAstSimpleRecordIndexKeyValue k <$> vt) v a
    annotate c (AST.GAstFromAccessNestedQueryAlias t (AST.GAstSelectSubQuery q) a) = do
        error "not implemented yet!"

--     error "not implemented yet!"
--     -- FIXME: solve
--     -- -- infer the query result
--     -- -- TODO: annotate sub query and get the type
--     -- let qt = inferSelectQuery c q
--     -- -- if query contains duplicate columns, resolve colisions
--     -- let cqt = foldl __getCountLabels [] <$> qt
--     -- -- create a record form the resolved colisions
--     -- let r = TAST.makeRecord . map __dedup <$> cqt
--     -- -- return the record indexed by key
--     -- let k = TAST.makeKey $ CU.toString a
--     -- -- TODO: solve the sub query
--     -- let aq = error "TODO: Not implemented" :: AST.GAstSelectSubQuery a b (d, Either TCInferenceError TAST.TAstSimpleRecordIndexPair) c
--     -- AST.GAstFromAccessNestedQueryAlias (t,TAST.TAstSimpleRecordIndexKeyValue k <$> r) aq a

-- __getCountLabels :: Eq a => [(a, Int)] -> a -> [(a, Int)]
-- __getCountLabels xs p = xs ++ [(p, c)]
--     where c = length . filter ((== p) . fst) $ xs

-- __extendKey :: (CU.ToStringable a, Show b) => a -> b -> TAST.TAstSimpleIndexKey
-- __extendKey k n = TAST.makeKey $ CU.toString k ++ ":" ++ show n

-- __dedup :: (Eq b, Num b, Show b) => (TAST.TAstSimpleAtomicIndexPair, b) -> TAST.TAstSimpleAtomicIndexPair
-- __dedup (p@(TAST.TAstSimpleAtomicIndexKeyValue k v), n) = if n == 0 then p else TAST.TAstSimpleAtomicIndexKeyValue (__extendKey k n) v


-- -- Full SELECT query
-- -- .................


-- -- | Select query with type inference.
-- --
-- --      [Note]: Corresponds to Rule @R4@
-- --
-- instance (Inferrable (AST.GAstSelectQuery a b c d e f) TAST.TAstDbView) where
--     infer :: TCX.TCXSimpleTypeContext -> AST.GAstSelectQuery a b c d e f g -> Either TCInferenceError TAST.TAstDbView
--     infer c v = snd . AST.getTypeInfo $ annotateSelectQuery c v

-- -- | Select query result type inference.
-- --
-- --      [Note]: Corresponds to Rule @R4@
-- --
-- annotateSelectQuery :: TCX.TCXSimpleTypeContext -> AST.GAstSelectQuery a b c d e f t -> AST.GAstSelectQuery a' b' c' d' e' f' (t,Either TCInferenceError TAST.TAstDbView)
-- annotateSelectQuery c (AST.GAstSelectQuery t as fs) = do
--     error "not implemented yet!"
--     -- FIXME: solve
--     -- let afts = annotateFromList c fs
--     -- let __fts = infer c fs :: Either TCInferenceError [TAST.TAstSimpleRecordIndexPair]
--     -- case __fts of
--     --     Left e -> error "not implemented"
--     --     Right fts -> do
--     --         let fats = foldl __collectAttributes [] fts
--     --         let fc = foldl __extendFromRecordPair c fts
--     --         let fac = foldl __extendFromRecordPairAttributes c fts
--     --         let uc = TCX.unite fc fac :: TCX.TCXSimpleTypeContext
--     --         let __ats = inferSelectList uc as :: Either TCInferenceError [TAST.TAstSimpleAtomicIndex]
--     --         case __ats of
--     --             Left e -> error "not implemented"
--     --             Right ats -> do
--     --                 let rs = __resolveView fats ats
--     --                 let hsl = any isLeft rs
--     --                 let at = if hsl then Left $ TE.combineErrors $ lefts rs else pure $ rights rs
--     --                 AST.GAstSelectQuery (t, at) as afts


-- __resolveView :: [TAST.TAstSimpleAtomicIndexPair] -> [TAST.TAstSimpleAtomicIndex] -> [Either TCInferenceError TAST.TAstSimpleAtomicIndexPair]
-- __resolveView fs = foldl (__resolveIndexesToView fs) []

-- __resolveIndexesToView :: [TAST.TAstSimpleAtomicIndexPair] -> [Either TCInferenceError TAST.TAstSimpleAtomicIndexPair] -> TAST.TAstSimpleAtomicIndex ->  [Either TCInferenceError TAST.TAstSimpleAtomicIndexPair]
-- __resolveIndexesToView ps vs TAST.TAstSimpleTypeRecordTotal    = vs ++ map pure ps
-- __resolveIndexesToView ps vs (TAST.TAstSimpleAtomicIndexPair p) = if p `elem` ps then vs ++ [Right p] else vs ++ [Left $ __attributeNotInSourceError p ]

-- __attributeNotInSourceError :: TAST.TAstSimpleAtomicIndexPair -> TCInferenceError
-- __attributeNotInSourceError (TAST.TAstSimpleAtomicIndexKeyValue k _ ) = TE.makeError $ "The requested attribute is not known in the provided source: `" ++ CU.toString k ++ "`"

-- __collectAttributes :: [TAST.TAstSimpleAtomicIndexPair] -> TAST.TAstSimpleRecordIndexPair -> [TAST.TAstSimpleAtomicIndexPair]
-- __collectAttributes xs (TAST.TAstSimpleRecordIndexKeyValue _ r) = xs ++ TAST.indexes r

-- __extendFromRecordPair :: TCX.TCXSimpleTypeContext -> TAST.TAstSimpleRecordIndexPair -> TCX.TCXSimpleTypeContext
-- __extendFromRecordPair c (TAST.TAstSimpleRecordIndexKeyValue k r) = __extend' c k r

-- __extendFromRecordPairAttributes :: TCX.TCXSimpleTypeContext -> TAST.TAstSimpleRecordIndexPair -> TCX.TCXSimpleTypeContext
-- __extendFromRecordPairAttributes c (TAST.TAstSimpleRecordIndexKeyValue _ r) = do
--     let ras = TAST.pairs r
--     foldl __extendFromAttributes c ras

-- __extendFromAttributes :: TCX.TCXSimpleTypeContext -> (TAST.TAstSimpleIndexKey, TAST.TAstAtomicType) -> TCX.TCXSimpleTypeContext
-- __extendFromAttributes c (k, a) = __extend' c k a

-- __extend' :: (CU.ToStringable a, TCX.Contextable b) => TCX.TCXSimpleTypeContext -> a -> b -> TCX.TCXSimpleTypeContext
-- __extend' c k r = do
--     let k' = TCX.makeKey $ CU.toString k
--     let r' = TCX.contextualize r
--     TCX.extend k' r' c


-- -- Common Utilities
-- -- ----------------


-- | Annotate the list elements.
--
--      [Note]: Corresponds to Rule @R3@
--
annotateList :: Inferrable a t => TCX.TCXSimpleTypeContext -> [a p] -> [a (p, Either TCInferenceError t)]
annotateList c = map (annotate c)

-- | Infer the types from the list elements.
--
--      [Note]: Corresponds to Rule @R3@
--
inferList :: Inferrable a t => TCX.TCXSimpleTypeContext -> [a p] -> Either TCInferenceError [t]
inferList c vs = do
    let ats = annotateList c vs
    let ets = map (snd . AST.getTypeInfo) ats
    case any isLeft ets of
        True  -> Left $ TE.combineErrors $ lefts ets
        False -> Right $ rights ets
