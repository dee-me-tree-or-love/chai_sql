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

-- -- Type Inference Procedures
-- -- -------------------------

-- -- Base utilities
-- -- --------------

-- | Simple alias for a type checker's inference error.
type TcInferenceError = TE.TEBaseError

-- | Simple alias for a type checker's check error.
type TcCheckError = TE.TEBaseError

-- | Simple wrapper for inference result.
type TcInferenceResult = Either TcInferenceError

-- | Simple wrapper for checking result.
type TcCheckResult = Maybe TcCheckError

-- | A result of the annotation with its source.
data TcInferenceWrapper t a = TcInferenceWrapper {
        inferenceResult :: TcInferenceResult t,
        checkingResult  :: TcCheckResult,
        inferenceSource :: a
    }
    deriving (Show, Eq)

class Annotatable a t where
    annotate :: TCX.TCXSimpleTypeContext -> a -> TcInferenceWrapper t a

    check :: TCX.TCXSimpleTypeContext -> a -> (TcCheckResult, TcInferenceResult t)
    check c v = (checkingResult s, inferenceResult s)
        where s = annotate c v

    infer :: TCX.TCXSimpleTypeContext -> a -> TcInferenceResult t
    infer c v = snd $ check c v

-- Axioms
-- ------


-- | Variable with inferrable types.
--
--      [Note]: Corresponds to the Axiom @A1@.
--
-- Examples:
--
-- >>> infer TCX.freshContext (AST.AstVariable "m") :: TcInferenceResult TAST.TAstAtomicType
-- Left (TEBaseError "Could not infer variable type. Variable `m` is not in context.")
--
-- >>> ctx = TCX.extend (TCX.makeKey "m") (TCX.contextualize TAST.TAstAtomicTypeBool) TCX.freshContext
-- >>> annotate ctx (AST.AstVariable "m") :: TcInferenceWrapper TAST.TAstAtomicType AST.AstVariable
-- TcInferenceWrapper {content = (Right TAstAtomicTypeBool,AstVariable "m")}
--
-- >>> infer ctx (AST.AstVariable "m")  :: TcInferenceResult TAST.TAstAtomicType
-- Right TAstAtomicTypeBool
--
instance TCX.Contextable t => Annotatable AST.AstVariable t where
    annotate :: TCX.TCXSimpleTypeContext -> AST.AstVariable -> TcInferenceWrapper t AST.AstVariable
    annotate c a@(AST.AstVariable v) = do
        let i = TCX.get (TCX.makeKey v) c
        case i of
            Just __t  -> do
                let dt = TCX.decontextualize __t :: TCX.Contextable t => Either TCX.TCXContextError t
                case dt of
                    Right t' -> TcInferenceWrapper (pure t') Nothing a
                    Left e   -> TcInferenceWrapper (Left e) Nothing a
            Nothing -> TcInferenceWrapper (Left $ __varNotKnownError v) Nothing a

-- | A utility to construct a variable inference error message.
__varNotKnownError :: String -> TcInferenceError
__varNotKnownError v = TE.makeError $ "Could not infer variable type. Variable `" ++ v ++ "` is not in context."


-- | Total record type with inferrable type.
--
--      [Note]: Corresponds to the Axiom @A2@.
--
-- Examples:
--
-- >>> annotate TCX.freshContext AST.AstSelectAttributeStarTotalRecord :: TcInferenceWrapper TAST.TAstSimpleAtomicIndex AST.AstSelectAttributeStarTotalRecord
-- TcInferenceWrapper {content = (Right TAstSimpleTypeRecordTotal,AstSelectAttributeStarTotalRecord)}
--
-- >>> infer TCX.freshContext AST.AstSelectAttributeStarTotalRecord :: TcInferenceResult TAST.TAstSimpleAtomicIndex
-- Right TAstSimpleTypeRecordTotal
--
instance Annotatable AST.AstSelectAttributeStarTotalRecord TAST.TAstSimpleAtomicIndex where
  annotate :: TCX.TCXSimpleTypeContext -> AST.AstSelectAttributeStarTotalRecord -> TcInferenceWrapper TAST.TAstSimpleAtomicIndex AST.AstSelectAttributeStarTotalRecord
  annotate _ = TcInferenceWrapper (pure TAST.TAstSimpleTypeRecordTotal) Nothing


-- -- -- Rules
-- -- -- -----

-- -- -- Attribute access
-- -- -- ................


-- | Attribute reference with type inference.
--
--      [Note]: Corresponds to the Rule @R1@
--
-- Examples:
--
-- >>> ctx = (TCX.extend (TCX.makeKey "m") (TCX.contextualize TAST.TAstAtomicTypeBool) TCX.freshContext)
-- >>> infer ctx (AST.AstSelectAttributeReferenceUnqualified (AST.AstVariable "m")) :: TcInferenceResult TAST.TAstSimpleAtomicIndexPair
-- Right (TAstSimpleAtomicIndexKeyValue (TAstSimpleIndexKey "m") TAstAtomicTypeBool)
--
-- >>> annotate ctx (AST.AstSelectAttributeReferenceQualified (AST.AstVariable "m") (AST.AstVariable "q")) :: TcInferenceWrapper TAST.TAstSimpleAtomicIndexPair AST.AstSelectAttributeReference
-- TcInferenceWrapper {content = (Left (TEBaseError "Can not retrieve record type from stored atomic."),AstSelectAttributeReferenceQualified (AstVariable "m") (AstVariable "q"))}
--
instance Annotatable AST.AstSelectAttributeReference TAST.TAstSimpleAtomicIndexPair where
    annotate :: TCX.TCXSimpleTypeContext -> AST.AstSelectAttributeReference -> TcInferenceWrapper TAST.TAstSimpleAtomicIndexPair AST.AstSelectAttributeReference
    annotate c a@(AST.AstSelectAttributeReferenceUnqualified v)  = do
        let (ce, vt) = check c v
        let k = TAST.makeKey $ CU.toString v
        let at = TAST.TAstSimpleAtomicIndexKeyValue k <$> vt
        TcInferenceWrapper at ce a
    annotate c a@(AST.AstSelectAttributeReferenceQualified b v) = do
        let (ce, bt) = check c b
        let k = TAST.makeKey $ CU.toString v
        let vt = TAST.get k <$> bt
        case vt of
            Right (Just at) -> TcInferenceWrapper (pure $ TAST.TAstSimpleAtomicIndexKeyValue k at) ce a
            Right Nothing -> TcInferenceWrapper (Left $ __recordUnknownAttributeError b v) ce a
            Left e -> TcInferenceWrapper (Left e) ce a


__recordUnknownAttributeError :: AST.AstVariable -> AST.AstVariable -> TcInferenceError
__recordUnknownAttributeError b v = do
    let bs = CU.toString b
    let vs = CU.toString v
    TE.makeError $ "Record `" ++ bs ++ "` does not contain attribute `" ++ vs ++ "`. In expression `" ++ bs ++ "." ++ vs ++ "`, reconsider attribute access."


-- | Single attribute access with type inference
--
--      [Note]: Corresponds to Rule @R2@
--
-- Examples:
--
-- >>> ctx = (TCX.extend (TCX.makeKey "m") (TCX.contextualize TAST.TAstAtomicTypeBool) TCX.freshContext)
-- >>> infer ctx (AST.AstSelectAttributeAccessReference (AST.AstSelectAttributeReferenceUnqualified (AST.AstVariable "m"))) :: TcInferenceResult TAST.TAstSimpleAtomicIndex
-- Right (TAstSimpleAtomicIndexPair (TAstSimpleAtomicIndexKeyValue (TAstSimpleIndexKey "m") TAstAtomicTypeBool))
--
-- >>> infer ctx (AST.AstSelectAttributeAccessStar AST.AstSelectAttributeStarTotalRecord) :: TcInferenceResult TAST.TAstSimpleAtomicIndex
-- Right TAstSimpleTypeRecordTotal
--
-- >>> annotate ctx (AST.AstSelectAttributeAccessStar AST.AstSelectAttributeStarTotalRecord) :: TcInferenceWrapper TAST.TAstSimpleAtomicIndex AST.AstSelectAttributeAccess
-- TcInferenceWrapper {content = (Right TAstSimpleTypeRecordTotal,AstSelectAttributeAccessStar AstSelectAttributeStarTotalRecord)}
--
instance Annotatable AST.AstSelectAttributeAccess TAST.TAstSimpleAtomicIndex where
    annotate :: TCX.TCXSimpleTypeContext -> AST.AstSelectAttributeAccess -> TcInferenceWrapper TAST.TAstSimpleAtomicIndex AST.AstSelectAttributeAccess
    annotate c a@(AST.AstSelectAttributeAccessStar v) =  TcInferenceWrapper vt ce a
        where (ce, vt) = check c v
    annotate c a@(AST.AstSelectAttributeAccessReference v) = do
        let (ce, vt) = check c v
        let at = TAST.TAstSimpleAtomicIndexPair <$> vt
        TcInferenceWrapper at ce a
    annotate c a@(AST.AstSelectAttributeAccessReferenceAlias v l) = do
        let (ce, vt) = check c v
        case vt of
            Right (TAST.TAstSimpleAtomicIndexKeyValue _ u) -> do
                let at = TAST.TAstSimpleAtomicIndexPair $ TAST.TAstSimpleAtomicIndexKeyValue (TAST.makeKey $ CU.toString l) u
                TcInferenceWrapper (pure at) ce a
            Left e  -> TcInferenceWrapper (Left e) ce a


-- Table access
-- ............


-- | Table access with type  inference.
--
--      [Note]: Corresponds to the Axiom @A1@ and Rule @R2@
--
-- Examples:
--
-- >>> ctx = (TCX.extend (TCX.makeKey "m") (TCX.contextualize TAST.emptyRecord) TCX.freshContext)
-- >>> infer ctx (AST.AstFromAccessReference (AST.AstVariable "m")) :: TcInferenceResult TAST.TAstSimpleRecordIndexPair
-- Right (TAstSimpleRecordIndexKeyValue (TAstSimpleIndexKey "m") (fromList []))
--
-- >>> annotate ctx (AST.AstFromAccessReference (AST.AstVariable "m")) :: TcInferenceWrapper TAST.TAstSimpleRecordIndexPair AST.AstFromAccess
-- TcInferenceWrapper {content = (Right (TAstSimpleRecordIndexKeyValue (TAstSimpleIndexKey "m") (fromList [])),AstFromAccessReference (AstVariable "m"))}
--
-- TODO: add a sub-query example
--
instance Annotatable AST.AstFromAccess TAST.TAstSimpleRecordIndexPair where
    annotate :: TCX.TCXSimpleTypeContext -> AST.AstFromAccess -> TcInferenceWrapper TAST.TAstSimpleRecordIndexPair AST.AstFromAccess
    annotate c a@(AST.AstFromAccessReference v) = do
        let (ce, vt) = check c v
        let k = TAST.makeKey $ CU.toString v
        let at = TAST.TAstSimpleRecordIndexKeyValue k <$> vt
        TcInferenceWrapper at ce a
    annotate c a@(AST.AstFromAccessReferenceAlias v l) = do
        let (ce, vt) = check c v
        let k = TAST.makeKey $ CU.toString l
        let at = TAST.TAstSimpleRecordIndexKeyValue k <$> vt
        TcInferenceWrapper at ce a
    annotate c a@(AST.AstFromAccessNestedQueryAlias q l) = do
        -- annotate sub query and get the type
        let aq = annotate c q :: TcInferenceWrapper TAST.TAstDbView AST.AstSelectQuery
        let qt = inferenceResult aq
        let ce = checkingResult aq
        -- if query contains duplicate columns, resolve colisions
        let cqt = foldl __getCountLabels [] <$> qt
        -- create a record form the resolved colisions
        let r = TAST.makeRecord . map __dedup <$> cqt
        -- return the record indexed by key
        let k = TAST.makeKey $ CU.toString l
        let at = TAST.TAstSimpleRecordIndexKeyValue k <$> r
        TcInferenceWrapper at ce a


__getCountLabels :: Eq a => [(a, Int)] -> a -> [(a, Int)]
__getCountLabels xs p = xs ++ [(p, c)]
    where c = length . filter ((== p) . fst) $ xs

__dedup :: (Eq b, Num b, Show b) => (TAST.TAstSimpleAtomicIndexPair, b) -> TAST.TAstSimpleAtomicIndexPair
__dedup (p@(TAST.TAstSimpleAtomicIndexKeyValue k v), n) = if n == 0 then p else TAST.TAstSimpleAtomicIndexKeyValue (__extendKey k n) v

__extendKey :: (CU.ToStringable a, Show b) => a -> b -> TAST.TAstSimpleIndexKey
__extendKey k n = TAST.makeKey $ CU.toString k ++ ":" ++ show n


-- -- -- Full SELECT query
-- -- -- .................


-- | Select query with type inference.
--
--      [Note]: Corresponds to Rule @R4@
--
-- TODO: add an example
--
instance Annotatable AST.AstSelectQuery TAST.TAstDbView where
  annotate :: TCX.TCXSimpleTypeContext -> AST.AstSelectQuery -> TcInferenceWrapper TAST.TAstDbView AST.AstSelectQuery
  annotate c a@(AST.AstSelectQuery Nothing as fs) = do -- ^ annotating a query /without/ a type hint
    let afs = map (annotate c) fs :: [TcInferenceWrapper TAST.TAstSimpleRecordIndexPair AST.AstFromAccess]
    let efts = map inferenceResult afs :: [TcInferenceResult TAST.TAstSimpleRecordIndexPair]
    let cets = map checkingResult afs :: [TcCheckResult]
    case any isLeft efts of
        True  -> TcInferenceWrapper (Left $ TE.combineErrors $ lefts efts) Nothing a
        False -> do
            let fts = rights efts
            let fats = foldl __collectAttributes [] fts
            let fc = foldl __extendFromRecordPair c fts
            let fac = foldl __extendFromAttributes c fats
            let uc = TCX.unite fc fac :: TCX.TCXSimpleTypeContext
            let aas = map (annotate uc) as :: [TcInferenceWrapper TAST.TAstSimpleAtomicIndex AST.AstSelectAttributeAccess]
            let eats = map inferenceResult aas :: [TcInferenceResult TAST.TAstSimpleAtomicIndex]
            case any isLeft eats of
                True  -> TcInferenceWrapper (Left $ TE.combineErrors $ lefts eats) (foldl (\e' e'' -> TE.joinErrors <$> e' <*> e'') Nothing cets) a
                False -> do
                    let ats = rights eats
                    let rs = __resolveView fats ats
                    let at = if any isLeft rs then Left $ TE.combineErrors $ lefts rs else pure $ rights rs
                    TcInferenceWrapper at Nothing a
  annotate c a@(AST.AstSelectQuery (Just h) as fs) = do -- ^ annotating a query /with/ a type hint
    let a' = annotate c (AST.AstSelectQuery Nothing as fs)
    let it = inferenceResult a'
    let ce = checkingResult a'
    case it of
        e@(Left _) -> TcInferenceWrapper e ce a
        t@(Right i) -> do
            if i == h
            then TcInferenceWrapper t ce a
            else TcInferenceWrapper t (TE.joinErrors <$> ce  <*> Just (__unmatchedHintError h i)) a

__resolveView :: [TAST.TAstSimpleAtomicIndexPair] -> [TAST.TAstSimpleAtomicIndex] -> [TcInferenceResult TAST.TAstSimpleAtomicIndexPair]
__resolveView fs = foldl (__resolveIndexesToView fs) []

__resolveIndexesToView :: [TAST.TAstSimpleAtomicIndexPair] -> [TcInferenceResult TAST.TAstSimpleAtomicIndexPair] -> TAST.TAstSimpleAtomicIndex ->  [TcInferenceResult TAST.TAstSimpleAtomicIndexPair]
__resolveIndexesToView ps vs TAST.TAstSimpleTypeRecordTotal    = vs ++ map pure ps
__resolveIndexesToView ps vs (TAST.TAstSimpleAtomicIndexPair p) = if p `elem` ps then vs ++ [Right p] else vs ++ [Left $ __attributeNotInSourceError p ]

__attributeNotInSourceError :: TAST.TAstSimpleAtomicIndexPair -> TcInferenceError
__attributeNotInSourceError (TAST.TAstSimpleAtomicIndexKeyValue k _ ) = TE.makeError $ "The requested attribute is not known in the provided source: `" ++ CU.toString k ++ "`"

__collectAttributes :: [TAST.TAstSimpleAtomicIndexPair] -> TAST.TAstSimpleRecordIndexPair -> [TAST.TAstSimpleAtomicIndexPair]
__collectAttributes xs (TAST.TAstSimpleRecordIndexKeyValue _ r) = xs ++ TAST.indexes r

__extendFromRecordPair :: TCX.TCXSimpleTypeContext -> TAST.TAstSimpleRecordIndexPair -> TCX.TCXSimpleTypeContext
__extendFromRecordPair c (TAST.TAstSimpleRecordIndexKeyValue k r) = __extend' c k r

__extendFromAttributes :: TCX.TCXSimpleTypeContext -> TAST.TAstSimpleAtomicIndexPair -> TCX.TCXSimpleTypeContext
__extendFromAttributes c (TAST.TAstSimpleAtomicIndexKeyValue k r) = __extend' c k r

__extend' :: (CU.ToStringable a, TCX.Contextable b) => TCX.TCXSimpleTypeContext -> a -> b -> TCX.TCXSimpleTypeContext
__extend' c k r = do
    let k' = TCX.makeKey $ CU.toString k
    let r' = TCX.contextualize r
    TCX.extend k' r' c

__unmatchedHintError :: (Show h, Show i) => h -> i -> TcInferenceError
__unmatchedHintError h i = TE.makeError $ "Specified type hint `" ++ show h ++ "` does not match inferred type `" ++ show i ++ "`"
