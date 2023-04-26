module ChaiMicroSql.ChaiMicroSql ( ) where

import           Data.List  (find)
import qualified Data.Map   as M
import           Data.Maybe (fromMaybe)

-- Basic AST

newtype AstSqlQuery
    = AstSqlQuery AstSelectStatement

data AstSelectStatement
    = AstBasicSelectStatement AstSelectHead AstSelectFrom

data AstSelectHead
    = AstSelectHeadAll
    | AstSelectHeadColumns [AstSelectHeadColumn]

data AstSelectHeadColumn
    = AstSelectHeadColumn {selectColumnName:: String, selectColumnSource:: String, selectColumnAlias:: Maybe String}
    -- TODO: support non-fully qualified column access
    -- = AstSelectHeadColumn {selectColumnAttribute:: String, selectColumnSource:: Maybe String, selectColumnAlias:: Maybe String}

newtype AstSelectFrom
    = AstSelectFromSources [AstSelectFromSource]

data AstSelectFromSource
    = AstSelectFromSourceTable {selectFromSourceTableName:: String, selectFromSourceTableAlias:: Maybe String}
    -- TODO: add supported to nested queries
    -- | AstSelectFromSourceQuery {selectFromSourceQuery:: AstSqlQuery, selectFromSourceQueryAlias:: Maybe String}


-- Basic Type System

-- Type wrapper


data BasicTypeVar
    = BasicTypeVarTotal
    | BasicTypeVarUnit String
    | BasicTypeVarRecord TypeContainer

type TypeContainer = [TypeAddress]

data TypeAddress = TypeAddress {typeAddressKey:: String, typeAddressValue :: BasicTypeVar}

tuplify :: TypeAddress -> (String, BasicTypeVar)
tuplify (TypeAddress k v) = (k, v)

findAttribute :: String -> TypeContainer -> Maybe BasicTypeVar
findAttribute k xs = fmap typeAddressValue (find (\c -> k == typeAddressKey c) xs)


-- Context

type TypeRecord = (M.Map String BasicTypeVar)

newtype BasicContext a = BasicContext a

instance Functor BasicContext where
    fmap :: (a -> b) -> BasicContext a -> BasicContext b
    fmap f (BasicContext a) = BasicContext $ f a

instance Applicative BasicContext where
  pure :: a -> BasicContext a
  pure = BasicContext
  (<*>) :: BasicContext (a -> b) -> BasicContext a -> BasicContext b
  (<*>) (BasicContext f) (BasicContext a) = BasicContext $ f a

instance Monad BasicContext where
  return :: a -> BasicContext a
  return = pure
  (>>=) :: BasicContext a -> (a -> BasicContext b) -> BasicContext b
  BasicContext a >>= f = f a

type BasicTypeContext = BasicContext TypeRecord

emptyContext :: BasicTypeContext
emptyContext = pure M.empty

-- Type wrapping

data TypeWrapper c t = TypeWrapper {context:: c, typevalue:: t}

-- Response wrapping

type TypeJudgement = Maybe BasicTypeVar

-- Type Operations

merge :: BasicTypeVar -> BasicTypeVar -> TypeJudgement
merge BasicTypeVarTotal x                           = pure x
merge (BasicTypeVarRecord p) (BasicTypeVarRecord q) = pure $ BasicTypeVarRecord $ p ++ q
merge _ _                                           = Nothing

merge' :: TypeJudgement -> TypeJudgement -> TypeJudgement
merge' j1 j2 = do
    t1 <- j1
    t2 <- j2
    merge t1 t2

unionContexts :: BasicTypeContext -> BasicTypeContext -> BasicTypeContext
unionContexts g1 g2 = do
    m1 <- g1
    M.union m1 <$> g2


-- AST Type inference

class Typed a where
    typeOf :: a -> (a, TypeJudgement)

instance Typed BasicTypeContext String where
    typeOf :: BasicTypeContext -> String -> (BasicTypeContext, TypeJudgement)
    typeOf g@(BasicContext m) k = (g, M.lookup k m)

instance Typed BasicTypeContext AstSelectFromSource where
    typeOf :: BasicTypeContext -> AstSelectFromSource -> (BasicTypeContext, TypeJudgement)
    typeOf g (AstSelectFromSourceTable s Nothing) = typeOf g s
    typeOf g (AstSelectFromSourceTable s (Just a)) =
        case mt of
            Just t  -> (fmap (M.insert a t) c , mt)
            Nothing -> (c, mt)
        where (c, mt) = typeOf g s

instance Typed BasicTypeContext AstSelectFrom where
    typeOf :: BasicTypeContext -> AstSelectFrom -> (BasicTypeContext, TypeJudgement)
    typeOf g (AstSelectFromSources xs) =
        foldl fOp (emptyContext, Just BasicTypeVarTotal) js
        where js = map (typeOf g) xs
              fOp (bg, bt) (ng, nt) = (unionContexts bg ng, merge' bt nt)

instance Typed BasicTypeContext AstSelectHeadColumn where
    typeOf :: BasicTypeContext -> AstSelectHeadColumn -> (BasicTypeContext, TypeJudgement)
    typeOf g (AstSelectHeadColumn n s a) =
        let al = fromMaybe n a in
        let (_, t) = typeOf g s in
            case t of
                Just (BasicTypeVarRecord r) -> case mft of
                    Nothing -> (g, Nothing)
                    Just ft  -> (g, Just (BasicTypeVarRecord [TypeAddress al ft]))
                    where mft = findAttribute n r
                Just _                      -> (g, Nothing)
                Nothing                     -> (g, Nothing)

instance Typed BasicTypeContext AstSelectHead where
    typeOf :: BasicTypeContext -> AstSelectHead -> (BasicTypeContext, TypeJudgement)
    typeOf g AstSelectHeadAll          = (g, Just BasicTypeVarTotal)
    typeOf g (AstSelectHeadColumns xs) =
        foldl fOp (emptyContext, Just BasicTypeVarTotal) js
        where js = map (typeOf g) xs
              fOp (bg, bt) (ng, nt) = (unionContexts bg ng, merge' bt nt)

instance Typed BasicTypeContext AstSelectStatement where
    typeOf :: BasicTypeContext -> AstSelectStatement -> (BasicTypeContext, TypeJudgement)
    typeOf g (AstBasicSelectStatement s f) =
        let (g', tf) = typeOf g f in
        let (_, ts) = typeOf (unionContexts g g') s in
            undefined
            -- FIXME: solve this using the projection
