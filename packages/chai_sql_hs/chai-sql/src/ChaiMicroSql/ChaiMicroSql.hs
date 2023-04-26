{-# LANGUAGE InstanceSigs #-}

module ChaiMicroSql.ChaiMicroSql ( ) where

import qualified Data.List as L (find)
import qualified Data.Map  as M


-- Basic AST
-- ~~~~~~~~~

newtype AstSqlQuery
    = AstSqlQuery AstSelectStatement
    deriving (Show, Eq)

data AstSelectStatement
    = AstBasicSelectStatement AstSelectHead AstSelectFrom
    deriving (Show, Eq)

data AstSelectHead
    = AstSelectHeadAll
    | AstSelectHeadColumns [AstSelectHeadColumn]
    deriving (Show, Eq)

data AstSelectHeadColumn
    = AstSelectHeadColumn {selectColumnName:: String, selectColumnSource:: String, selectColumnAlias:: Maybe String}
    -- TODO: support non-fully qualified column access
    -- = AstSelectHeadColumn {selectColumnAttribute:: String, selectColumnSource:: Maybe String, selectColumnAlias:: Maybe String}
    deriving (Show, Eq)

newtype AstSelectFrom
    = AstSelectFromSources [AstSelectFromSource]
    deriving (Show, Eq)

data AstSelectFromSource
    = AstSelectFromSourceTable {selectFromSourceTableName:: String, selectFromSourceTableAlias:: Maybe String}
    -- TODO: add supported to nested queries
    -- | AstSelectFromSourceQuery {selectFromSourceQuery:: AstSqlQuery, selectFromSourceQueryAlias:: Maybe String}
    deriving (Show, Eq)


-- Basic Type System
-- ~~~~~~~~~~~~~~~~~~

-- Type Variables

data TypeVariable
    = TypeVariableTotal
    | TypeVariableUnit String
    | TypeVariableRecord TypeContainer
    deriving (Show, Eq)

type TypeContainer = [TypeAddress]

data TypeAddress = TypeAddress {typeAddressKey:: String, typeAddressValue :: TypeVariable}
    deriving (Show, Eq)

-- Type manipulations

tuplify :: TypeAddress -> (String, TypeVariable)
tuplify (TypeAddress k v) = (k, v)

find :: String -> TypeContainer -> Maybe TypeVariable
find k xs = fmap typeAddressValue (L.find (\c -> k == typeAddressKey c) xs)


-- Context
-- ~~~~~~~

type TypeRecord = (M.Map String TypeVariable)

newtype BasicContext a = BasicContext a

-- Context utils

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

type TypeBasicContext = BasicContext TypeRecord

-- Context manipulation

emptyContext :: TypeBasicContext
emptyContext = pure M.empty

unionContexts :: TypeBasicContext -> TypeBasicContext -> TypeBasicContext
unionContexts g1 g2 = do
    m1 <- g1
    M.union m1 <$> g2


-- Type Operations
-- ~~~~~~~~~~~~~~~

-- Response wrapping

newtype TypeErrorMessage = TypeErrorMessage String

type TypeJudgement = Either TypeErrorMessage TypeVariable

-- Type judgements

merge :: TypeVariable -> TypeVariable -> TypeJudgement
merge TypeVariableTotal x                           = pure x
merge (TypeVariableRecord p) (TypeVariableRecord q) = pure $ TypeVariableRecord $ p ++ q
merge v u                                           = Left $ TypeErrorMessage m
    where m = "Could not merge two variables: " ++ show u ++ ", and " ++ show v

merge' :: TypeJudgement -> TypeJudgement -> TypeJudgement
merge' j1 j2 = do
    t1 <- j1
    t2 <- j2
    merge t1 t2

-- Type wrapping

data TypeWrapper c t = TypeWrapper {context:: c, typevalue:: t}

-- AST Type inference
-- ~~~~~~~~~~~~~~~~~~

class Typed a where
    typeOf :: a -> (a, TypeJudgement)

