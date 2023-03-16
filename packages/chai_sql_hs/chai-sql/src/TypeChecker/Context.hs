{-# Language FlexibleInstances #-}
{-# Language InstanceSigs #-}
module TypeChecker.Context
    (
        module TypeChecker.Context
    ) 
    where

import qualified Data.Map              as M
import qualified Language.Types.System as LTS
import qualified Schemas.Loader        as SL

-- Config to context loaders
-- =========================

class FromConfig a where
    fromConfigSchema :: SL.ConfigSchema -> a

-- Base Types context
-- ++++++++++++++++++

newtype DbTypeName = DbTypeName String
    deriving (Eq, Show, Ord)

type DbTypeContext = M.Map DbTypeName LTS.TypeTerm

instance FromConfig DbTypeContext where
    fromConfigSchema :: SL.ConfigSchema -> DbTypeContext
    fromConfigSchema (SL.ConfigSchema ts _) = M.fromList $ map (\s -> (DbTypeName s, LTS.TVTypeTerm s)) ts

-- Base Table context
-- ++++++++++++++++++

newtype DbTableName = DbTableName String
    deriving (Eq, Show, Ord)

newtype DbAttributeName = DbAttributeName String
    deriving (Eq, Show, Ord)

data TypedDbTable = TypedDbTable
    {
        dbTableName  :: DbTableName,
        dbAttributes :: M.Map DbAttributeName LTS.TypeTerm
    }
    deriving (Eq, Show)

type DbTableContext = M.Map DbTableName TypedDbTable

indexTable :: SL.ConfigTableDefinition -> (DbTableName, TypedDbTable)
indexTable (SL.ConfigTableDefinition t ats)
    = (DbTableName t, TypedDbTable {dbTableName = DbTableName t, dbAttributes = M.fromList $ map indexAttribute ats})
    where indexAttribute (SL.ConfigAttributeDefinition n s) = (DbAttributeName n, LTS.TVTypeTerm s)

instance FromConfig DbTableContext where
    fromConfigSchema :: SL.ConfigSchema -> DbTableContext
    fromConfigSchema (SL.ConfigSchema _ ts) = M.fromList $ map indexTable ts

