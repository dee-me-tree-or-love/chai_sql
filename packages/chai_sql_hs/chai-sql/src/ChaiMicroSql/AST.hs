{-# LANGUAGE InstanceSigs #-}
{- | The ASTT for a Micro SQL fragment.

Basic SQL Fragment feature support:

- [x] Asterix selection, e.g. `select * from foos;`
- [ ] Qualified asterix selection, e.g. `select foos.* from foos;`
- [x] Fully qualified selections, e.g. `select foos.foo from foos;`
- [x] Non-qualified selections, e.g. `select foo from foos;`
- [x] Selection-clause alias, e.g. `select foos.foo as bar from foos;`
- [x] From-clause alias, e.g. `select f.foo as bar from foos as f;`
- [x] Nested queries, e.g. `select f.foo as bar from (select * from bars as b) as f;`
- [ ] Where-clauses, e.g. `select * from foos where foos.foo > 42;`

-}
module ChaiMicroSql.AST ( module ChaiMicroSql.AST ) where

-- Basic AST
-- ~~~~~~~~~

-- | A single SQL select query.
-- TODO: @Typed@
data ASTSelectQuery = ASTSelectQuery ASTSelectList ASTFromList
    deriving (Show, Eq)

-- | A list of all attribute access.
-- TODO: @Typed@
type ASTSelectList = [ASTSelectAttribute]

-- | A single attribute access.
-- @Typed@
data ASTSelectAttribute
    = ASTSelectAttributeStar ASTSelectAttributeStarTotalRecord                      -- ^ e.g. @SELECT *@
    | ASTSelectAttributeReference ASTSelectAttributeReference                       -- ^ e.g. @SELECT X@
    | ASTSelectAttributeReferenceAlias ASTSelectAttributeReference ASTSimpleAlias   -- ^ e.g. @SELECT X AS Y@
    deriving (Show, Eq)

-- | A constant total record representation.
-- @Typed@
data ASTSelectAttributeStarTotalRecord = ASTSelectAttributeStarTotalRecord deriving (Show, Eq)

-- | A single attribute reference.
-- @Typed@
data ASTSelectAttributeReference
    = ASTSelectAttributeReferenceUnqualified ASTVariable                -- ^ e.g. `X`
    | ASTSelectAttributeReferenceQualified ASTVariable ASTVariable      -- ^ e.g. `X.Y`
    deriving (Show, Eq)

type ASTFromList = [ASTFromTable]

-- | A single table access.
-- TODO: @Typed@
data ASTFromTable
    = ASTFromTableReference ASTFromTableReference                       -- ^ e.g. @FROM X@
    | ASTFromTableReferenceAlias ASTFromTableReference ASTSimpleAlias   -- ^ e.g. @FROM X AS Y@
    deriving (Show, Eq)

-- | A single table reference.
-- TODO: @Typed@
data ASTFromTableReference
    = ASTFromTableReferenceUnqualified ASTVariable                -- ^ e.g. @X@
    | ASTFromTableReferenceNestedQuery ASTSelectQuery             -- ^ e.g. @([QUERY])@
    deriving (Show, Eq)

-- Common utilities

-- @Typed@
newtype ASTVariable = ASTVariable String deriving (Show, Eq)
-- TODO: @Typed@
newtype ASTSimpleAlias = ASTSimpleAlias String deriving (Show, Eq)

class ASTToString a where
    toString :: a -> String

instance ASTToString ASTVariable where
    toString :: ASTVariable -> String
    toString (ASTVariable v) = v

instance ASTToString ASTSimpleAlias where
    toString :: ASTSimpleAlias -> String
    toString (ASTSimpleAlias a) = a
