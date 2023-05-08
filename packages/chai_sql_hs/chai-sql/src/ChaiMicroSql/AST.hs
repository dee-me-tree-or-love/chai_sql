{-# LANGUAGE InstanceSigs #-}
{- | The ASTT for a Micro SQL fragment.

= /Micro/ SQL Fragment

Supports SQL queries only of the following shape: @SELECT \_\_ FROM \_\_;@

Some supported examples:

- [x] Asterix selection, e.g. `select * from foos;`
- [x] Fully qualified selections, e.g. `select foos.foo from foos;`
- [x] Non-qualified selections, e.g. `select foo from foos;`
- [x] Selection-clause alias, e.g. `select foos.foo as bar from foos;`
- [x] From-clause alias, e.g. `select f.foo as bar from foos as f;`
- [x] Nested queries, e.g. `select f.foo as bar from (select * from bars) as f;`

== (Some) supported features

    - Supports aliases @x AS y@ in @SELECT@ and @FROM@
    - Supports nested queries (sub-queries) in @FROM@
    - Supports multiple column selection with @SELECT x, y, ...@
    - Supports cross-product in @FROM@ with @FROM x, y, ...@
    - Supports wildcard (asterisk/star) selection with @SELECT *@
    - Supports fully-qualified selections with @SELECT x.y@

== Not-supported features

    - No support for qualified wildcard select with @SELECT x.*@
    - No support for @WHERE@ clauses
    - No support for @GROUP BY@ clauses
    - No support for @ORDER BY@ clauses
    - No support for @LIMIT@ clauses

-}
module ChaiMicroSql.AST ( module ChaiMicroSql.AST ) where

import qualified ChaiMicroSql.CommonUtils as CU
import qualified ChaiMicroSql.TAST        as TAST

-- Basic AST
-- ~~~~~~~~~

-- | An optional type hint wrapper
data ASTTypeHinted a b = ASTTypeHinted a (Maybe b)
    deriving (Show, Eq)

-- | A regular select query with an optional type hint.
type ASTTypeHintedSelectQuery = ASTTypeHinted ASTSelectQuery TAST.TASTDbView

-- | A single SQL select query.
-- @Typed@
data ASTSelectQuery = ASTSelectQuery ASTSelectList ASTFromList
    deriving (Show, Eq)

-- | A list of all attribute access.
-- @Typed@
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

-- | A list of all source table access.
-- @Typed@
type ASTFromList = [ASTFromTable]

-- | A single table access.
-- @Typed@
data ASTFromTable
    = ASTFromTableReference ASTVariable                                 -- ^ e.g. @FROM X@
    | ASTFromTableReferenceAlias ASTVariable ASTSimpleAlias             -- ^ e.g. @FROM X AS Y@
    | ASTFromNestedQueryReferenceAlias ASTTypeHintedSelectQuery ASTSimpleAlias    -- ^ e.g. @FROM (...) AS Y@
    deriving (Show, Eq)

-- Common utilities
-- ----------------

-- | A simple variable name wrapper.
-- @Typed@
newtype ASTVariable = ASTVariable String deriving (Show, Eq)
-- | A simple alias name wrapper.
newtype ASTSimpleAlias = ASTSimpleAlias String deriving (Show, Eq)

instance CU.ToStringable ASTVariable where
    toString :: ASTVariable -> String
    toString (ASTVariable v) = v

instance CU.ToStringable ASTSimpleAlias where
    toString :: ASTSimpleAlias -> String
    toString (ASTSimpleAlias a) = a
