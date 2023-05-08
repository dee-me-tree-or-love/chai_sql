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

-- AST without type information
-- ----------------------------

-- | A single SQL select query.
type ASTSelectQuery = GASTSelectQueryTyped ASTSelectAttribute ASTFromTable ()

-- | A single sub query access.
--
--      [Note]: is used to untie the type-level recursion.
--
newtype ASTSelectSubQuery = ASTSelectSubQuery ASTSelectQuery

-- | A single attribute access.
type ASTSelectAttribute = GASTSelectAttributeTyped ASTSelectAttributeReference ()

-- | A single reference access.
type ASTSelectAttributeReference = GASTSelectAttributeReferenceTyped ()

-- | A single from source access.
type ASTFromTable = GASTFromTableTyped ASTSelectSubQuery ()

-- Typed AST building blocks
-- -------------------------


-- | A single SQL select query.
data GASTSelectQueryTyped s f t = GASTSelectQueryTyped [s] [f] t
    deriving (Show, Eq)

-- | A single attribute access.
data GASTSelectAttributeTyped r t
    = GASTSelectAttributeTypedStar ASTSelectAttributeStarTotalRecord t  -- ^ e.g. @SELECT *@
    | GASTSelectAttributeTypedReference r t                             -- ^ e.g. @SELECT X@
    | GASTSelectAttributeTypedReferenceAlias r ASTSimpleAlias t         -- ^ e.g. @SELECT X AS Y@
    deriving (Show, Eq)

-- | A single attribute reference.
data GASTSelectAttributeReferenceTyped t
    = GASTSelectAttributeReferenceTypedUnqualified ASTVariable t               -- ^ e.g. `X`
    | GASTSelectAttributeReferenceTypedQualified ASTVariable ASTVariable t     -- ^ e.g. `X.Y`
    deriving (Show, Eq)

-- | A single table access.
data GASTFromTableTyped q t
    = GASTFromTableTypedReference ASTVariable t                                                 -- ^ e.g. @FROM X@
    | GASTFromTableTypedReferenceAlias ASTVariable ASTSimpleAlias t                             -- ^ e.g. @FROM X AS Y@
    | GASTFromNestedQueryTypedReferenceAlias q ASTSimpleAlias t   -- ^ e.g. @FROM (...) AS Y@
    deriving (Show, Eq)

-- Common utilities
-- ----------------

-- | A constant total record representation.
data ASTSelectAttributeStarTotalRecord = ASTSelectAttributeStarTotalRecord deriving (Show, Eq)

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
