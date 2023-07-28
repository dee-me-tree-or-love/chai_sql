{-# LANGUAGE InstanceSigs #-}
{- | The AstT for a Micro SQL fragment.

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

-- Concrete AST
-- ------------

-- TODO(tech-debt): simplify the data names and align with parser

-- | A single SQL select query with a possible type hint.
data AstSelectQuery = AstSelectQuery (Maybe TAST.TAstDbView) [AstSelectAttributeAccess] [AstFromAccess]
    deriving (Show, Eq)

-- TODO: extend to support constants
-- | A single attribute access.
data AstSelectAttributeAccess
    = AstSelectAttributeAccessStar AstSelectAttributeStarTotalRecord                        -- ^ e.g. @SELECT * ...@
    | AstSelectAttributeAccessReference AstSelectAttributeReference                         -- ^ e.g. @SELECT X ...@
    | AstSelectAttributeAccessReferenceAlias AstSelectAttributeReference AstSimpleAlias    -- ^ e.g. @SELECT X AS Y ...@
    deriving (Show, Eq)

-- | A single attribute reference.
data AstSelectAttributeReference
    = AstSelectAttributeReferenceUnqualified AstVariable               -- ^ e.g. @X@
    | AstSelectAttributeReferenceQualified AstVariable AstVariable     -- ^ e.g. @X.Y@
    deriving (Show, Eq)

-- | A constant total record representation.
data AstSelectAttributeStarTotalRecord = AstSelectAttributeStarTotalRecord deriving (Show, Eq)

-- | A single table access.
data AstFromAccess
    = AstFromAccessReference AstVariable                              -- ^ e.g. @... FROM x ...@
    | AstFromAccessReferenceAlias AstVariable AstSimpleAlias          -- ^ e.g. @... FROM x AS y ...@
    | AstFromAccessNestedQueryAlias AstSelectQuery AstSimpleAlias     -- ^ e.g. @... FROM (SELECT ...) AS Y ...@
    deriving (Show, Eq)

-- | A simple variable name wrapper.
newtype AstVariable = AstVariable String deriving (Show, Eq)

-- | A simple alias name wrapper.
newtype AstSimpleAlias = AstSimpleAlias String deriving (Show, Eq)

instance CU.ToStringable AstVariable where
    toString :: AstVariable -> String
    toString (AstVariable v) = v

instance CU.ToStringable AstSimpleAlias where
    toString :: AstSimpleAlias -> String
    toString (AstSimpleAlias a) = a

