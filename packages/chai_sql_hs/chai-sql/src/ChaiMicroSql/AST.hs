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
import qualified ChaiMicroSql.TypeErrors  as TE

-- AST with type annotations
-- -------------------------

type TypeHintAnnotation a b = (a, Either TE.TEBaseError b)

-- | A single SQL select query with possible type hint and required annotation.
type ASTSelectQueryHintedAnnotated = GASTSelectQueryTyped
    (TypeHintAnnotation () TAST.TASTSimpleAtomicIndexPair)
    (TypeHintAnnotation () TAST.TASTSimpleAtomicIndex)
    (TypeHintAnnotation () TAST.TASTSimpleRecordIndexPair)
    (Maybe TAST.TASTDbView)


-- AST with type hints
-- -------------------

-- | A single SQL select query with an optional type hint.
type ASTSelectQueryHinted = GASTSelectQueryTyped () () () (Maybe TAST.TASTDbView)

-- AST without type information
-- ----------------------------

-- | A single SQL select query.
type ASTSelectQuery = GASTSelectQueryTyped () () () ()


-- Typed AST building blocks
-- -------------------------

-- | A single SQL select query.
data GASTSelectQueryTyped srt sat ft qt
    = GASTSelectQueryTyped
        [GASTSelectAttributeTyped srt sat]
        [GASTFromTableTyped srt sat ft qt]
        qt
    deriving (Show, Eq)

-- | A single attribute access.
data GASTSelectAttributeTyped rt t
    = GASTSelectAttributeTypedStar ASTSelectAttributeStarTotalRecord t  -- ^ e.g. @SELECT *@
    | GASTSelectAttributeTypedReference (GASTSelectAttributeReferenceTyped rt) t                             -- ^ e.g. @SELECT X@
    | GASTSelectAttributeTypedReferenceAlias (GASTSelectAttributeReferenceTyped rt) ASTSimpleAlias t         -- ^ e.g. @SELECT X AS Y@
    deriving (Show, Eq)

-- | A single attribute reference.
data GASTSelectAttributeReferenceTyped t
    = GASTSelectAttributeReferenceTypedUnqualified ASTVariable t               -- ^ e.g. `X`
    | GASTSelectAttributeReferenceTypedQualified ASTVariable ASTVariable t     -- ^ e.g. `X.Y`
    deriving (Show, Eq)

-- | A single sub query access.
--
--      [Note]: is used to untie the type-level recursion.
--
newtype GASTSelectSubQueryTyped srt sat ft qt = GASTSelectSubQueryTyped (GASTSelectQueryTyped srt sat ft qt)
    deriving (Show, Eq)

-- | A single table access.
data GASTFromTableTyped srt sat ft qt
    = GASTFromTableTypedReference ASTVariable ft                                                        -- ^ e.g. @FROM X@
    | GASTFromTableTypedReferenceAlias ASTVariable ASTSimpleAlias ft                                    -- ^ e.g. @FROM X AS Y@
    | GASTFromNestedQueryTypedReferenceAlias (GASTSelectSubQueryTyped srt sat ft qt) ASTSimpleAlias ft  -- ^ e.g. @FROM (...) AS Y@
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
