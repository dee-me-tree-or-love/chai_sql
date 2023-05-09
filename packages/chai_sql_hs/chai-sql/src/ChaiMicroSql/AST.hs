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
import qualified ChaiMicroSql.TypeErrors  as TE

-- AST with type annotations
-- -------------------------

type TypeHintAnnotation a b = (a, Either TE.TEBaseError b)

-- | A single SQL select query with possible type hint and required annotation.
type AstSelectQueryHintedAnnotated = GAstSelectQuery
    (TypeHintAnnotation () TAST.TAstSimpleAtomicIndexPair)
    (TypeHintAnnotation () TAST.TAstSimpleAtomicIndex)
    (TypeHintAnnotation () TAST.TAstSimpleRecordIndexPair)
    (Maybe TAST.TAstDbView)


-- Typed AST building blocks
-- -------------------------

class TypeInfoable a where
    getTypeInfo :: a t -> t

-- | A single SQL select query.
data GAstSelectQuery srt sat ft t
    = GAstSelectQuery t
        [GAstSelectAttributeAccess srt sat]
        [GAstFromAccess srt sat t ft]
    deriving (Show, Eq)

instance TypeInfoable (GAstSelectQuery a b c) where
    getTypeInfo :: GAstSelectQuery a b c t -> t
    getTypeInfo (GAstSelectQuery t _ _) = t

-- | A single attribute access.
data GAstSelectAttributeAccess rt t
    = GAstSelectAttributeAccessStar t AstSelectAttributeStarTotalRecord                             -- ^ e.g. @SELECT *@
    | GAstSelectAttributeAccessReference t (GAstSelectAttributeReference rt)                        -- ^ e.g. @SELECT X@
    | GAstSelectAttributeAccessReferenceAlias t (GAstSelectAttributeReference rt) AstSimpleAlias    -- ^ e.g. @SELECT X AS Y@
    deriving (Show, Eq)

instance TypeInfoable (GAstSelectAttributeAccess a) where
    getTypeInfo :: GAstSelectAttributeAccess a t -> t
    getTypeInfo (GAstSelectAttributeAccessStar t _)             = t
    getTypeInfo (GAstSelectAttributeAccessReference t _)        = t
    getTypeInfo (GAstSelectAttributeAccessReferenceAlias t _ _) = t

-- | A single attribute reference.
data GAstSelectAttributeReference t
    = GAstSelectAttributeReferenceUnqualified t AstVariable               -- ^ e.g. `X`
    | GAstSelectAttributeReferenceQualified t AstVariable AstVariable     -- ^ e.g. `X.Y`
    deriving (Show, Eq)

instance TypeInfoable GAstSelectAttributeReference where
    getTypeInfo :: GAstSelectAttributeReference t -> t
    getTypeInfo (GAstSelectAttributeReferenceUnqualified t _) = t
    getTypeInfo (GAstSelectAttributeReferenceQualified t _ _) = t

-- | A single sub query access.
--
--      [Note]: is used to untie the type-level recursion.
--
newtype GAstSelectSubQuery srt sat ft t = GAstSelectSubQuery (GAstSelectQuery srt sat ft t)
    deriving (Show, Eq)

instance TypeInfoable (GAstSelectSubQuery a b c) where
    getTypeInfo :: GAstSelectSubQuery a b c t -> t
    getTypeInfo (GAstSelectSubQuery q) = getTypeInfo q

-- | A single table access.
data GAstFromAccess srt sat qt t
    = GAstFromAccessReference t AstVariable                                                 -- ^ e.g. @FROM X@
    | GAstFromAccessReferenceAlias t AstVariable AstSimpleAlias                             -- ^ e.g. @FROM X AS Y@
    | GAstFromAccessNestedQueryAlias t (GAstSelectSubQuery srt sat t qt) AstSimpleAlias     -- ^ e.g. @FROM (...) AS Y@
    deriving (Show, Eq)

instance TypeInfoable (GAstFromAccess a b c) where
    getTypeInfo :: GAstFromAccess a b c t -> t
    getTypeInfo (GAstFromAccessReference t _)          = t
    getTypeInfo (GAstFromAccessReferenceAlias t _ _)   = t
    getTypeInfo (GAstFromAccessNestedQueryAlias t _ _) = t


-- Common utilities
-- ----------------


-- | A constant total record representation.
data AstSelectAttributeStarTotalRecord = AstSelectAttributeStarTotalRecord deriving (Show, Eq)

-- | A simple variable name wrapper.
-- @Typed@
newtype AstVariable = AstVariable String deriving (Show, Eq)

-- | A simple alias name wrapper.
newtype AstSimpleAlias = AstSimpleAlias String deriving (Show, Eq)

instance CU.ToStringable AstVariable where
    toString :: AstVariable -> String
    toString (AstVariable v) = v

instance CU.ToStringable AstSimpleAlias where
    toString :: AstSimpleAlias -> String
    toString (AstSimpleAlias a) = a
