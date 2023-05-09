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
data GAstSelectQuery
        srt -- ^ Select reference type info
        sat -- ^ Select access type info
        ft  -- ^ From access type info
        st  -- ^ Star type info
        vt  -- ^ Variable type info
        at  -- ^ Alias type info
        t   -- ^ Query type info
    = GAstSelectQuery t
        [GAstSelectAttributeAccess srt st vt at sat]
        [GAstFromAccess srt sat t st vt at ft]
    deriving (Show, Eq)

instance TypeInfoable (GAstSelectQuery a b c d e f) where
    getTypeInfo (GAstSelectQuery t _ _) = t

-- | A single attribute access.
data GAstSelectAttributeAccess
        rt  -- ^ Select reference type info
        st  -- ^ Star type
        vt  -- ^ Variable type
        at  -- ^ Alias type
        t   -- ^ Select access type
    = GAstSelectAttributeAccessStar t (GAstSelectAttributeStarTotalRecord st)                          -- ^ e.g. @SELECT *@
    | GAstSelectAttributeAccessReference t (GAstSelectAttributeReference vt rt)                        -- ^ e.g. @SELECT X@
    | GAstSelectAttributeAccessReferenceAlias t (GAstSelectAttributeReference vt rt) (GAstSimpleAlias at)    -- ^ e.g. @SELECT X AS Y@
    deriving (Show, Eq)

instance TypeInfoable (GAstSelectAttributeAccess a b c d) where
    getTypeInfo (GAstSelectAttributeAccessStar t _)             = t
    getTypeInfo (GAstSelectAttributeAccessReference t _)        = t
    getTypeInfo (GAstSelectAttributeAccessReferenceAlias t _ _) = t

-- | A single attribute reference.
data GAstSelectAttributeReference vt t
    = GAstSelectAttributeReferenceUnqualified t (GAstVariable vt)               -- ^ e.g. `X`
    | GAstSelectAttributeReferenceQualified t (GAstVariable vt) (GAstVariable vt)     -- ^ e.g. `X.Y`
    deriving (Show, Eq)

instance TypeInfoable (GAstSelectAttributeReference a) where
    getTypeInfo (GAstSelectAttributeReferenceUnqualified t _) = t
    getTypeInfo (GAstSelectAttributeReferenceQualified t _ _) = t

-- | A single sub query access.
--
--      [Note]: is used to untie the type-level recursion.
--
newtype GAstSelectSubQuery
        srt -- ^ Select reference type info
        sat -- ^ Select access type info
        ft  -- ^ From access type info
        st  -- ^ Star type info
        vt  -- ^ Variable type info
        at  -- ^ Alias type info
        t   -- ^ Query type info
    = GAstSelectSubQuery (GAstSelectQuery srt sat ft st vt at t)
    deriving (Show, Eq)

instance TypeInfoable (GAstSelectSubQuery a b c d e f) where
    getTypeInfo (GAstSelectSubQuery q) = getTypeInfo q


-- | A single table access.
data GAstFromAccess
        srt -- ^ Select reference type info
        sat -- ^ Select access type info
        qt  -- ^ Query type info
        st  -- ^ Star type info
        vt  -- ^ Variable type info
        at  -- ^ Alias type info
        t   -- ^ From type info
    = GAstFromAccessReference t (GAstVariable vt)                                                 -- ^ e.g. @FROM X@
    | GAstFromAccessReferenceAlias t (GAstVariable vt) (GAstSimpleAlias at)                             -- ^ e.g. @FROM X AS Y@
    | GAstFromAccessNestedQueryAlias t (GAstSelectSubQuery srt sat t st vt at qt) (GAstSimpleAlias at)     -- ^ e.g. @FROM (...) AS Y@
    deriving (Show, Eq)

instance TypeInfoable (GAstFromAccess a b c d e f) where
    getTypeInfo (GAstFromAccessReference t _)          = t
    getTypeInfo (GAstFromAccessReferenceAlias t _ _)   = t
    getTypeInfo (GAstFromAccessNestedQueryAlias t _ _) = t


-- Common utilities
-- ----------------


-- | A constant total record representation.
newtype GAstSelectAttributeStarTotalRecord t = GAstSelectAttributeStarTotalRecord t deriving (Show, Eq)

instance TypeInfoable GAstSelectAttributeStarTotalRecord where
    getTypeInfo :: GAstSelectAttributeStarTotalRecord t -> t
    getTypeInfo (GAstSelectAttributeStarTotalRecord t) = t

-- | A simple variable name wrapper.
-- @Typed@
data GAstVariable t = GAstVariable t String deriving (Show, Eq)

instance TypeInfoable GAstVariable where
    getTypeInfo :: GAstVariable t -> t
    getTypeInfo (GAstVariable t _) = t

-- | A simple alias name wrapper.
data GAstSimpleAlias t = GAstSimpleAlias t String deriving (Show, Eq)

instance TypeInfoable GAstSimpleAlias where
    getTypeInfo :: GAstSimpleAlias t -> t
    getTypeInfo (GAstSimpleAlias t _) = t

instance CU.ToStringable (GAstVariable a) where
    toString :: GAstVariable a -> String
    toString (GAstVariable _ v) = v

instance CU.ToStringable (GAstSimpleAlias a) where
    toString :: GAstSimpleAlias a -> String
    toString (GAstSimpleAlias _ a) = a
