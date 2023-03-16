module Language.Ast
  (
    module Language.Ast
  )
  where

import qualified Language.Types.System as LTS

-- Ast context information
-- =======================

data AstContext t = AstContext
  { lineNumber   :: Int
  , columnNumber :: Int
  , typeInfo     :: t
  }
  deriving (Eq, Show)

type NaiveTypeInfo = Maybe LTS.TypeVariable
type NaiveMaybeTypedAstContext = AstContext NaiveTypeInfo

placeholderContext :: NaiveMaybeTypedAstContext
placeholderContext = AstContext {lineNumber = 0, columnNumber = 0, typeInfo = Nothing}

-- Top Level: SQL Statements
-- =========================

type StackSqlStatement ctx = [SqlStatement ctx]

data SqlStatement ctx
  = SSelectStatement ctx (SelectStatement ctx)
  | SSqlComment ctx (SqlComment ctx)
  deriving (Eq, Show)


-- Mid Level: SQL Comments
-- +++++++++++++++++++++++

data SqlComment ctx
  = SCommentPlain ctx
  | SCommentChai ctx (ChaiStatement ctx)
  deriving (Eq, Show)

data ChaiStatement ctx
  = SChaiTrigger ctx (Term ctx)
  | SChaiExpression ctx (Term ctx) (Term ctx) (Operator ctx) (Term ctx)
  -- TODO(new-features): aggregate the SChaiCompound into a separate ChaiProposition construct
  | SChaiCompound ctx (Term ctx) (Term ctx) (Term ctx) (StackChaiAttributePair ctx)
  deriving (Eq, Show)

type StackChaiAttributePair ctx = [ChaiAttributePair ctx]

data ChaiAttributePair ctx
  = SChaiAttributePair ctx (Term ctx) (Term ctx)
  deriving (Eq, Show)

-- Mid Level: SELECT Statements
-- ++++++++++++++++++++++++++++

data SelectStatement ctx
  -- support for SELECT
  = SSelectStatementAtom ctx (MaybeSelectOption ctx) (StackSelectAccess ctx)
  -- support for SELECT-FROM
  | SSelectStatementWithFrom ctx (MaybeSelectOption ctx) (StackSelectAccess ctx) (SelectFrom ctx)
  -- TODO(backlog): support from SELECT-FROM-WHERE
  -- | SSelectStatementWithFromWhere MaybeSelectOption StackSelectAccess SelectFrom SelectWhere
  deriving (Eq, Show)

type MaybeSelectOption ctx = Maybe (SelectOption ctx)

data SelectOption ctx
  = SSelectDistinct ctx
  | SSelectAll ctx
  deriving (Eq, Show)

type StackSelectAccess ctx = [SelectAccess ctx]

data SelectAccess ctx
  = SSelectAccessColumn ctx (Term ctx)
  | SSelectAccessColumnQualified ctx (Term ctx) (Term ctx)
  | SSelectAccessConstant ctx (Constant ctx)
  | SSelectAccessStar ctx
  -- TODO(backlog): support SELECT X AS Y
  deriving (Eq, Show)

data SelectFrom ctx
  = SSelectFromTable ctx (Term ctx)
  | SSelectFromStatements ctx (StackSqlStatement ctx)
  -- TODO(backlog): support FROM X AS Y
  deriving (Eq, Show)

-- Low Level: TERMS, CONSTANTS, OPERATORS
-- +++++++++++++++++++++++++++

data Operator ctx = SOperator ctx String
  deriving (Eq, Show)

data Term ctx = STerm ctx String
  deriving (Eq, Show)

data Constant ctx
  = SSingleQuotedTextConstant ctx String
  | SDoubleQuotedTextConstant ctx String
  | SNumberConstant ctx Int
  deriving (Eq, Show)
