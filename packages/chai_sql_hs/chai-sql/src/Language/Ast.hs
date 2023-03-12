module Language.Ast
  (
    module Language.Ast
  )
  where


-- Top Level: SQL Statements
-- +++++++++++++++++++++++++

type StackSqlStatement = [SqlStatement]

newtype SqlStatement
  = SSelectStatement SelectStatement
  deriving (Eq, Show)


-- Mid Level: SELECT Statements
-- ++++++++++++++++++++++++++++

data SelectStatement
  -- support for SELECT
  = SSelectStatementAtom MaybeSelectOption StackSelectAccess
  -- support for SELECT-FROM
  | SSelectStatementWithFrom MaybeSelectOption StackSelectAccess SelectFrom
  -- TODO(feature): support from SELECT-FROM-WHERE
  -- | SSelectStatementWithFromWhere MaybeSelectOption StackSelectAccess SelectFrom SelectWhere
  deriving (Eq, Show)

type MaybeSelectOption = Maybe SelectOption

data SelectOption
  = SSelectDistinct
  | SSelectAll
  deriving (Eq, Show)

type StackSelectAccess = [SelectAccess]

data SelectAccess
  = SSelectAccessColumn Term
  | SSelectAccessColumnQualified Term Term
  | SSelectAccessConstant Constant
  | SSelectAccessStar
  -- TODO(feature): support SELECT X AS Y
  deriving (Eq, Show)

data SelectFrom
  = SSelectFromTable Term
  | SSelectFromStatements StackSqlStatement
  -- TODO(feature): support FROM X AS Y
  deriving (Eq, Show)

-- Low Level: TERMS, CONSTANTS
-- +++++++++++++++++++++++++++

newtype Term = STerm String
  deriving (Eq, Show)

data Constant
  = SSingleQuotedTextConstant String
  | SDoubleQuotedTextConstant String
  | SNumberConstant Int
  deriving (Eq, Show)
