module Language.Ast
  (
    module Language.Ast
  )
  where


-- Top Level: SQL Statements
-- +++++++++++++++++++++++++

type StackSqlStatement = [SqlStatement]

data SqlStatement
  = SSelectStatement SelectStatement
  | SSqlComment SqlComment
  deriving (Eq, Show)


-- Mid Level: SQL Comments
-- +++++++++++++++++++++++

data SqlComment
  = SCommentPlain
  | SCommentChai ChaiStatement
  deriving (Eq, Show)

data ChaiStatement
  = SChaiTrigger Term
  | SChaiExpression Term Term Operator Term
  -- TODO(new-features): aggregate the SChaiCompound into a separate ChaiProposition construct
  | SChaiCompound Term Term Term StackChaiAttributePair
  deriving (Eq, Show)

type StackChaiAttributePair = [ChaiAttributePair]

data ChaiAttributePair
  = SChaiAttributePair Term Term
  deriving (Eq, Show)

-- Mid Level: SELECT Statements
-- ++++++++++++++++++++++++++++

data SelectStatement
  -- support for SELECT
  = SSelectStatementAtom MaybeSelectOption StackSelectAccess
  -- support for SELECT-FROM
  | SSelectStatementWithFrom MaybeSelectOption StackSelectAccess SelectFrom
  -- TODO(backlog): support from SELECT-FROM-WHERE
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
  -- TODO(backlog): support SELECT X AS Y
  deriving (Eq, Show)

data SelectFrom
  = SSelectFromTable Term
  | SSelectFromStatements StackSqlStatement
  -- TODO(backlog): support FROM X AS Y
  deriving (Eq, Show)

-- Low Level: TERMS, CONSTANTS, OPERATORS
-- +++++++++++++++++++++++++++

newtype Operator = SOperator String
  deriving (Eq, Show)

newtype Term = STerm String
  deriving (Eq, Show)

data Constant
  = SSingleQuotedTextConstant String
  | SDoubleQuotedTextConstant String
  | SNumberConstant Int
  deriving (Eq, Show)
