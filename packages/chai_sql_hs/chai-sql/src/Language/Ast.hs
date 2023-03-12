module Language.Ast
  (
    module Language.Ast
  )
  where

type StackSqlStatement = [SqlStatement]

newtype SqlStatement
  = SSelectStatement SelectStatement
  deriving (Eq, Show)

data SelectStatement
  = SSelectStatementAtom MaybeSelectOption StackSelectAccess
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
  deriving (Eq, Show)

newtype Term = STerm String
  deriving (Eq, Show)

data Constant
  = SSingleQuotedTextConstant String
  | SDoubleQuotedTextConstant String
  | SNumberConstant Int
  deriving (Eq, Show)
