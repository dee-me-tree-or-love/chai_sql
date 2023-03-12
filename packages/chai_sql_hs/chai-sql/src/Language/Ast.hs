module Language.Ast
  (
    module Language.Ast
  )
  where

data SqlStatement
  = SSqlComment SqlComment
  | SSqlExpression SqlExpression

data SqlComment
  = SBasicComment String
  | SChaiComment  ChaiComment

data ChaiComment
  = SChaiCheck
  | SChaiNewtype String ChaiTypeExpression
  | SChaiReturns ChaiTypeExpression

-- TODO: fix
data ChaiTypeExpression
  = SChaiTypeNumber
  | SChaiTypeString
  | SChaiTypeBoolean
  | SChaiTypeCompoundDbView String String
  | SChaiTypeVariable String

-- TODO: fix
data SqlExpression
  = SNothing

data Term
  = SNumber Int
  | SText String
  deriving (Eq, Show)