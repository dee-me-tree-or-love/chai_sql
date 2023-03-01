module Simple.Ast (
      Expression (..),
      Operator (..),
      Term (..)
) where

data Expression
      = SBinExpression Operator Expression Expression
      | SUnExpression Operator Expression
      | STerm Term
      deriving (Eq, Show)

newtype Operator
      = SOperator Char 
      deriving (Eq, Show)

data Term
      = SNumber Int
      | SText String
      | SExpressionContainer Expression
      deriving (Eq, Show)