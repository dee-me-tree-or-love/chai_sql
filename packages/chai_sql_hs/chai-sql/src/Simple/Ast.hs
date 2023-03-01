module Simple.Ast (
      TypedExpression (..),
      TypeHint (..),
      Expression (..),
      Operator (..),
      Term (..)
) where

data TypedExpression
      = STypedExpression TypeHint Expression
      | SUntypedExpression Expression
      deriving (Eq, Show)

newtype TypeHint
      = STypeHint String
      deriving (Eq, Show)

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