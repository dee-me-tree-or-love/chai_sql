module Simple.Tokens (Token (..)) where

data Token
  = TLeftBrace
  | TRightBrace
  | TOperator Char
  | TText String
  | TNumber Int
  deriving (Eq, Show)