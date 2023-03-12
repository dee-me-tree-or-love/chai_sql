module Language.Tokens (Token(..)) where

data Token
  = TComment
  | TChaiComment
  | TLeftBrace
  | TRightBrace
  | TSemicolon
  | TColon
  | TSelect
  | TFrom
  | TOperator String
  | TText String
  | TSingleQuoted String
  | TDoubleQuoted String
  | TNumber Int
  | TChaiSQLHead
  deriving (Eq, Show)