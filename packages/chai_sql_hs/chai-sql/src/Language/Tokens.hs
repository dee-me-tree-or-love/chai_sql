module Language.Tokens (Token(..)) where

data Token
  = TComment
  | TChaiComment
  | TLeftBrace
  | TRightBrace
  | TSemicolon
  | TColon
  | TComma
  | TDot
  | TSelect
  | TFrom
  | TDistinct
  | TAll
  | TStar
  | TOperator String
  | TTerm String
  | TSingleQuoted String
  | TDoubleQuoted String
  | TNumber Int
  | TChaiSQLHead
  deriving (Eq, Show)
