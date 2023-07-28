module ChaiMicroSql.Parsing.Tokens (Token(..)) where

data Token
  = TComment
  | TChaiComment
  -- symbols
  | TLeftBrace
  | TRightBrace
  | TLeftAngle
  | TRightAngle
  | TLeftCurl
  | TRightCurl
  | TSemicolon
  | TColon
  | TComma
  | TDot
  -- keywords
  | TSelect
  | TFrom
  | TDistinct
  | TAll
  | TStar
  | TAs
  -- type keywords
  | TCsBool
  | TCsText
  | TCsNumber
  -- constructs
  | TOperator String
  | TTerm String
  | TSingleQuoted String
  | TDoubleQuoted String
  | TNumber Int
  deriving (Eq, Show)
