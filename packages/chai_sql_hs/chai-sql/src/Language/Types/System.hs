module Language.Types.System
    (
        module Language.Types.System
    )
    where

data TypeVariable
    = TVBasic TypeTerm
    | TVCompound
  deriving (Eq, Show)

newtype TypeTerm
    = TVTypeTerm String
  deriving (Eq, Show)

data TypeVarialbeCompound
    = TVTypeVariableDbView DbViewNotation StackDbViewAttribute
  deriving (Eq, Show)

newtype DbViewNotation
    = TVDbViewNotation String
  deriving (Eq, Show)

type StackDbViewAttribute = [DbViewAttribute]

data DbViewAttribute
    = TVDbViewAttribute TypeTerm TypeTerm
  deriving (Eq, Show)
