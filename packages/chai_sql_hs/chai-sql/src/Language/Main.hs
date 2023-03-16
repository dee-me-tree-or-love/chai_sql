module Language.Main
  (
    getTokens,
    getAst,
  )
where

import qualified Language.Ast    as LAST (StackSqlStatement, NaiveMaybeTypedAstContext)
import qualified Language.Lexer  as LL (scan)
import qualified Language.Parser as LP (parse)
import qualified Language.Tokens as LT (Token)

-- | Tokenizes a string.
--
-- Examples:
--
-- TODO: provide some examples
--
-- >>> getTokens "%%%%% this causes an error"
-- lexical error
--
getTokens :: String -> [LT.Token]
getTokens = LL.scan


-- | Parses the tokenized input into an AST
--
-- Examples:
--
-- TODO: provide some examples
--
getAst :: [LT.Token] -> LAST.StackSqlStatement LAST.NaiveMaybeTypedAstContext
getAst = LP.parse
