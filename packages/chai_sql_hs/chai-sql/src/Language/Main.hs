module Language.Main
  (
    getTokens,
    getAst,
  )
where

import qualified Language.Ast    as LAST (Term)
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
-- TODO: provide examples
--
getAst :: [LT.Token] -> LAST.Term
getAst = LP.parse
