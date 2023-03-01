module Simple.Main (main) where

import qualified Simple.Lexer as SL ( scan )
import qualified Simple.Tokens as ST ( Token )
import qualified Simple.Parser as SP ( parse )
import qualified Simple.Ast as SAST ( Expression )


-- | Tokenizes a string.
--
-- Examples:
--
-- >>> getTokens "(+ 2 1)"
-- [TLeftBrace,TOperator '+',TNumber 2,TNumber 1,TRightBrace]
--
-- >>> getTokens "(+ cat 1)"
-- [TLeftBrace,TOperator '+',TText "cat",TNumber 1,TRightBrace]
--
-- >>> getTokens "-- this is a comment"
-- []
--
-- >>> getTokens "%%%%% this causes an error"
-- lexical error
getTokens :: String -> [ST.Token]
getTokens = SL.scan


-- | Parses the tokenized input into an AST
--
-- >>> getAst $ getTokens "(+ 1 2)"
-- STerm (SExpressionContainer (SBinExpression (SOperator '+') (STerm (SNumber 1)) (STerm (SNumber 2))))
--
-- >>> getAst $ getTokens "(+ 1)"
-- STerm (SExpressionContainer (SUnExpression (SOperator '+') (STerm (SNumber 1))))
--
-- >>> getAst $ getTokens "(+ (+ 1 2))"
-- STerm (SExpressionContainer (SUnExpression (SOperator '+') (STerm (SExpressionContainer (SBinExpression (SOperator '+') (STerm (SNumber 1)) (STerm (SNumber 2)))))))
--
getAst :: [ST.Token] -> SAST.Expression
getAst = SP.parse

main :: IO ()
main = do
  s <- getContents
  print $ getAst $ getTokens s 
