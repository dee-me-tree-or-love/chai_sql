{
module Simple.Lexer (scan) where
import Simple.Tokens as ST (Token (..))
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+                        ;
  "--".*                         ;
  "("                            { \_ -> ST.TLeftBrace }
  ")"                            { \_ -> ST.TRightBrace }
  [\=\+\-\*\/\(\)]               { \s -> ST.TOperator (head s) }
  $digit+                        { \s -> ST.TNumber (read s) }
  $alpha [$alpha $digit \_ \']*  { \s -> ST.TText s }


{
-- Each action has type :: String -> Token

-- | Tokenizes a string.
scan :: String -> [ST.Token]
scan = alexScanTokens
}