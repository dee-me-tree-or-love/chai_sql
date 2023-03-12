{
module Language.Lexer (scan) where
import Language.Tokens as LT
}

%wrapper "basic"

@select   = select|SELECT
@from     = from|FROM
@as       = as|AS
@chaisql  = \@chaisql

$operator     = [\=\+\-\*\/]              -- various operators
$digit        = 0-9                       -- digits
$alpha        = [a-zA-Z]                  -- alphabetic characters
$punctiation  = [\,\;\(\)\[\]\{\}]        -- punctiation signs
$special      = [\!\@\#\$\%\^\&\*\<\>\?]  -- special signs
$binder       = [\_\-]                    -- hyphens and other links

@verbal       = $alpha [$alpha $digit $binder $special]*
@spaceverbal  = [$alpha $digit $binder $special $punctiation $white]*

tokens :-

  $white+                           ;

  -- constructs
  "--"(\ )+@chaisql                 { \_ -> LT.TChaiComment }
  "--"(\ )+@spaceverbal             { \_ -> LT.TComment }
  ";"                               { \_ -> LT.TSemicolon }
  ":"                               { \_ -> LT.TColon }
  "("                               { \_ -> LT.TLeftBrace }
  ")"                               { \_ -> LT.TRightBrace }

  -- special keywords
  -- @chaisql                          { \_ -> LT.TChaiSQLHead }
  @select                           { \_ -> LT.TSelect }
  @from                             { \_ -> LT.TFrom }

  -- expression blocks
  $operator+                        { \s -> LT.TOperator s }

  -- numeric inputs
  $digit+                           { \s -> LT.TNumber (read s) }

  -- textual inputs
  @verbal                           { \s -> LT.TText s }
  \' @spaceverbal \'                { \s -> LT.TSingleQuoted s }
  \" @spaceverbal \"                { \s -> LT.TDoubleQuoted s }


{
-- Each action has type :: String -> Token

-- | Tokenizes a string.
scan :: String -> [LT.Token]
scan = alexScanTokens
}