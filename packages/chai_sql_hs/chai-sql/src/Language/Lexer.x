{
module Language.Lexer (scan) where
import Language.Tokens as LT
}

%wrapper "basic"

@select   = select|SELECT
@from     = from|FROM
@as       = as|AS
@distinct = distinct|DISTINCT
@all      = all|ALL
@star     = \*
@chaisql  = \@chaisql

$operator     = [\=\+\-\/]              -- various operators
$digit        = 0-9                       -- digits
$alpha        = [a-zA-Z]                  -- alphabetic characters
$punctiation  = [\,\;\(\)\[\]\{\}]        -- punctiation signs
$special      = [\!\@\#\$\%\^\&\*\<\>\?]  -- special signs
$binder       = [\_\-]                    -- hyphens and other links

@textual      = $alpha [$alpha $digit $binder]*
@verbal       = $alpha [$alpha $digit $binder $special]*
@spaceverbal  = [$alpha $digit $binder $special $punctiation $white]*

tokens :-

  $white+                           ;

  -- constructs
  "--"(\ )+@chaisql                 { \_ -> LT.TChaiComment }
  "--"(\ )+@spaceverbal             { \_ -> LT.TComment }
  ";"                               { \_ -> LT.TSemicolon }
  ":"                               { \_ -> LT.TColon }
  ","                               { \_ -> LT.TComma }
  "."                               { \_ -> LT.TDot }
  "("                               { \_ -> LT.TLeftBrace }
  ")"                               { \_ -> LT.TRightBrace }
  "<"                               { \_ -> LT.TLeftAngle }
  ">"                               { \_ -> LT.TRightAngle }
  "{"                               { \_ -> LT.TLeftCurl }
  "}"                               { \_ -> LT.TRightCurl }

  -- special keywords
  @select                           { \_ -> LT.TSelect }
  @from                             { \_ -> LT.TFrom }
  @distinct                         { \_ -> LT.TDistinct }
  @all                              { \_ -> LT.TAll }
  @star                             { \_ -> LT.TStar }

  -- expression blocks
  $operator+                        { \s -> LT.TOperator s }

  -- numeric inputs
  $digit+                           { \s -> LT.TNumber (read s) }

  -- textual inputs
  @textual                           { \s -> LT.TTerm s }
  \' @spaceverbal \'                { \s -> LT.TSingleQuoted s }
  \" @spaceverbal \"                { \s -> LT.TDoubleQuoted s }


{
-- Each action has type :: String -> Token

-- | Tokenizes a string.
scan :: String -> [LT.Token]
scan = alexScanTokens
}