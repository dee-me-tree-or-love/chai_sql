{
module ChaiMicroSql.Parsing.Lexer (tokenize) where

import ChaiMicroSql.Parsing.Tokens as CPT
}

%wrapper "basic"

@select   = select|SELECT
@from     = from|FROM
@as       = as|AS
@distinct = distinct|DISTINCT
@all      = all|ALL
@star     = \*
@chaisql  = \@chaisql|\@cs|chaisql
@csBool   = Bool
@csText   = Text
@csNumber = Number

$operator     = [\=\+\-\/]                -- various operators
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
  -- FIXME: this somehow overshadows the TChaiComment with space
  "--"(\ )+@spaceverbal             { \_ -> CPT.TComment }
  "--"@chaisql                      { \_ -> CPT.TChaiComment }
  ";"                               { \_ -> CPT.TSemicolon }
  ":"                               { \_ -> CPT.TColon }
  ","                               { \_ -> CPT.TComma }
  "."                               { \_ -> CPT.TDot }
  "("                               { \_ -> CPT.TLeftBrace }
  ")"                               { \_ -> CPT.TRightBrace }
  "<"                               { \_ -> CPT.TLeftAngle }
  ">"                               { \_ -> CPT.TRightAngle }
  "{"                               { \_ -> CPT.TLeftCurl }
  "}"                               { \_ -> CPT.TRightCurl }

  -- special keywords
  @select                           { \_ -> CPT.TSelect }
  @from                             { \_ -> CPT.TFrom }
  @distinct                         { \_ -> CPT.TDistinct }
  @all                              { \_ -> CPT.TAll }
  @star                             { \_ -> CPT.TStar }
  @as                               { \_ -> CPT.TAs }

  -- type keywords
  @csBool                           { \_ -> CPT.TCsBool }
  @csText                           { \_ -> CPT.TCsText }
  @csNumber                         { \_ -> CPT.TCsNumber }

  -- expression blocks
  $operator+                        { \s -> CPT.TOperator s }

  -- numeric inputs
  $digit+                           { \s -> CPT.TNumber (read s) }

  -- textual inputs
  @textual                          { \s -> CPT.TTerm s }
  \' @spaceverbal \'                { \s -> CPT.TSingleQuoted s }
  \" @spaceverbal \"                { \s -> CPT.TDoubleQuoted s }


{
-- Each action has type :: String -> Token

-- | Tokenizes a string.
tokenize :: String -> [CPT.Token]
tokenize = alexScanTokens
}