{
module ChaiMicroSql.Parsing.Parser (parse) where

import qualified ChaiMicroSql.Parsing.Tokens as CPT
import qualified ChaiMicroSql.AST as SAST
}

%name parse
%tokentype{CPT.Token}
%error {parseError}

-- Token bindings
%token
    select      { CPT.TSelect }
    distinct    { CPT.TDistinct }
    all         { CPT.TAll }
    from        { CPT.TFrom }
    star        { CPT.TStar }
    as          { CPT.TAs }

    ':'         { CPT.TColon }
    ';'         { CPT.TSemicolon }
    ','         { CPT.TComma }
    '.'         { CPT.TDot }
    '('         { CPT.TLeftBrace }
    ')'         { CPT.TRightBrace }
    '<'         { CPT.TLeftAngle }
    '>'         { CPT.TRightAngle }
    '{'         { CPT.TLeftCurl }
    '}'         { CPT.TRightCurl }

    '--'        { CPT.TComment }
    '-- @cs'    { CPT.TChaiComment }

    operator    { CPT.TOperator $$ }
    term        { CPT.TTerm $$ }
    number      { CPT.TNumber $$ }
    sqstring    { CPT.TSingleQuoted $$ }
    dqstring    { CPT.TDoubleQuoted $$ }

-- Grammar rules
-- ~~~~~~~~~~~~~
%%

-- Top Level: SQL Statements
-- +++++++++++++++++++++++++

StackSqlSelectQueries   :: { [SAST.AstSelectQuery] }
StackSqlSelectQueries   : SqlSelectQuery                            { [$1] }
                        | StackSqlSelectQueries SqlSelectQuery      { $2 : $1 }

SqlSelectQuery :: { SAST.AstSelectQuery }
SqlSelectQuery  : SqlSelectQuery ';'                                { $1 }
                | select StackAttributeAccess                       { SAST.AstSelectQuery mempty $2 [] }
                | select StackAttributeAccess from StackFromAccess  { SAST.AstSelectQuery mempty $2 $4 }

StackAttributeAccess    :: { [SAST.AstSelectAttributeAccess] }
StackAttributeAccess    : AttributeAccess                           { [$1] }
                        | StackAttributeAccess ',' AttributeAccess  { $3 : $1 }

-- TODO: extend to support aliases and constants
AttributeAccess     :: { SAST.AstSelectAttributeAccess }
AttributeAccess     : Variable                  { SAST.AstSelectAttributeAccessReference $ SAST.AstSelectAttributeReferenceUnqualified $1 }
                    | Variable '.' Variable     { SAST.AstSelectAttributeAccessReference $ SAST.AstSelectAttributeReferenceQualified $1 $3 }
                    | star                      { SAST.AstSelectAttributeAccessStar SAST.AstSelectAttributeStarTotalRecord }


StackFromAccess     :: { [SAST.AstFromAccess] }
StackFromAccess     : FromAccess                        { [$1] }
                    | StackFromAccess ',' FromAccess    { $3 : $1 }

-- TODO: extend to support aliases and sub-queries
FromAccess  :: { SAST.AstFromAccess }
FromAccess  : Variable  { SAST.AstFromAccessReference $1 }

Variable    :: { SAST.AstVariable }
Variable    : term { SAST.AstVariable $1 }

{
parseError :: [CPT.Token] -> a
parseError ts = error $ "Parse error: " ++ (show ts)
}