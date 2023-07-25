{
module ChaiMicroSql.Parsing.Parser (parse) where

import qualified ChaiMicroSql.AST            as AST
import qualified ChaiMicroSql.TAST            as TAST
import qualified ChaiMicroSql.Parsing.Tokens as CPT
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

    csBool      { CPT.TCsBool }
    csText      { CPT.TCsText }
    csNumber    { CPT.TCsNumber }

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

StackSqlSelectQueries   :: { [AST.AstSelectQuery] }
StackSqlSelectQueries   : SqlSelectQuery                            { [$1] }
                        | StackSqlSelectQueries SqlSelectQuery      { $2 : $1 }

SqlSelectQuery :: { AST.AstSelectQuery }
SqlSelectQuery  : SqlSelectQuery ';'                                                        { $1 }
                | SqlSelectQueryTypeHint select StackAttributeAccess                        { AST.AstSelectQuery $1 $3 [] }
                | SqlSelectQueryTypeHint select StackAttributeAccess from StackFromAccess   { AST.AstSelectQuery $1 $3 $5 }

SqlSelectQueryTypeHint :: { Maybe TAST.TAstDbView }
SqlSelectQueryTypeHint  : '-- @cs' ':' ':' '{' TypeHintAttributes '}'  { pure $5 }
                        |                                                           { mempty }

TypeHintAttributes  :: { [TAST.TAstSimpleAtomicIndexPair] }
TypeHintAttributes  : TypeHintAttributePair                         { [$1] }
                    | TypeHintAttributes ',' TypeHintAttributePair  { $3 : $1 }

TypeHintAttributePair   :: { TAST.TAstSimpleAtomicIndexPair }
TypeHintAttributePair   : TypeHintAttributeKey ':' TypeHintAttributeType    { TAST.TAstSimpleAtomicIndexKeyValue $1 $3 }

TypeHintAttributeKey    :: { TAST.TAstSimpleIndexKey }
TypeHintAttributeKey    : term { TAST.makeKey $1 }

TypeHintAttributeType   :: { TAST.TAstAtomicType }
                        : csBool    { TAST.TAstAtomicTypeBool }
                        | csText    { TAST.TAstAtomicTypeText }
                        | csNumber  { TAST.TAstAtomicTypeNumber }

StackAttributeAccess    :: { [AST.AstSelectAttributeAccess] }
StackAttributeAccess    : AttributeAccess                           { [$1] }
                        | StackAttributeAccess ',' AttributeAccess  { $3 : $1 }

-- TODO: extend to support constants
AttributeAccess     :: { AST.AstSelectAttributeAccess }
AttributeAccess     : AttributeAccessTerm           { AST.AstSelectAttributeAccessReference $1 }
                    | AttributeAccessTerm Alias     { AST.AstSelectAttributeAccessReferenceAlias $1 $2 }
                    | star                          { AST.AstSelectAttributeAccessStar AST.AstSelectAttributeStarTotalRecord }

AttributeAccessTerm :: { AST.AstSelectAttributeReference }
AttributeAccessTerm : Variable                      { AST.AstSelectAttributeReferenceUnqualified $1 }
                    | Variable '.' Variable         { AST.AstSelectAttributeReferenceQualified $1 $3 }

StackFromAccess     :: { [AST.AstFromAccess] }
StackFromAccess     : FromAccess                        { [$1] }
                    | StackFromAccess ',' FromAccess    { $3 : $1 }

FromAccess  :: { AST.AstFromAccess }
FromAccess  : Variable                      { AST.AstFromAccessReference $1 }
            | Variable Alias                { AST.AstFromAccessReferenceAlias $1 $2 }
            | '(' SqlSelectQuery ')' Alias  { AST.AstFromAccessNestedQueryAlias $2 $4 } 

Variable    :: { AST.AstVariable }
Variable    : term { AST.AstVariable $1 }

Alias       :: { AST.AstSimpleAlias }
Alias       : as term { AST.AstSimpleAlias $2 }

{
parseError :: [CPT.Token] -> a
parseError ts = error $ "Parse error: " ++ (show ts)
}