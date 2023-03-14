{
module Language.Parser (parse) where
import qualified Language.Tokens as LT
import qualified Language.Ast as LAST
import qualified Language.Lexer as LL
}

%name parse
%tokentype{LT.Token}
%error {parseError}

-- Token bindings
%token
    select      { LT.TSelect }
    distinct    { LT.TDistinct }
    all         { LT.TAll }
    from        { LT.TFrom }
    star        { LT.TStar }

    ':'         { LT.TColon }
    ';'         { LT.TSemicolon }
    ','         { LT.TComma }
    '.'         { LT.TDot }
    '('         { LT.TLeftBrace }
    ')'         { LT.TRightBrace }
    '<'         { LT.TLeftAngle }
    '>'         { LT.TRightAngle }
    '{'         { LT.TLeftCurl }
    '}'         { LT.TRightCurl }

    '--'        { LT.TComment }
    '-- @cs'    { LT.TChaiComment }

    operator    { LT.TOperator $$ }
    term        { LT.TTerm $$ }
    number      { LT.TNumber $$ }
    sqstring    { LT.TSingleQuoted $$ }
    dqstring    { LT.TDoubleQuoted $$ }

-- Grammar rules
-- ~~~~~~~~~~~~~
%%

-- Top Level: SQL Statements
-- +++++++++++++++++++++++++

-- NB: Using left recursion for efficiency, but thus the parsed list is reverse
-- See: https://haskell-happy.readthedocs.io/en/latest/using.html#parsing-sequences
StackSqlStatement   :: { LAST.StackSqlStatement }
StackSqlStatement   : SqlStatement                          { [$1] }
                    | StackSqlStatement SqlStatement        { $2 : $1 }

SqlStatement    :: { LAST.SqlStatement }
SqlStatement    : SelectStatement ';'       { LAST.SSelectStatement $1 }
                | SelectStatement           { LAST.SSelectStatement $1 }
                | SqlComment                { LAST.SSqlComment $1 }


-- Mid Level: SQL Comments
-- +++++++++++++++++++++++

SqlComment      :: { LAST.SqlComment }
SqlComment      : '--'                 { LAST.SCommentPlain }
                | ChaiStatement        { LAST.SCommentChai $1 }

ChaiStatement       :: { LAST.ChaiStatement }
ChaiStatement       : '-- @cs' ':' Term                                                     { LAST.SChaiTrigger $3 }
                    | '-- @cs' ':' Term Term Operator Term                                  { LAST.SChaiExpression $3 $4 $5 $6 }
                    | '-- @cs' ':' Term Term '<' Term '>' '{' StackChaiAttributePair '}'    { LAST.SChaiCompound $3 $4 $6 $9 }

StackChaiAttributePair      :: { LAST.StackChaiAttributePair }
StackChaiAttributePair      : ChaiAttributePair                            { [$1] }
                            | StackChaiAttributePair ',' ChaiAttributePair      { $3 : $1 }

ChaiAttributePair       :: { LAST.ChaiAttributePair }
ChaiAttributePair       : Term ':' Term         { LAST.SChaiAttributePair $1 $3 }

-- Mid Level: SELECT Statements
-- ++++++++++++++++++++++++++++

SelectStatement     :: { LAST.SelectStatement }
SelectStatement     : select MaybeSelectOption StackSelectAccess                        { LAST.SSelectStatementAtom $2 $3 }
                    | select MaybeSelectOption StackSelectAccess SelectFrom             { LAST.SSelectStatementWithFrom $2 $3 $4 }
                    -- TODO(feature): support from SELECT-FROM-WHERE
                    -- | select MaybeSelectOption StackSelectAccess SelectFrom SelectWhere { LAST.SSelectStatementWithFromWhere $2 $3 $4 $5 }

MaybeSelectOption   :: { LAST.MaybeSelectOption }
MaybeSelectOption   : distinct    { Just LAST.SSelectDistinct }
                    | all         { Just LAST.SSelectAll }
                    | {- empty -} { Nothing }

StackSelectAccess   :: { LAST.StackSelectAccess }
StackSelectAccess   : SelectAccess                              { [$1] }
                    | StackSelectAccess ',' SelectAccess        { $3 : $1 }

SelectAccess    :: { LAST.SelectAccess }
SelectAccess    : Term              { LAST.SSelectAccessColumn $1 }
                | Term '.' Term     { LAST.SSelectAccessColumnQualified $1 $1 }
                | Constant          { LAST.SSelectAccessConstant $1 }
                | star              { LAST.SSelectAccessStar }

SelectFrom      :: {LAST.SelectFrom }
SelectFrom      : from Term                         { LAST.SSelectFromTable $2 }
                | from '(' StackSqlStatement ')'    { LAST.SSelectFromStatements $3 }


-- Low Level: TERMS, CONSTANTS
-- +++++++++++++++++++++++++++

Operator    :: { LAST.Operator }
Operator    : operator      { LAST.SOperator $1 }

Term        :: { LAST.Term }
Term        : term          { LAST.STerm $1 }

Constant    :: { LAST.Constant }
Constant    : number        { LAST.SNumberConstant $1 }
            | sqstring      { LAST.SSingleQuotedTextConstant $1 }
            | dqstring      { LAST.SDoubleQuotedTextConstant $1 }


{
parseError :: [LT.Token] -> a
parseError ts = error $ "Parse error: " ++ (show ts)
}