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
StackSqlStatement   :: { LAST.StackSqlStatement LAST.NaiveMaybeTypedAstContext }
StackSqlStatement   : SqlStatement                          { [$1] }
                    | StackSqlStatement SqlStatement        { $2 : $1 }

SqlStatement    :: { LAST.SqlStatement LAST.NaiveMaybeTypedAstContext }
SqlStatement    : SelectStatement ';'       { LAST.SSelectStatement emptyContext $1 }
                | SelectStatement           { LAST.SSelectStatement emptyContext $1 }
                | SqlComment                { LAST.SSqlComment emptyContext $1 }


-- Mid Level: SQL Comments
-- +++++++++++++++++++++++

SqlComment      :: { LAST.SqlComment LAST.NaiveMaybeTypedAstContext }
SqlComment      : '--'                 { LAST.SCommentPlain emptyContext }
                | ChaiStatement        { LAST.SCommentChai emptyContext $1 }

ChaiStatement       :: { LAST.ChaiStatement LAST.NaiveMaybeTypedAstContext }
ChaiStatement       : '-- @cs' ':' Term                                                     { LAST.SChaiTrigger emptyContext $3 }
                    | '-- @cs' ':' Term Term Operator Term                                  { LAST.SChaiExpression emptyContext $3 $4 $5 $6 }
                    | '-- @cs' ':' Term Term '<' Term '>' '{' StackChaiAttributePair '}'    { LAST.SChaiCompound emptyContext $3 $4 $6 $9 }

StackChaiAttributePair      :: { LAST.StackChaiAttributePair LAST.NaiveMaybeTypedAstContext }
StackChaiAttributePair      : ChaiAttributePair                            { [$1] }
                            | StackChaiAttributePair ',' ChaiAttributePair      { $3 : $1 }

ChaiAttributePair       :: { LAST.ChaiAttributePair LAST.NaiveMaybeTypedAstContext }
ChaiAttributePair       : Term ':' Term         { LAST.SChaiAttributePair emptyContext $1 $3 }

-- Mid Level: SELECT Statements
-- ++++++++++++++++++++++++++++

SelectStatement     :: { LAST.SelectStatement LAST.NaiveMaybeTypedAstContext }
SelectStatement     : select MaybeSelectOption StackSelectAccess                        { LAST.SSelectStatementAtom emptyContext $2 $3 }
                    | select MaybeSelectOption StackSelectAccess SelectFrom             { LAST.SSelectStatementWithFrom emptyContext $2 $3 $4 }
                    -- TODO(feature): support from SELECT-FROM-WHERE
                    -- | select MaybeSelectOption StackSelectAccess SelectFrom SelectWhere { LAST.SSelectStatementWithFromWhere emptyContext $2 $3 $4 $5 }

MaybeSelectOption   :: { LAST.MaybeSelectOption LAST.NaiveMaybeTypedAstContext }
MaybeSelectOption   : distinct    { Just \$ LAST.SSelectDistinct emptyContext }
                    | all         { Just \$ LAST.SSelectAll emptyContext }
                    | {- empty -} { Nothing }

StackSelectAccess   :: { LAST.StackSelectAccess LAST.NaiveMaybeTypedAstContext }
StackSelectAccess   : SelectAccess                              { [$1] }
                    | StackSelectAccess ',' SelectAccess        { $3 : $1 }

SelectAccess    :: { LAST.SelectAccess LAST.NaiveMaybeTypedAstContext }
SelectAccess    : Term              { LAST.SSelectAccessColumn emptyContext $1 }
                | Term '.' Term     { LAST.SSelectAccessColumnQualified emptyContext $1 $1 }
                | Constant          { LAST.SSelectAccessConstant emptyContext $1 }
                | star              { LAST.SSelectAccessStar emptyContext }

SelectFrom      :: {LAST.SelectFrom LAST.NaiveMaybeTypedAstContext }
SelectFrom      : from Term                         { LAST.SSelectFromTable emptyContext $2 }
                | from '(' StackSqlStatement ')'    { LAST.SSelectFromStatements emptyContext $3 }


-- Low Level: TERMS, CONSTANTS
-- +++++++++++++++++++++++++++

Operator    :: { LAST.Operator LAST.NaiveMaybeTypedAstContext }
Operator    : operator      { LAST.SOperator emptyContext $1 }

Term        :: { LAST.Term LAST.NaiveMaybeTypedAstContext }
Term        : term          { LAST.STerm emptyContext $1 }

Constant    :: { LAST.Constant LAST.NaiveMaybeTypedAstContext }
Constant    : number        { LAST.SNumberConstant emptyContext $1 }
            | sqstring      { LAST.SSingleQuotedTextConstant emptyContext $1 }
            | dqstring      { LAST.SDoubleQuotedTextConstant emptyContext $1 }


{
emptyContext :: LAST.NaiveMaybeTypedAstContext
emptyContext = LAST.AstContext {LAST.lineNumber = 0, LAST.columnNumber = 0, LAST.typeInfo = Nothing}

parseError :: [LT.Token] -> a
parseError ts = error $ "Parse error: " ++ (show ts)
}