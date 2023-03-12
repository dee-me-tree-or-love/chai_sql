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

    ';'         { LT.TSemicolon }
    ','         { LT.TComma }
    '.'         { LT.TDot }
    '('         { LT.TLeftBrace }
    ')'         { LT.TRightBrace }

    term        { LT.TTerm $$ }
    number      { LT.TNumber $$ }
    sqstring    { LT.TSingleQuoted $$ }
    dqstring    { LT.TDoubleQuoted $$ }

-- Grammar rules
%%

-- NB: Using left recursion for efficiency, but thus the parsed list is reverse
-- See: https://haskell-happy.readthedocs.io/en/latest/using.html#parsing-sequences
StackSqlStatement   :: { LAST.StackSqlStatement }
StackSqlStatement   : SqlStatement                              { [$1] }
                    | StackSqlStatement ';'                  { $1 }
                    | StackSqlStatement ';' SqlStatement     { $3 : $1 }

SqlStatement    :: { LAST.SqlStatement }
SqlStatement    : SelectStatement { LAST.SSelectStatement $1 }

SelectStatement     :: { LAST.SelectStatement }
SelectStatement     : select MaybeSelectOption StackSelectAccess     { LAST.SSelectStatementAtom $2 $3 }

MaybeSelectOption   :: { LAST.MaybeSelectOption }
MaybeSelectOption   : distinct    { Just LAST.SSelectDistinct }
                    | all         { Just LAST.SSelectAll }
                    | {- empty -} { Nothing }

StackSelectAccess   :: { LAST.StackSelectAccess }
StackSelectAccess   : SelectAccess                          { [$1] }
                    | StackSelectAccess ',' SelectAccess { $3 : $1 }

SelectAccess    :: { LAST.SelectAccess }
SelectAccess    : Term                      { LAST.SSelectAccessColumn $1 }
                | Term '.' Term             { LAST.SSelectAccessColumnQualified $1 $1 }
                | Constant                  { LAST.SSelectAccessConstant $1 }
                | star                      { LAST.SSelectAccessStar }

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