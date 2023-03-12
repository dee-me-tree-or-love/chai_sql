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
    number  { LT.TNumber $$ }
    text    { LT.TText $$ }

-- Grammar rules
%%

Term    : number         { LAST.SNumber $1 }
        | text           { LAST.SText $1 }


{
parseError :: [LT.Token] -> a
parseError ts = error $ "Parse error: " ++ (show ts)
}