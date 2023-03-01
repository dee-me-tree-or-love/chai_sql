{
module Simple.Parser (parse) where
import qualified Simple.Tokens as ST
import qualified Simple.Ast as SAST
import qualified Simple.Lexer as SL
}

%name parse
%tokentype{ST.Token}
%error {parseError}

-- Token bindings
%token

    number  { ST.TNumber $$ }
    text    { ST.TText $$ }
    op      { ST.TOperator $$ }
    '('     { ST.TLeftBrace }
    ')'     { ST.TRightBrace }
    thi     { ST.TTypeHintIndicator }

-- Grammar rules
%%

TExp    : thi TH Exp    { SAST.STypedExpression $2 $3 }
        | Exp           { SAST.SUntypedExpression $1 }

TH      : text          { SAST.STypeHint $1 }

Exp     : Op Exp Exp    { SAST.SBinExpression $1 $2 $3 }
        | Op Exp        { SAST.SUnExpression $1 $2 }
        | Term          { SAST.STerm $1 }

Op      : op            { SAST.SOperator $1 }

Term    : number        { SAST.SNumber $1 }
        | text          { SAST.SText $1 }
        | '(' Exp ')'   { SAST.SExpressionContainer $2 }

{
parseError :: [ST.Token] -> a
parseError ts = error $ "Parse error: " ++ (show ts)
}