module Simple.TypeChecker (annotate) where

import qualified Simple.Ast as SAST

-- Default types
-- ~~~~~~~~~~~~~

textTypeHint :: SAST.TypeHint
textTypeHint = SAST.STypeHint "Text"

numberTypeHint :: SAST.TypeHint
numberTypeHint = SAST.STypeHint "Number"

-- TODO: Replace with Nothing
failTypeHint :: SAST.TypeHint
failTypeHint = SAST.STypeHint "FAIL"


-- Type checking
-- ~~~~~~~~~~~~~

annotate :: SAST.TypedExpression -> SAST.TypedExpression
annotate (SAST.STypedExpression x y) = SAST.STypedExpression x y
annotate (SAST.SUntypedExpression x) = SAST.STypedExpression t x
    where t = infer x

infer :: SAST.Expression -> SAST.TypeHint
infer (SAST.STerm (SAST.SText _)) = textTypeHint
infer (SAST.STerm (SAST.SNumber _)) = numberTypeHint
infer (SAST.STerm (SAST.SExpressionContainer e)) = infer e
infer (SAST.SUnExpression _ e) = infer e
infer (SAST.SBinExpression o e1 e2) = solve o (infer e1) (infer e2)

-- TODO: wrap it in Maybe's
solve :: SAST.Operator -> SAST.TypeHint -> SAST.TypeHint -> SAST.TypeHint
solve _ t1 t2
    | t1 == t2 = t1
    | otherwise  = failTypeHint