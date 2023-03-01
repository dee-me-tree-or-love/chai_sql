module Simple.TypeChecker (annotate, check) where

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


-- Type inference
-- ~~~~~~~~~~~~~~

-- TODO: make use of Functors

annotate :: SAST.TypedExpression -> SAST.TypedExpression
annotate (SAST.SUntypedExpression x) = SAST.STypedExpression t x
    where t = infer x
annotate x = x

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

-- Type checking
-- ~~~~~~~~~~~~~

check :: SAST.TypedExpression -> Either String String
check (SAST.STypedExpression t e)
    | t == it && (t /= failTypeHint) && (it /= failTypeHint) = Right "Okay"
    | otherwise = Left ("Inferred type: " ++ show it ++ ", doesn't match specified: " ++ show t)
    where it = infer e
check (SAST.SUntypedExpression e)
    | failTypeHint == it = Left ("Inferred type: " ++ show it)
    | otherwise = Right "Okay"
    where it = infer e