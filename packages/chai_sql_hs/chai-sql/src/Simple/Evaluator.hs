module Simple.Evaluator (eval, EvalOutput) where

import qualified Simple.Ast as SAST

-- TODO: make this a functor
data EvalResult =
    NumberResult Int
    | TextResult String
    | ErrorResult String
    deriving (Eq, Show)

type EvalOutput = Either String EvalResult

eval :: SAST.TypedExpression -> EvalOutput
eval (SAST.STypedExpression _ x) = compute x
eval (SAST.SUntypedExpression x) = compute x

-- Computing the expressions
-- ~~~~~~~~~~~~~~~~~~~~~~~~~

compute :: SAST.Expression -> EvalOutput
-- leaf
compute (SAST.STerm t) = get t
-- unary expression
compute (SAST.SUnExpression o e) = sApply o $ compute e
-- binary expression
compute (SAST.SBinExpression o e1 e2) = bApply o (compute e1) (compute e2)

get :: SAST.Term -> EvalOutput
get (SAST.SNumber n) = Right $ NumberResult n
get (SAST.SText t) = Right $ TextResult t
get (SAST.SExpressionContainer e) = compute e

sApply :: SAST.Operator -> EvalOutput -> EvalOutput
sApply (SAST.SOperator '+') v = v
sApply (SAST.SOperator o) v = Left  $ "Unsupported operator: " ++ [o] ++ ", for input: " ++ show v

bApply :: SAST.Operator -> EvalOutput -> EvalOutput -> EvalOutput
-- Numbers
bApply (SAST.SOperator '+') (Right (NumberResult x))  (Right (NumberResult y)) = Right $ NumberResult $ x + y
bApply (SAST.SOperator '-') (Right (NumberResult x))  (Right (NumberResult y)) = Right $ NumberResult $ x - y
bApply (SAST.SOperator '*') (Right (NumberResult x))  (Right (NumberResult y)) = Right $ NumberResult $ x * y
-- Text
bApply (SAST.SOperator '+') (Right (TextResult x))  (Right (TextResult y)) = Right $ TextResult $ x ++ y
-- No runtime support
bApply (SAST.SOperator o) v1 v2 = Left $ "Unsupported operator: " ++ [o] ++ ", for inputs: " ++ show v1 ++ ", and " ++ show v2