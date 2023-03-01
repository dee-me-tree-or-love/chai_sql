module Simple.Main (main) where

import qualified Simple.Lexer as SL ( scan )
import qualified Simple.Tokens as ST ( Token )
import qualified Simple.Parser as SP ( parse )
import qualified Simple.Ast as SAST ( TypedExpression )
import qualified Simple.TypeChecker as STC ( annotate, check )
import qualified Simple.Evaluator as SE (eval, EvalOutput)


-- | Tokenizes a string.
--
-- Examples:
--
-- >>> getTokens "(+ 2 1)"
-- [TLeftBrace,TOperator '+',TNumber 2,TNumber 1,TRightBrace]
--
-- >>> getTokens "(+ cat 1)"
-- [TLeftBrace,TOperator '+',TText "cat",TNumber 1,TRightBrace]
--
-- >>> getTokens "-- this is a comment"
-- []
--
-- >>> getTokens "~~ TypeHint"
-- [TTypeHintIndicator,TText "TypeHint"]
--
-- >>> getTokens "%%%%% this causes an error"
-- lexical error
getTokens :: String -> [ST.Token]
getTokens = SL.scan


-- | Parses the tokenized input into an AST
--
-- >>> getAst $ getTokens "(+ 1 2)"
-- SUntypedExpression (STerm (SExpressionContainer (SBinExpression (SOperator '+') (STerm (SNumber 1)) (STerm (SNumber 2)))))
--
-- >>> getAst $ getTokens "(+ 1)"
-- SUntypedExpression (STerm (SExpressionContainer (SUnExpression (SOperator '+') (STerm (SNumber 1)))))
--
-- >>> getAst $ getTokens "(+ (+ 1 2))"
-- SUntypedExpression (STerm (SExpressionContainer (SUnExpression (SOperator '+') (STerm (SExpressionContainer (SBinExpression (SOperator '+') (STerm (SNumber 1)) (STerm (SNumber 2))))))))
--
-- >>> getAst $ getTokens "~~ Number (+ (+ 1 2))"
-- STypedExpression (STypeHint "Number") (STerm (SExpressionContainer (SUnExpression (SOperator '+') (STerm (SExpressionContainer (SBinExpression (SOperator '+') (STerm (SNumber 1)) (STerm (SNumber 2))))))))
--
getAst :: [ST.Token] -> SAST.TypedExpression
getAst = SP.parse


-- | Assigns expression types.
--
-- >>> assignTypes $ getAst $ getTokens "~~ Number (+ (+ 1 2))"
-- STypedExpression (STypeHint "Number") (STerm (SExpressionContainer (SUnExpression (SOperator '+') (STerm (SExpressionContainer (SBinExpression (SOperator '+') (STerm (SNumber 1)) (STerm (SNumber 2))))))))
--
-- >>> assignTypes $ getAst $ getTokens "(+ (+ 1 2))"
-- STypedExpression (STypeHint "Number") (STerm (SExpressionContainer (SUnExpression (SOperator '+') (STerm (SExpressionContainer (SBinExpression (SOperator '+') (STerm (SNumber 1)) (STerm (SNumber 2))))))))
--
-- >>> assignTypes $ getAst $ getTokens "(+ (+ cat mouse))"
-- STypedExpression (STypeHint "Text") (STerm (SExpressionContainer (SUnExpression (SOperator '+') (STerm (SExpressionContainer (SBinExpression (SOperator '+') (STerm (SText "cat")) (STerm (SText "mouse"))))))))
--
-- >>> assignTypes $ getAst $ getTokens "(+ (+ cat 2))"
-- STypedExpression (STypeHint "FAIL") (STerm (SExpressionContainer (SUnExpression (SOperator '+') (STerm (SExpressionContainer (SBinExpression (SOperator '+') (STerm (SText "cat")) (STerm (SNumber 2))))))))
--
assignTypes :: SAST.TypedExpression -> SAST.TypedExpression
assignTypes = STC.annotate

-- | Checks the correctness of the typed expression
--
-- >>> checkTypes $ getAst $ getTokens "~~ Number (+ (+ 1 2))"
-- Right "Okay"
--
-- >>> checkTypes $ getAst $ getTokens "(+ (+ 1 2))"
-- Right "Okay"
--
-- >>> checkTypes $ getAst $ getTokens "(+ (+ cat mouse))"
-- Right "Okay"
--
-- >>> checkTypes $ getAst $ getTokens "(+ (+ cat 2))"
-- Left "Inferred type: STypeHint \"FAIL\""
--

-- >>> checkTypes $ getAst $ getTokens "~~ String (+ (+ 1 2))"
-- Left "Inferred type: STypeHint \"Number\" doesn't match specified: STypeHint \"String\""
--
checkTypes :: SAST.TypedExpression -> Either String String
checkTypes = STC.check

-- | Evaluates the expression.
--
-- >>> evalExpression $ getAst $ getTokens "~~ Number (+ (+ 1 2))"
-- Right (NumberResult 3)
--
-- >>> evalExpression $ getAst $ getTokens "~~ Number (+ (- 2 5))"
-- Right (NumberResult (-3))

-- >>> evalExpression $ getAst $ getTokens "(+ (+ 1 2))"
-- Right (NumberResult 3)
--
-- >>> evalExpression $ getAst $ getTokens "(+ (+ cat mouse))"
-- Right (TextResult "catmouse")
--
-- >>> evalExpression $ getAst $ getTokens "~~ String (+ (+ 1 2))"
-- Right (NumberResult 3)
--
-- >>> evalExpression $ getAst $ getTokens "(+ (+ cat 2))"
-- Left "Unsupported operator: +, for inputs: Right (TextResult \"cat\"), and Right (NumberResult 2)"
--
evalExpression :: SAST.TypedExpression -> SE.EvalOutput
evalExpression = SE.eval

main :: IO ()
main = do
  s <- getContents
  let ast = getAst $ getTokens s
  print $ "\nInferred type:\t" ++ show (assignTypes ast)
  print $ "\nType check:\t" ++ show (checkTypes ast)
  print $ "\nEvaluation:\t" ++ show (evalExpression ast)
