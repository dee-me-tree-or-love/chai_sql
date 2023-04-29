{- | The Type Checker (Inference) for a Micro SQL fragment.



-}
module ChaiMicroSql.TypeChecker (
        inferVar,
        __varError,
        inferTotalRecord,
        inferAttributeReference,
        __baseNotRecordError
    ) where

import qualified ChaiMicroSql.AST         as AST
import qualified ChaiMicroSql.TAST        as TAST
import qualified ChaiMicroSql.TypeContext as TCX

-- Type Inference Procedures
-- ~~~~~~~~~~~~~~~~~~~~~~~~~

newtype TCInferenceError = TCInferenceError String deriving (Show, Eq)

-- Axioms
-- ^^^^^^

-- | Variable type inference.(*)
--
-- - Note: Corresponds to the @Axiom A1@.
--
-- Examples
--
-- >>> inferVar (TCX.freshContext) (AST.ASTVariable "foo")
-- Left (TCInferenceError "Could not infer variable type. Variable `foo` is not in context.")
-- >>> let k = TCX.TCSimpleTypeContextKey "foo"
-- >>> let v = TAST.TASTSimpleTypeBasic TAST.TASTSimpleTypeBasicBool
-- >>> inferVar (TCX.extend k v TCX.freshContext) (AST.ASTVariable "foo")
-- Right (TASTSimpleTypeBasic TASTSimpleTypeBasicBool)
--
inferVar :: TCX.TCSimpleTypeContext -> AST.ASTVariable -> Either TCInferenceError TAST.TASTSimpleType
inferVar c (AST.ASTVariable v) = do
    let i = TCX.get (TCX.TCSimpleTypeContextKey v) c
    case i of
        Just t  -> Right t
        Nothing -> Left $ __varError v

-- | A utility to construct a variable inference error message.
__varError :: String -> TCInferenceError
__varError v = TCInferenceError $ "Could not infer variable type. Variable `" ++ v ++ "` is not in context."


-- | Total record type inference.(*)
--
-- - Note: Corresponds to the @Axiom A2@.
--
inferTotalRecord :: TCX.TCSimpleTypeContext -> AST.ASTSelectAttributeStarTotalRecord -> Either TCInferenceError TAST.TASTSimpleType
inferTotalRecord _ _ = Right $ TAST.TASTSimpleTypeRecord TAST.TASTSimpleTypeRecordTotal


-- Rules
-- ^^^^^

-- | Attribute reference inference.
inferAttributeReference :: TCX.TCSimpleTypeContext -> AST.ASTSelectAttributeReference -> Either TCInferenceError TAST.TASTSimpleType
inferAttributeReference c (AST.ASTSelectAttributeReferenceUnqualified v) = do
    at <- inferVar c v
    let k = TAST.TASTSimpleTypeBasicIndexKey $ AST.toString v
    let i = TAST.TASTSimpleTypeBasicIndexKeyValue k at
    Right $ TAST.TASTSimpleTypeBasic $ TAST.TASTSimpleTypeBasicIndex i
inferAttributeReference c (AST.ASTSelectAttributeReferenceQualified b v) = do
    bt <- inferVar c b
    case bt of
        TAST.TASTSimpleTypeBasic t  -> Left $ __baseNotRecordError b v t
        TAST.TASTSimpleTypeRecord r -> undefined

__baseNotRecordError :: AST.ASTVariable -> AST.ASTVariable -> TAST.TASTSimpleTypeBasic -> TCInferenceError
__baseNotRecordError b v t = TCInferenceError $ "Could not infer the type of access `" ++ AST.toString b ++ "." ++ AST.toString v ++ "`. Variable `" ++ AST.toString b ++ "` is a `" ++ show t ++ "` type and is not a Record."
