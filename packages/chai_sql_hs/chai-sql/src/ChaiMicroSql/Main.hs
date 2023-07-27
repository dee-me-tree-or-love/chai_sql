module ChaiMicroSql.Main (
        loadYamlContext,
        loadsYamlContext,
        parseSql,
        inferSqlType,
        checkSqlType
    ) where

import qualified ChaiMicroSql.AST                       as AST
import qualified ChaiMicroSql.Loaders.YamlContextLoader as YCL
import qualified ChaiMicroSql.Parsing.Lexer             as CPL
import qualified ChaiMicroSql.Parsing.Parser            as CPP
import qualified ChaiMicroSql.TAST                      as TAST
import qualified ChaiMicroSql.TypeChecker               as TC
import qualified ChaiMicroSql.TypeContext               as TCX
import           Data.ByteString                        (ByteString)

loadYamlContext :: ByteString -> Either YCL.YclLoaderError TCX.TCXSimpleTypeContext
loadYamlContext = YCL.load

loadsYamlContext :: String -> Either YCL.YclLoaderError TCX.TCXSimpleTypeContext
loadsYamlContext = YCL.loads

parseSql :: String -> [AST.AstSelectQuery]
parseSql = CPP.parse . CPL.tokenize

inferSqlType :: TCX.TCXSimpleTypeContext -> AST.AstSelectQuery -> TC.TcInferenceResult TAST.TAstDbView
inferSqlType = TC.infer

checkSqlType :: TCX.TCXSimpleTypeContext -> AST.AstSelectQuery -> TC.TcCheckResult
checkSqlType c = TC.checkingResult . (TC.annotate c :: AST.AstSelectQuery -> TC.TcInferenceWrapper TAST.TAstDbView AST.AstSelectQuery)
