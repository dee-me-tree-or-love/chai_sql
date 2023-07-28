{-# LANGUAGE DeriveDataTypeable #-}
module CLI.Main (main) where


import qualified ChaiMicroSql.Main      as CSM
import           System.Console.CmdArgs (Data, Default (def), Typeable, cmdArgs,
                                         help, modes, typ, typFile, (&=))


data ChaiSql
    = Infer {expression :: Maybe String, path :: Maybe FilePath, schema :: FilePath}
    | Check {expression :: Maybe String, path :: Maybe FilePath, schema :: FilePath}
     deriving (Show, Data, Typeable)

expressionHint :: String
expressionHint = "SQL expression to evaluate"

pathHint :: String
pathHint = "SQL file to evaluate"

schemaHint :: String
schemaHint = "Database schema to use for evaluation"


infer :: ChaiSql
infer = Infer {
        expression = def &= help expressionHint &= typ "SQL",
        path = def &= help pathHint &= typFile,
        schema = def &= help schemaHint &= typFile
    }


check :: ChaiSql
check = Check {
        expression = def &= help expressionHint &= typ "SQL",
        path = def &= help pathHint &= typFile,
        schema = def &= help schemaHint &= typFile
    }

-- CLI features:
-- - [x] Basic working
-- - [x] Naive output
-- - [ ] Better (monadic) working
-- - [ ] Prettier output
-- - [ ] Exit codes

handleInferExpression :: String -> String -> IO ()
handleInferExpression eIn cIn = do
    -- TODO(tech-debt): make a monadic parser
    let sql = head $ CSM.parseSql eIn
    case CSM.loadsYamlContext cIn of
          Left err -> error $ show err
          Right ctx -> do
            let inferredType = CSM.inferSqlType ctx sql
            -- TODO(tech-debt): make a beautiful formatting of the inferred type output/error
            print inferredType

handleCheckExpression :: String -> String -> IO ()
handleCheckExpression eIn cIn = do
    -- TODO(tech-debt): make a monadic parser
    let sql = head $ CSM.parseSql eIn
    case CSM.loadsYamlContext cIn of
          Left err -> error $ show err
          Right ctx -> do
            let checkResult = CSM.checkSqlType ctx sql
            -- TODO(tech-debt): make a beautiful formatting of the type checking output/error
            print checkResult

handle :: ChaiSql -> IO ()
-- Type inference
handle Infer {expression = Just eIn, path = Nothing, schema = sIn} = do
    cIn <- readFile sIn
    handleInferExpression eIn cIn
handle Infer {expression = Nothing, path = Just pIn, schema = sIn} = do
    cIn <- readFile sIn
    eIn <- readFile pIn
    handleInferExpression eIn cIn
-- Type checking
handle Check {expression = Just eIn, path = Nothing, schema = sIn} = do
    cIn <- readFile sIn
    handleCheckExpression eIn cIn
handle Check {expression = Nothing, path = Just pIn, schema = sIn} = do
    cIn <- readFile sIn
    eIn <- readFile pIn
    handleCheckExpression eIn cIn

-- Incorrect inference inputs
handle Infer {expression = Just "", path = _, schema = _} = error "Provided SQL `expression` can not be empty."
handle Infer {expression = _, path = Just "", schema = _} = error "Provided SQL file `path` can not be empty."
handle Infer {expression = _, path = _, schema = ""} = error "Path to the schema file can not be empty."
handle Infer {expression = _, path = _, schema = _} = error "Either `expression` or `path` to a SQL file must be provided."
-- Incorrect inference inputs
handle Check {expression = Just "", path = _, schema = _} = error "Provided SQL `expression` can not be empty."
handle Check {expression = _, path = Just "", schema = _} = error "Provided SQL file `path` can not be empty."
handle Check {expression = _, path = _, schema = ""} = error "Path to the schema file can not be empty."
handle Check {expression = _, path = _, schema = _} = error "Either `expression` or `path` to a SQL file must be provided."

    putStrLn "🤔 I think I am still a work in progress."

main :: IO ()
main = handle =<< cmdArgs (modes [infer,check])
