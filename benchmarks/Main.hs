module Main where

import Control.Monad.IO.Class (liftIO)
import Criterion.Main
import Criterion.Types (Config (..), Verbosity (..))
-- import WLPVerifier (parseAndProcessGCL, setupZ3Env)

import qualified Data.Map as M
import FormulaProcessor (negateExpr, processAST, wlp)
import GCLParser.GCLDatatype
import GCLParser.Parser
import PreProcessor
import Z3.Base
import Z3.Monad (Z3, assert, check, evalZ3)
import Z3Solver (buildEnv, exprToZ3)

-- Array of programs to benchmark
benchmarkCases :: [FilePath]
benchmarkCases = ["examples/E.gcl", "examples/S1.gcl"]

-- Benchmark the `check` call
benchmarkCheck :: Expr -> [VarDeclaration] -> IO ()
benchmarkCheck processedWlp decls = evalZ3 $ do
  z3Expr <- setupZ3Env processedWlp decls
  assert z3Expr
  _ <- check
  return ()

main :: IO ()
main = do
  -- Prepare benchmarks for each file
  benchmarks <- mapM createBenchmark benchmarkCases

  -- Configure Criterion settings
  let config =
        defaultConfig
          { csvFile = Just "benchmark_results.csv",
            timeLimit = 5.0,
            verbosity = Verbose
          }

  -- Run the benchmarks with the specified config
  defaultMainWith config benchmarks

-- Create a benchmark for a single GCL file
createBenchmark :: FilePath -> IO Benchmark
createBenchmark filePath = do
  eitherWlp <- parseAndProcessGCL filePath
  case eitherWlp of
    Left err -> do
      putStrLn ("Error processing GCL file: " ++ err)
      return $ bench ("Error: " ++ filePath) $ nfIO (return ())
    Right (processedWlp, decls) ->
      return $ bench filePath $ nfIO (benchmarkCheck processedWlp decls)

-- | Parses and preprocesses a GCL file, then computes the WLP.
parseAndProcessGCL :: String -> IO (Either String (Expr, [VarDeclaration]))
parseAndProcessGCL file = do
  result <- parseGCLfile file
  case result of
    Left err -> return $ Left err
    Right program -> do
      let preprocessedProgram = preprocess program 10 True True True
      let processedWlp = wlp (stmt preprocessedProgram) (LitB True)
      let decls = input program -- Extract variable declarations
      return $ Right (processedWlp, decls)

-- | Sets up the Z3 environment using variable declarations and the WLP expression.
setupZ3Env :: Expr -> [VarDeclaration] -> Z3 AST
setupZ3Env processedWlp decls = do
  let negatedWlp = negateExpr processedWlp
  env <- buildEnv decls processedWlp M.empty -- Build the Z3 environment
  exprToZ3 negatedWlp env -- Convert the WLP to Z3 expression