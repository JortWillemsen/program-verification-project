module Main where

import Control.Monad.IO.Class (liftIO)
import Criterion.Main
import Criterion.Types (Config (..), Verbosity (..))
import FormulaProcessor (processAST)
import GCLParser.GCLDatatype
import GCLParser.Parser
import PreProcessor
import WLPVerifier (parseAndProcessGCL, setupZ3Env)
import Z3.Monad (assert, check, evalZ3)

-- Array of programs to benchmark
benchmarkCases :: [FilePath]
benchmarkCases = ["examples/E.gcl", "examples/S1.gcl"]

-- Benchmark the `check` call
benchmarkCheck :: Expr -> IO ()
benchmarkCheck processedWlp = evalZ3 $ do
  z3Expr <- setupZ3Env processedWlp
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
    Right processedWlp ->
      return $ bench filePath $ nfIO (benchmarkCheck processedWlp)
