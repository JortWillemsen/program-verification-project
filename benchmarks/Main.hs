module Main where

import Criterion.Main
import Criterion.Types (Config (..), Verbosity (..))
import GCLParser.GCLDatatype
import GCLParser.Parser
import Runner (run)
import Types (Options (..))

-- Array of programs to benchmark
benchmarkCases :: [(FilePath, String, Options)]
benchmarkCases =
  [ ("examples/E.gcl", "E", Options {verbose = False, k = 10, n = 2, pruneLen = 0}),
    ("examples/S1.gcl", "S1", Options {verbose = False, k = 10, n = 2, pruneLen = 0})
    -- ("examples/benchmark/divByN.gcl", "divByN k=10", Options {verbose = False, k = 10, n = 10, pruneLen = 0}),
    -- ("examples/benchmark/divByN.gcl", "divByN k=20", Options {verbose = False, k = 20, n = 10, pruneLen = 0}),
    -- ("examples/benchmark/divByN.gcl", "divByN k=30", Options {verbose = False, k = 30, n = 10, pruneLen = 0}),
    -- ("examples/benchmark/divByN.gcl", "divByN k=40", Options {verbose = False, k = 40, n = 10, pruneLen = 0}),
    -- ("examples/benchmark/divByN.gcl", "divByN k=50", Options {verbose = False, k = 50, n = 10, pruneLen = 0}),
    -- ("examples/benchmark/memberOf.gcl", "memberOf k=10", Options {verbose = False, k = 10, n = 10, pruneLen = 0}),
    -- ("examples/benchmark/memberOf.gcl", "memberOf k=20", Options {verbose = False, k = 20, n = 10, pruneLen = 0}),
    -- ("examples/benchmark/memberOf.gcl", "memberOf k=30", Options {verbose = False, k = 30, n = 10, pruneLen = 0}),
    -- ("examples/benchmark/memberOf.gcl", "memberOf k=40", Options {verbose = False, k = 40, n = 10, pruneLen = 0}),
    -- ("examples/benchmark/memberOf.gcl", "memberOf k=50", Options {verbose = False, k = 50, n = 10, pruneLen = 0}),
    -- ("examples/benchmark/pullUp.gcl", "pullUp k=10", Options {verbose = False, k = 10, n = 10, pruneLen = 0}),
    -- ("examples/benchmark/pullUp.gcl", "pullUp k=20", Options {verbose = False, k = 20, n = 10, pruneLen = 0}),
    -- ("examples/benchmark/pullUp.gcl", "pullUp k=30", Options {verbose = False, k = 30, n = 10, pruneLen = 0}),
    -- ("examples/benchmark/pullUp.gcl", "pullUp k=40", Options {verbose = False, k = 40, n = 10, pruneLen = 0}),
    -- ("examples/benchmark/pullUp.gcl", "pullUp k=50", Options {verbose = False, k = 50, n = 10, pruneLen = 0}),
    -- ("examples/benchmark/invalidDivByN.gcl", "invalidDivByN n=2", Options {verbose = False, k = 20, n = 2, pruneLen = 0}),
    -- ("examples/benchmark/invalidDivByN.gcl", "invalidDivByN n=3", Options {verbose = False, k = 20, n = 3, pruneLen = 0}),
    -- ("examples/benchmark/invalidDivByN.gcl", "invalidDivByN n=4", Options {verbose = False, k = 20, n = 4, pruneLen = 0}),
    -- ("examples/benchmark/invalidDivByN.gcl", "invalidDivByN n=5", Options {verbose = False, k = 20, n = 5, pruneLen = 0}),
    -- ("examples/benchmark/invalidDivByN.gcl", "invalidDivByN n=6", Options {verbose = False, k = 20, n = 6, pruneLen = 0}),
    -- ("examples/benchmark/invalidDivByN.gcl", "invalidDivByN n=7", Options {verbose = False, k = 20, n = 7, pruneLen = 0}),
    -- ("examples/benchmark/invalidDivByN.gcl", "invalidDivByN n=8", Options {verbose = False, k = 20, n = 8, pruneLen = 0}),
    -- ("examples/benchmark/invalidDivByN.gcl", "invalidDivByN n=9", Options {verbose = False, k = 20, n = 9, pruneLen = 0}),
    -- ("examples/benchmark/invalidDivByN.gcl", "invalidDivByN n=10", Options {verbose = False, k = 20, n = 10, pruneLen = 0}),
    -- ("examples/benchmark/invalidMemberOf.gcl", "invalidMemberOf n=2", Options {verbose = False, k = 20, n = 2, pruneLen = 0}),
    -- ("examples/benchmark/invalidMemberOf.gcl", "invalidMemberOf n=3", Options {verbose = False, k = 20, n = 3, pruneLen = 0}),
    -- ("examples/benchmark/invalidMemberOf.gcl", "invalidMemberOf n=4", Options {verbose = False, k = 20, n = 4, pruneLen = 0}),
    -- ("examples/benchmark/invalidMemberOf.gcl", "invalidMemberOf n=5", Options {verbose = False, k = 20, n = 5, pruneLen = 0}),
    -- ("examples/benchmark/invalidMemberOf.gcl", "invalidMemberOf n=6", Options {verbose = False, k = 20, n = 6, pruneLen = 0}),
    -- ("examples/benchmark/invalidMemberOf.gcl", "invalidMemberOf n=7", Options {verbose = False, k = 20, n = 7, pruneLen = 0}),
    -- ("examples/benchmark/invalidMemberOf.gcl", "invalidMemberOf n=8", Options {verbose = False, k = 20, n = 8, pruneLen = 0}),
    -- ("examples/benchmark/invalidMemberOf.gcl", "invalidMemberOf n=9", Options {verbose = False, k = 20, n = 9, pruneLen = 0}),
    -- ("examples/benchmark/invalidMemberOf.gcl", "invalidMemberOf n=10", Options {verbose = False, k = 20, n = 10, pruneLen = 0}),
    -- ("examples/benchmark/invalidPullUp.gcl", "invalidPullUp n=2", Options {verbose = False, k = 20, n = 2, pruneLen = 0}),
    -- ("examples/benchmark/invalidPullUp.gcl", "invalidPullUp n=3", Options {verbose = False, k = 20, n = 3, pruneLen = 0}),
    -- ("examples/benchmark/invalidPullUp.gcl", "invalidPullUp n=4", Options {verbose = False, k = 20, n = 4, pruneLen = 0}),
    -- ("examples/benchmark/invalidPullUp.gcl", "invalidPullUp n=5", Options {verbose = False, k = 20, n = 5, pruneLen = 0}),
    -- ("examples/benchmark/invalidPullUp.gcl", "invalidPullUp n=6", Options {verbose = False, k = 20, n = 6, pruneLen = 0}),
    -- ("examples/benchmark/invalidPullUp.gcl", "invalidPullUp n=7", Options {verbose = False, k = 20, n = 7, pruneLen = 0}),
    -- ("examples/benchmark/invalidPullUp.gcl", "invalidPullUp n=8", Options {verbose = False, k = 20, n = 8, pruneLen = 0}),
    -- ("examples/benchmark/invalidPullUp.gcl", "invalidPullUp n=9", Options {verbose = False, k = 20, n = 9, pruneLen = 0}),
    -- ("examples/benchmark/invalidPullUp.gcl", "invalidPullUp n=10", Options {verbose = False, k = 20, n = 10, pruneLen = 0})
  ]

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
createBenchmark :: (FilePath, String, Options) -> IO Benchmark
createBenchmark (filePath, title, options) = do
  return $ bench title $ nfIO (run options filePath)
