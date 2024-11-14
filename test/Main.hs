module Main where

import Runner (run)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Types (Options (..))

main :: IO ()
main = defaultMain tests

-- Array of tuples that contain the programs and whether they are valid or not
testCases :: [(String, Bool)]
testCases =
  [ ("examples/E.gcl", False),
    ("examples/min.gcl", True),
    ("examples/minind.gcl", True),
    ("examples/reverse.gcl", True),
    ("examples/S1.gcl", True),
    ("examples/swap.gcl", True),
    -- Benchmarks
    -- ("examples/benchmark/divByN.gcl", True),
    -- ("examples/benchmark/memberOf.gcl", True),
    -- ("examples/benchmark/pullUp.gcl", True),
    ("examples/benchmark/invalidDivByN.gcl", False),
    ("examples/benchmark/invalidMemberOf.gcl", False),
    ("examples/benchmark/invalidPullUp.gcl", False)
  ]

-- Define the test suite
tests :: TestTree
tests =
  testGroup
    "GCL Program Validity Tests"
    [ testCase (filename ++ " validity test") $ do
        let options =
              Options
                { verbose = False,
                  k = 10,
                  n = 10,
                  pruneLen = 2
                }
        result <- run options filename -- Execute the run function
        assertEqual ("Expected validity for " ++ filename) expected result
      | (filename, expected) <- testCases -- Generate a test case for each entry
    ]
