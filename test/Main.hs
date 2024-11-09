module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import WLPVerifier (run)

main :: IO ()
main = defaultMain tests

-- Array of tuples that contain the programs and whether they are valid or not
testCases :: [(String, Bool)]
testCases =
  [ --("examples/E.gcl", False),
    --("examples/min.gcl", True),
    --("examples/minind.gcl", True),
    -- ("examples/reftest.gcl", True)
    --("examples/reverse.gcl", True),
    --("examples/S1.gcl", True),
    --("examples/swap.gcl", True),
    -- Benchmarks
    -- ("examples/benchmark/bsort.gcl", True), -- Loop optional
    -- ("examples/benchmark/divByN.gcl", True),
    -- ("examples/benchmark/find12.gcl", True), -- Exceptions
    -- ("examples/benchmark/invalidBSort.gcl", False) -- Loop optional
    --("examples/benchmark/invalidDivByN.gcl", False),
    -- ("examples/benchmark/invalidFind12.gcl", False), -- Exceptions
    ("examples/benchmark/invalidMemberOf.gcl", False),
    --("examples/benchmark/invalidMin.gcl", False), -- References
    ("examples/benchmark/invalidPullUp.gcl", False)
    --("examples/benchmark/memberOf.gcl", True),
    -- ("examples/benchmark/min.gcl", True), -- References
    --("examples/benchmark/pullUp.gcl", True)
  ]

-- Define the test suite
tests :: TestTree
tests =
  testGroup
    "GCL Program Validity Tests"
    [ testCase (filename ++ " validity test") $ do
        result <- run filename -- Execute the run function
        assertEqual ("Expected validity for " ++ filename) expected result
      | (filename, expected) <- testCases -- Generate a test case for each entry
    ]
