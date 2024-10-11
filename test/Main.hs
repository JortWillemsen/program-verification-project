module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import WLPVerifier (run)

main :: IO ()
main = defaultMain tests

-- Array of tuples that contain the programs and whether they are valid or not
testCases :: [(String, Bool)]
testCases =
  [ ("examples/E.gcl", False),
    -- ("examples/min.gcl", True),
    -- ("examples/minind.gcl", True),
    -- ("examples/reftest.gcl", True)
    -- ("examples/reverse.gcl", True),
    ("examples/S1.gcl", True)
    -- ("examples/swap.gcl", True),
    -- ("examples/test.gcl", True)
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
