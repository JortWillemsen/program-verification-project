module Main where

import WLPVerifier (run)

main :: IO ()
main = do
  isValid <- run "examples/benchmark/invalidMemberOf.gcl"
  putStrLn $ "Is the program valid? " ++ show isValid
