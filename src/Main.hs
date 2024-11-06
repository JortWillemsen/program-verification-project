module Main where

import WLPVerifier (run)

main :: IO ()
main = do
  isValid <- run "examples/S1.gcl"
  putStrLn $ "Is the program valid? " ++ show isValid
