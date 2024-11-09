module Main where

import WLPVerifier (run)

main :: IO ()
main = do
  isValid <- run "examples/minind.gcl"
  putStrLn $ "Is the program valid? " ++ show isValid
