module Main where

import Runner (run)
import Types (Options (..))

main :: IO ()
main = do
  let options =
        Options
          { verbose = False,
            k = 10,
            n = 3,
            pruneLen = 0
          }

  isValid <- run options "examples/benchmark/invalidDivByN.gcl"

  putStrLn $ "Is the program valid? " ++ show isValid
