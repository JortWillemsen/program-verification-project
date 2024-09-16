module SecondExample where

import GCLParser.Parser (parseGCLfile)
import GCLParser.PrettyPrint (ppProgram2String)

run :: IO ()
run = do
  gcl <- parseGCLfile "examples/benchmark/bsort.gcl"

  let (Right prg) = gcl
  putStrLn . ppProgram2String $ prg
