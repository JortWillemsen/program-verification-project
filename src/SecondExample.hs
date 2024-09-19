module SecondExample where

import GCLParser.Parser (parseGCLfile)
import GCLParser.GCLDatatype (Program(..))

run :: IO ()
run = do
  result <- parseGCLfile "examples/min.gcl"
  
  case result of
    Left err -> do
      putStrLn "Failed to parse the GCL file:"
      putStrLn err
    Right gcl -> do
      -- Extract the stmt from the parsed Program
      let statement = stmt gcl
      
      -- Print the statement
      print statement
