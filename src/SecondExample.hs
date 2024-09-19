module SecondExample where

import GCLParser.Parser (parseGCLfile)
import GCLParser.GCLDatatype (Program(..), Stmt (..))

run :: IO ()
run = do
  result <- parseGCLfile "examples/min.gcl"
  
  case result of
      Left err -> do
        putStrLn "Failed to parse the GCL file:"
        putStrLn err
      Right gcl -> do
        processStmt $ stmt gcl

-- Function to recursively handle and pattern match on Stmt
processStmt :: Stmt -> IO ()
processStmt statement = case statement of
  Skip -> putStrLn "The statement is: Skip"
  Assert expr -> putStrLn $ "The statement is an Assert with condition: " ++ show expr
  Assume expr -> putStrLn $ "The statement is an Assume with condition: " ++ show expr
  Assign var expr -> putStrLn $ "The statement is an Assign: " ++ var ++ " := " ++ show expr
  AAssign var idx expr -> putStrLn $ "The statement is an Array Assign: " ++ var ++ "[" ++ show idx ++ "] := " ++ show expr
  Seq s1 s2 -> do
    putStrLn "The statement is a Sequence of two statements:"
    processStmt s1
    processStmt s2
  IfThenElse guard s1 s2 -> do
    putStrLn $ "The statement is an IfThenElse with guard: " ++ show guard
    putStrLn "Then branch:"
    processStmt s1
    putStrLn "Else branch:"
    processStmt s2
  While guard body -> do
    putStrLn $ "The statement is a While loop with guard: " ++ show guard
    putStrLn "Body:"
    processStmt body
  Block vars body -> do
    putStrLn $ "The statement is a Block with variables: " ++ show vars
    processStmt body
  TryCatch e s1 s2 -> do
    putStrLn $ "The statement is a Try-Catch block with exception: " ++ e
    putStrLn "Try block:"
    processStmt s1
    putStrLn "Catch block:"
    processStmt s2
  _ -> putStrLn "Other statement type"