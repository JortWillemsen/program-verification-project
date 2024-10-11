{-# LANGUAGE LambdaCase #-}

module WLPVerifier where

import qualified Data.Map as M
import FormulaProcessor (negateExpr, processAST, wlp)
import GCLParser.GCLDatatype
import GCLParser.Parser
import PreProcessor
import Z3.Base
import Z3.Monad
import Z3Solver (createEnv, exprToZ3)

-- Function to run the entire verification process
run :: String -> IO Bool
run file = do
  eitherWlp <- parseAndProcessGCL file
  case eitherWlp of
    Left _ -> return False
    Right wlp ->
      evalZ3 $ do
        -- Set up the Z3 environment and get the Z3 expression
        z3Expr <- setupZ3Env wlp
        -- Perform the check, which can be benchmarked separately
        resultZ3 <- check
        -- Handle the result
        return $ case resultZ3 of
          Sat -> False -- Program is not valid
          Unsat -> True -- Program is valid
          _ -> False -- Handle other cases

-- Function to set up the Z3 environment and return the Z3 expression
setupZ3Env :: Expr -> Z3 AST
setupZ3Env processedWlp = do
  env1 <- createEnv processedWlp M.empty
  exprToZ3 (negateExpr processedWlp) env1

-- Function to parse and preprocess a GCL file
parseAndProcessGCL :: String -> IO (Either String Expr)
parseAndProcessGCL file =
  parseGCLfile file >>= \case
    Left err -> return $ Left err
    Right program ->
      let preprocessedProgram = preprocess program 10 True True True
          processedWlp = processAST preprocessedProgram
       in return $ Right processedWlp
