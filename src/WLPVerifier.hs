{-# LANGUAGE LambdaCase #-}

module WLPVerifier where

import Control.Monad.Cont (MonadIO (liftIO))
import qualified Data.Map as M
import FormulaProcessor (PostCondition, negateExpr, processAST, wlp)
import GCLParser.GCLDatatype
import GCLParser.Parser
import PreProcessor
import Z3.Base
import Z3.Monad
import Z3Solver (buildEnv, exprToZ3)

-- | Runs the entire verification process on a given GCL file.
-- Takes a file path as input and returns 'True' if the program is valid,
-- or 'False' if it is not valid or if parsing fails.
run :: String -> IO Bool
run file = do
  -- Step 1: Parse the GCL file
  result <- parseGCLfile file

  case result of
    -- If parsing fails, print the error and return False
    Left err -> do
      putStrLn "Failed to parse the GCL file:"
      putStrLn err
      return False

    -- If parsing succeeds, proceed with the verification
    Right program -> do
      -- Step 2: Preprocess the parsed program
      -- Preprocess the program with a maximum recursion depth of 10 and flags enabled
      let preprocessedProgram = preprocess program 10 True True True

      -- Step 3: Compute the WLP (Weakest Liberal Precondition) expression
      -- Here, 'LitB True' represents the postcondition we are verifying against
      let processedWlp = wlp (stmt preprocessedProgram) (LitB True)

      -- Step 4: Run the Z3 solver
      evalZ3 $ do
        -- Create the environment for Z3 with the WLP expression
        env1 <- buildEnv (input program) processedWlp M.empty

        -- Negate the WLP expression and convert it to a Z3 expression
        z3Expr <- exprToZ3 (negateExpr processedWlp) env1

        -- Assert the Z3 expression and check the satisfiability
        assert z3Expr
        resultZ3 <- check

        -- Step 5: Handle the result from the Z3 solver
        case resultZ3 of
          -- If satisfiable, the program is invalid (counterexample exists)
          Sat -> return False
          -- If unsatisfiable, the program is valid (no counterexample exists)
          Unsat -> return True
          -- In any other case, return False as a fallback
          _ -> return False
