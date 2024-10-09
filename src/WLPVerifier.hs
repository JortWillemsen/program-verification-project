module WLPVerifier where

import qualified Data.Map as M
import FormulaProcessor (negateExpr, wlp)
import GCLParser.GCLDatatype
import GCLParser.Parser
import PreProcessor
import Z3.Base
import Z3.Monad
import Z3Solver (createEnv, exprToZ3)

run :: String -> IO Bool
run file = do
  result <- parseGCLfile file

  case result of
    Left err -> do
      putStrLn "Failed to parse the GCL file:"
      putStrLn err
      return False -- Return False if parsing fails
    Right program -> do
      -- Preprocess the program once and store the result
      let preprocessedProgram = preprocess program True True True True

      -- Compute the WLP expression for benchmarking
      let processedWlp = wlp (stmt preprocessedProgram) (LitB True)

      evalZ3 $ do
        env1 <- createEnv processedWlp M.empty
        z3Expr <- exprToZ3 (negateExpr processedWlp) env1

        assert z3Expr

        resultZ3 <- check

        case resultZ3 of
          Sat -> do
            return False -- Return False if the program is not valid
          Unsat -> do
            return True -- Return True if the program is valid
          _ -> return False -- Handle any other cases
