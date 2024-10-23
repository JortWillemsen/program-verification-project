module WLPVerifier where

import Control.Monad.Cont (MonadIO (liftIO))
import qualified Data.Map as M
import DirectedCallGraphProcessor (programDCG, reversedProgramDCG)
import GCLParser.GCLDatatype (Expr (..), Program (input, output, stmt))
import GCLParser.Parser (parseGCLfile)
import PreProcessor (makeUniqueForall, preprocess)
import ProgramProcessor (negateExpr, processAST, wlp)
import Z3.Base (Result (Sat, Unsat))
import Z3.Monad (Result (Sat, Unsat), assert, check, evalZ3)
import Z3Solver (buildEnv, exprToZ3, getVarDeclarations)
import DCG
import Prelude

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
      let asdf = stmt program

      let yeet = reversedProgramDCG asdf

      putStrLn $ printDCG yeet

      return False

-- let uniqueProgram = makeUniqueForall program

-- -- Step 2: Preprocess the parsed program
-- -- Preprocess the program with a maximum recursion depth of 10 and flags enabled

-- -- print uniqueProgram

-- putStrLn ""

-- let preprocessedProgram = preprocess uniqueProgram 10 True True True

-- -- Step 3: Compute the WLP (Weakest Liberal Precondition) expression
-- -- Here, 'LitB True' represents the postcondition we are verifying against
-- let processedWlp = wlp (stmt preprocessedProgram) (LitB True)

-- -- print processedWlp

-- putStrLn ""

-- -- Step 4: Run the Z3 solver
-- evalZ3 $ do
--   -- Create the environment for Z3 with the WLP expression
--   env1 <- buildEnv (input uniqueProgram ++ output uniqueProgram ++ getVarDeclarations (stmt uniqueProgram)) (wlp (stmt uniqueProgram) (LitB True)) M.empty

--   -- liftIO $ print env1

--   -- Negate the WLP expression and convert it to a Z3 expression
--   z3Expr <- exprToZ3 (negateExpr processedWlp) env1

--   -- liftIO $ print z3Expr

--   -- Assert the Z3 expression and check the satisfiability
--   assert z3Expr
--   resultZ3 <- check

--   -- Step 5: Handle the result from the Z3 solver
--   case resultZ3 of
--     -- If satisfiable, the program is invalid (counterexample exists)
--     Sat -> return False
--     -- If unsatisfiable, the program is valid (no counterexample exists)
--     Unsat -> return True
--     -- In any other case, return False as a fallback
--     _ -> return False
