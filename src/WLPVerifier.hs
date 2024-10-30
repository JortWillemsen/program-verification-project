module WLPVerifier where

import Control.Monad.Cont (MonadIO (liftIO))
import qualified Data.Map as M
import DirectedCallGraphProcessor (dcgToPaths, programDCG, wlpDCG, solveZ3DCGs)
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
      let uniqueProgram = makeUniqueForall program
      
      let preprocessedProgram = preprocess uniqueProgram 10 True True True
            
      let pdcg = programDCG $ stmt preprocessedProgram
      
      let paths = dcgToPaths pdcg

      let wlpPaths = map (\p -> wlpDCG $ reverseDCG p) paths

      let stringpaths = map (\p -> printDCG p) wlpPaths

      putStrLn $ concat stringpaths

      result <- evalZ3 $ do
        env1 <- buildEnv (input program ++ output program ++ getVarDeclarations (stmt program)) (wlp (stmt program) (LitB True)) M.empty

        list <- solveZ3DCGs wlpPaths env1

        return list

      putStrLn (show result)

      return (all (\x -> x) result)
