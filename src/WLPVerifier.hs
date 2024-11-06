module WLPVerifier where

import Control.Monad.Cont (MonadIO (liftIO), filterM)
import DCG
import qualified Data.Map as M
import DirectedCallGraphProcessor (programDCG, prunePath, dcgToStatements, statementsToPath, validatePath)
import GCLParser.GCLDatatype (Expr (..), Program (input, output, stmt))
import GCLParser.Parser (parseGCLfile)
import PreProcessor (makeUniqueForall, preprocess)
import ProgramProcessor (negateExpr, wlp)
import Z3.Base (Result (Sat, Unsat))
import Z3.Monad (Result (Sat, Unsat), assert, check, evalZ3)
import Z3Solver (buildEnv, exprToZ3, getVarDeclarations)
import Prelude
import Text.ParserCombinators.ReadP (string)

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

      let preprocessedProgram = preprocess uniqueProgram True True True

      result <- evalZ3 $ do


        let pdcg = programDCG $ stmt preprocessedProgram

        --liftIO $ putStrLn $ printDCG pdcg

        let paths = dcgToStatements pdcg

        envPaths <- statementsToPath paths (input program ++ output program)


        prunedPaths <- filterM prunePath envPaths


        liftIO $ putStrLn (concatMap (\p -> show p ++ "\n") envPaths)
        liftIO $ putStrLn (concatMap (\p -> show (wlp p) ++ "\n") envPaths)
        -- liftIO $ putStrLn (concatMap (\p -> show p ++ "\n") prunedPaths)

        liftIO $ putStrLn $ "Paths pruned: " ++ show (length paths - length prunedPaths) ++ " Of " ++ show (length paths)

        r <- mapM validatePath prunedPaths

        case r of
          [] -> return False
          _ -> return $ and r

        -- liftIO $ putStrLn ("Paths pruned: " ++ show nPruned)

        -- wlpPaths <- mapM (wlpDCG . reverseDCG) paths

        -- let stringpaths = concatMap printDCG wlpPaths

        -- -- liftIO $ putStrLn (stringpaths)


        -- solveZ3DCGs wlpPaths env1

      return result
