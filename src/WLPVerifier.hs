module WLPVerifier where

import Control.Monad.Cont (MonadIO (liftIO), filterM)
import DCG
import qualified Data.Map as M
import DirectedCallGraphProcessor (dcgToStatements, programDCG, prunePath, statementsToPath, validatePath)
import GCLParser.GCLDatatype (Expr (..), PrimitiveType (PTInt), Program (input, output, stmt), Stmt (..), Type (..), VarDeclaration (VarDeclaration))
import GCLParser.Parser (parseGCLfile)
import PreProcessor (makeUniqueForall, preprocess)
import ProgramProcessor (negateExpr, wlp)
import Text.ParserCombinators.ReadP (string)
import Z3.Base (Result (Sat, Unsat))
import Z3.Monad (Result (Sat, Unsat), assert, check, evalZ3)
import Z3Solver (buildEnv, exprToZ3)
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

      let preprocessedProgram = preprocess uniqueProgram True True True

      evalZ3 $ do
        let pdcg = programDCG $ stmt preprocessedProgram

        -- liftIO $ putStrLn $ printDCG pdcg

        let paths = dcgToStatements pdcg

        envPaths <- statementsToPath paths (input program ++ output program ++ getVarDeclarationsProgram (stmt preprocessedProgram))

        -- liftIO $ putStrLn (concatMap (\p -> show p ++ "\n") envPaths)

        prunedPaths <- filterM prunePath envPaths

        r <- mapM validatePath prunedPaths

        case r of
          [] -> return False
          _ -> return $ and r

-- liftIO $ putStrLn (concatMap (\p -> show (wlp p) ++ "\n") envPaths)
-- liftIO $ putStrLn (concatMap (\p -> show p ++ "\n") prunedPaths)

-- liftIO $ putStrLn $ "Paths pruned: " ++ show (length paths - length prunedPaths) ++ " Of " ++ show (length paths)

-- liftIO $ putStrLn ("Paths pruned: " ++ show nPruned)

-- wlpPaths <- mapM (wlpDCG . reverseDCG) paths

-- let stringpaths = concatMap printDCG wlpPaths

-- -- liftIO $ putStrLn (stringpaths)

-- solveZ3DCGs wlpPaths env1

getVarDeclarationsProgram :: Stmt -> [VarDeclaration]
getVarDeclarationsProgram Skip = []
getVarDeclarationsProgram (Assert e) = getVarsDeclarationsExpr e
getVarDeclarationsProgram (Assume e) = getVarsDeclarationsExpr e
getVarDeclarationsProgram (Assign _ _) = []
getVarDeclarationsProgram (DrefAssign _ _) = []
getVarDeclarationsProgram (AAssign {}) = []
getVarDeclarationsProgram (Seq s1 s2) = getVarDeclarationsProgram s1 ++ getVarDeclarationsProgram s2
getVarDeclarationsProgram (IfThenElse _ s1 s2) = getVarDeclarationsProgram s1 ++ getVarDeclarationsProgram s2
getVarDeclarationsProgram (While _ s) = getVarDeclarationsProgram s
getVarDeclarationsProgram (Block vars s) = vars ++ getVarDeclarationsProgram s
getVarDeclarationsProgram (TryCatch _ s1 s2) = getVarDeclarationsProgram s1 ++ getVarDeclarationsProgram s2

getVarsDeclarationsExpr :: Expr -> [VarDeclaration]
getVarsDeclarationsExpr (Parens e) = getVarsDeclarationsExpr e
getVarsDeclarationsExpr (Forall str e) = VarDeclaration str (PType PTInt) : getVarsDeclarationsExpr e
getVarsDeclarationsExpr (Exists str e) = VarDeclaration str (PType PTInt) : getVarsDeclarationsExpr e
getVarsDeclarationsExpr (BinopExpr _ e1 e2) = getVarsDeclarationsExpr e1 ++ getVarsDeclarationsExpr e2
getVarsDeclarationsExpr _ = []
