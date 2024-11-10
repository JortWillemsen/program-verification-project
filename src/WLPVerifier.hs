module WLPVerifier where

import Control.Monad.Cont (MonadIO (liftIO), filterM)
import DCG
import qualified Data.Map as M
import DirectedCallGraphProcessor (dcgToStatements, programDCG, prunePath, validatePath, feasiblePath)
import GCLParser.GCLDatatype (Expr (..), PrimitiveType (PTInt), Program (input, output, stmt), Stmt (..), Type (..), VarDeclaration (VarDeclaration))
import GCLParser.Parser (parseGCLfile)
import PreProcessor (makeUniqueForall, preprocess)
import ProgramProcessor (negateExpr, wlp)
import Text.ParserCombinators.ReadP (string)
import Z3.Base (Result (Sat, Unsat))
import Z3.Monad (Result (Sat, Unsat), assert, check, evalZ3, MonadZ3)
import Z3Solver (buildEnv, makeProblematicAST)
import Prelude
import Types (Path(Path))

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
        -- problem <- makeProblematicAST

        -- assert problem
        -- result <- check

        -- case result of
        --   Sat -> return True
        --   Unsat -> return False
        --   _ -> error "Result is weird (feasible)"

        let pdcg = programDCG $ stmt preprocessedProgram

        -- liftIO $ putStrLn $ printDCG pdcg

        paths <- dcgToStatements pdcg (input program ++ output program ++ getVarDeclarationsProgram (stmt preprocessedProgram)) []
        -- let pathLengths = map (\(Path s _) -> length s) paths
        -- liftIO $ putStrLn (concatMap (\(Path s _) -> show s ++ "\n") paths)
        --liftIO $ putStr $ "Max path length: " ++ show (maximum pathLengths)
        -- prunedPaths <- filterM prunePath envPaths
        --liftIO $ putStr $ ", Total paths: " ++ show (length paths)
        
        feasiblePaths <- filterM feasiblePath paths

        r <- mapM validatePath feasiblePaths
        
        --liftIO $ putStrLn (concatMap (\p -> show p ++ "\n") r)
        --liftIO $ putStrLn $ "Paths pruned: " ++ show (length paths - length prunedPaths) ++ " Of " ++ show (length paths)

        case r of
          [] -> return False
          _ -> return $ and r

-- liftIO $ putStrLn (concatMap (\p -> show (wlp p) ++ "\n") envPaths)
        

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

validityCheck :: (MonadZ3 z3) => [Path] -> z3 Bool
validityCheck (p:ps) = do
  valid <- validatePath p

  if (valid)
    then validityCheck ps
    else error $ "Found invalid path: \n" ++ show (wlp p) ++ "\n Exiting verifier"
validityCheck [] = return True