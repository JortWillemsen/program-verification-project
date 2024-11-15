module Runner where

import Control.Monad.Cont (MonadIO (liftIO), filterM, when)
import GCLHelper (getVarDeclarationsProgram, makeUnique, replaceExperimentParam)
import GCLParser.GCLDatatype (Program (..))
import GCLParser.Parser (parseGCLfile)
import PathGenerator (dcgToPaths, programToDCG)
import Types (Env, Options (..), Path)
import Z3Solver (buildEnv, isSatisfiablePath, isValidPathModel)

run :: Options -> String -> IO Bool
run options file = do
  result <- parseGCLfile file

  case result of
    Left _ -> do
      error "Failed to parse the GCL file:"
    Right program -> do
      let uniqueProgram = makeUnique program
      let replacedProgram = replaceExperimentParam uniqueProgram "N" (n options)
      let varDeclarations = getVarDeclarationsProgram replacedProgram
      let dcg = programToDCG (stmt replacedProgram) options

      let env = buildEnv varDeclarations

      when (verbose options) $ liftIO $ putStrLn "Env: "
      when (verbose options) $ liftIO $ print env

      paths <- dcgToPaths dcg env options

      when (verbose options) $ liftIO $ putStrLn "Number of Paths: "
      when (verbose options) $ liftIO $ print (length paths)

      -- Filter out the unfeasible paths
      fPaths <- filterM (isSatisfiablePath env) paths

      when (verbose options) $ liftIO $ putStrLn "Number of Feasible Paths: "
      when (verbose options) $ liftIO $ print (length fPaths)

      -- Check if all feasible paths are valid
      r <- validProgram fPaths env

      case r of
        Left b -> return b
        Right p -> do
          liftIO $ putStrLn "Incorrect path"
          liftIO $ print p
          return False

validProgram :: [Path] -> Env -> IO (Either Bool Path)
validProgram [] _ = return $ Left True
validProgram (p : ps) env = do
  valid <- isValidPathModel env p
  case valid of
    Left _ -> validProgram ps env
    Right m -> do
      liftIO $ putStrLn "Input for the program"
      liftIO $ putStr m

      return $ Right p
