module Main where

import Control.Monad.Cont (MonadIO (liftIO), filterM, when)
import GCLHelper (getVarDeclarationsProgram, makeUnique, replaceExperimentParam)
import GCLParser.GCLDatatype (Program (..))
import GCLParser.Parser (parseGCLfile)
import PathGenerator (dcgToPaths, programToDCG)
import Types (Options (..), Path (Path))
import Z3.Monad (evalZ3)
import Z3Solver (buildEnv, isSatisfiablePath, isValidPath)

main :: IO ()
main = do
  let options =
        Options
          { verbose = True,
            k = 10,
            n = 3,
            pruneLen = 50
          }

  isValid <- run options "examples/min.gcl"

  putStrLn $ "Is the program valid? " ++ show isValid

run :: Options -> String -> IO Bool
run options file = do
  result <- parseGCLfile file

  case result of
    Left _ -> do
      error "Failed to parse the GCL file:"
    Right program -> evalZ3 $ do
      let uniqueProgram = makeUnique program
      let replacedProgram = replaceExperimentParam uniqueProgram "N" (n options)
      let varDeclarations = getVarDeclarationsProgram replacedProgram
      let dcg = programToDCG (stmt replacedProgram) options

      env <- buildEnv varDeclarations
      paths <- dcgToPaths dcg env options

      when (verbose options) $ liftIO $ putStrLn "Env: "
      when (verbose options) $ liftIO $ print env
      when (verbose options) $ liftIO $ putStrLn "Paths: "
      when (verbose options) $ liftIO $ putStrLn $ concatMap (\(Path s _) -> show s ++ "\n") paths

      -- Filter out the unfeasible paths
      fPaths <- filterM isSatisfiablePath paths
      
      when (verbose options) $ liftIO $ putStrLn "Feasible Paths: "
      when (verbose options) $ liftIO $ putStrLn $ concatMap (\(Path s _) -> show s ++ "\n") fPaths

      -- Check if all feasible paths are valid
      r <- mapM isValidPath fPaths

      case r of
        [] -> return False
        _ -> return $ and r
