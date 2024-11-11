module Main where

import Control.Monad.Cont (MonadIO (liftIO), filterM, when)
import GCLHelper (getVarDeclarationsProgram, makeUnique, replaceExperimentParam)
import GCLParser.GCLDatatype (Program (..))
import GCLParser.Parser (parseGCLfile)
import PathGenerator (dcgToPaths, programToDCG)
import Types (Options (..), Path)
import Z3Solver (isSatisfiablePath, isValidPath, buildEnv)

main :: IO ()
main = do
  let options =
        Options
          { verbose = True,
            k = 10,
            n = 3,
            pruneLen = 2
          }

  isValid <- run options "examples/minind.gcl"

  putStrLn $ "Is the program valid? " ++ show isValid

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

      paths <- dcgToPaths dcg env options

      when (verbose options) $ liftIO $ putStrLn "Env: "
      when (verbose options) $ liftIO $ print env
      when (verbose options) $ liftIO $ putStrLn "Paths: "
      when (verbose options) $ liftIO $ putStrLn $ concatMap (\s -> show s ++ "\n") paths

      -- Filter out the unfeasible paths
      fPaths <- filterM (isSatisfiablePath env) paths

      -- liftIO $ putStrLn "Feasible Paths: "
      -- liftIO $ putStrLn $ concatMap (\s -> show s ++ "\n") fPaths

      -- Check if all feasible paths are valid
      r <- mapM (isValidPath env) fPaths

      case r of
        [] -> return False
        _ -> return $ and r
