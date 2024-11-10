module Main where
import GCLParser.Parser (parseGCLfile)
import PathGenerator (dcgToPaths, programToDCG)
import GCLHelper (makeUnique, replaceExperimentParam, getVarDeclarationsProgram)
import Types (Options(..), Path (Path))
import GCLParser.GCLDatatype (Program(..))
import Control.Monad.Cont (MonadIO(liftIO), when, filterM)
import Z3Solver (buildEnv, isValidPath, isSatisfiablePath)
import Z3.Monad (evalZ3)
import WlpGenerator (conjunctive, wlp)

main :: IO ()
main = do
  let options = Options { 
      verbose = True
    , k = 10
    , n = 3
    , pruneLen = 50
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

        when (verbose options) $ liftIO $ print env
        when (verbose options) $ liftIO $ putStrLn $ concatMap (\(Path s _) -> show (conjunctive s) ++ "\n") paths
        when (verbose options) $ liftIO $ print (length paths)

        fPaths <- filterM isSatisfiablePath paths
        --when (verbose options) $ liftIO $ print (length fPaths)


        r <- mapM isValidPath fPaths

        case r of
          [] -> return False
          _ -> return $ and r

