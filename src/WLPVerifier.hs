{-# LANGUAGE LambdaCase #-}

module WLPVerifier where

import Control.Monad.Cont (MonadIO (liftIO))
import qualified Data.Map as M
import FormulaProcessor (negateExpr, processAST, wlp)
import GCLParser.GCLDatatype
import GCLParser.Parser
import PreProcessor
import Z3.Base
import Z3.Monad
import Z3Solver (createEnv, exprToZ3)

-- Function to run the entire verification process
run :: String -> IO Bool
run file = do
  -- eitherWlp <- parseAndProcessGCL file
  result <- parseGCLfile file

  case result of
    Left err -> do
      putStrLn "Failed to parse the GCL file:"
      putStrLn err
      return False -- Return False if parsing fails
    Right program -> do
      -- Preprocess the program once and store the result
      let preprocessedProgram = preprocess program 10 True True True
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

--   case result of
--     Left _ -> return False
--     Right wlp ->
--       evalZ3 $ do
--         liftIO $ print $ OpNeg wlp

--         env1 <- createEnv wlp M.empty

--         -- Set up the Z3 environment and get the Z3 expression
--         z3Expr <- exprToZ3 (negateExpr wlp) env1

--         -- Perform the check, which can be benchmarked separately
--         resultZ3 <- check
--         -- Handle the result
--         return $ case resultZ3 of
--           Sat -> False -- Program is not valid
--           Unsat -> True -- Program is valid
--           _ -> False -- Handle other cases

-- -- Function to parse and preprocess a GCL file
-- parseAndProcessGCL :: String -> IO (Either String Expr)
-- parseAndProcessGCL file =
--   parseGCLfile file >>= \case
--     Left err -> return $ Left err
--     Right program ->
--       let preprocessedProgram = preprocess program 10 True True True
--           processedWlp = wlp (stmt preprocessedProgram) (LitB True)
--        in return $ Right processedWlp
