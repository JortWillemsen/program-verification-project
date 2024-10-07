module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map as M
import FormulaProcessor (wlp)
import GCLParser.GCLDatatype (Expr (..), Program (..))
import GCLParser.Parser (parseGCLfile)
import Z3.Monad
  ( Result (Sat),
    assert,
    astToString,
    check,
    evalZ3,
    getModel,
    modelToString,
  )
import Z3Solver (createEnv, exprToZ3)

main :: IO ()
main = run

run :: IO ()
run = do
  result <- parseGCLfile "examples/second_test.gcl"

  case result of
    Left err -> do
      putStrLn "Failed to parse the GCL file:"
      putStrLn err
    Right program -> do
      -- let z3 = exprToZ3 $ stmt gcl
      let processedWlp = wlp (stmt program) (LitB True)
      evalZ3 $ do
        env1 <- createEnv processedWlp M.empty
        z3Expr <- exprToZ3 processedWlp env1

        liftIO $ putStrLn $ "Z3 Expression: " ++ show z3Expr
        -- Convert Z3 expression to a readable string
        z3ExprStr <- astToString z3Expr
        liftIO $ putStrLn $ "Z3 Expression: " ++ z3ExprStr

        liftIO $ print z3Expr

        assert z3Expr

        resultZ3 <- check

        case resultZ3 of
          Sat -> do
            mbModel <- getModel -- getModel returns Maybe Model
            case mbModel of
              (_, Just model) -> do
                modelStr <- modelToString model -- Convert model to string
                liftIO $ putStrLn "Model:"
                liftIO $ putStrLn modelStr
              (_, Nothing) -> liftIO $ putStrLn "No model found"
          _ -> return ()
