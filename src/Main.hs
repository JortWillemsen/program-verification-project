module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map as M
import FormulaProcessor (negateExpr, wlp)
import GCLParser.GCLDatatype (Expr (..), Program (..))
import GCLParser.Parser (parseGCLfile)
import PreProcessor (preprocess)
import Z3.Monad
  ( Result (..),
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
  result <- parseGCLfile "examples/E.gcl"

  case result of
    Left err -> do
      putStrLn "Failed to parse the GCL file:"
      putStrLn err
    Right program -> do
      -- let z3 = exprToZ3 $ stmt gcl
      let preprocessedProgram = preprocess program True True True True
      let processedWlp = wlp (stmt preprocessedProgram) (LitB True)
      evalZ3 $ do
        env1 <- createEnv processedWlp M.empty
        z3Expr <- exprToZ3 (negateExpr processedWlp) env1
        -- Convert Z3 expression to a readable string
        z3ExprStr <- astToString z3Expr
        liftIO $ putStrLn $ "Z3 Expression: " ++ z3ExprStr

        assert z3Expr

        resultZ3 <- check

        case resultZ3 of
          Sat -> do
            liftIO $ putStrLn "Negation of the expression is satisfiable"
            mbModel <- getModel -- getModel returns Maybe Model
            case mbModel of
              (_, Just model) -> do
                modelStr <- modelToString model -- Convert model to string
                liftIO $ putStrLn "Model:"
                liftIO $ putStrLn modelStr
              (_, Nothing) -> liftIO $ putStrLn "No model found"
          Unsat -> do
            liftIO $ putStrLn "Negation of the expression is unsatisfiable, thus valid"
          _ -> return ()
