module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map as M
import FormulaProcessor (negateExpr, wlp)
import GCLParser.GCLDatatype (Expr (..), Program (..))
import GCLParser.Parser (parseGCLfile)
import PreProcessor (preprocess)
import WLPVerifier (run)
import Z3.Monad
  ( Result (..),
    assert,
    check,
    evalZ3,
    getModel,
    modelToString,
  )
import Z3Solver (createEnv, exprToZ3)

main :: IO ()
main = do
  isValid <- run "examples/benchmark/divByN.gcl"
  putStrLn $ "Is the program valid? " ++ show isValid
