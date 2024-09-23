{-# LANGUAGE AllowAmbiguousTypes #-}

module SecondExample where

import Control.Monad.IO.Class
import GCLParser.GCLDatatype (BinOp (..), Expr (..), Program (..), Stmt (..))
import GCLParser.Parser (parseGCLfile)
import Z3.Monad

run :: IO ()
run = do
  result <- parseGCLfile "examples/min.gcl"

  case result of
    Left err -> do
      putStrLn "Failed to parse the GCL file:"
      putStrLn err
    Right gcl -> do
      -- let z3 = exprToZ3 $ stmt gcl
      print $ stmt gcl

-- data Stmt
--   = Skip
--   | Assert Expr
--   | Assume Expr
--   | Assign String Expr
--   | AAssign String Expr Expr
--   | DrefAssign String Expr
--   | Seq Stmt Stmt
--   | IfThenElse Expr Stmt Stmt
--   | While Expr Stmt
--   | Block [VarDeclaration] Stmt
--   | TryCatch String Stmt Stmt

--    | Call       [String]         [Expr] String

-- runZ3
stmtToZ3 :: (MonadZ3 z3) => Stmt -> z3 AST
stmtToZ3 Skip = mkTrue
stmtToZ3 (Assert e) = exprToZ3 e
stmtToZ3 (Assume e) = exprToZ3 e
stmtToZ3 (Assign str e) = undefined
stmtToZ3 (AAssign str e1 e2) = undefined
stmtToZ3 (DrefAssign str e) = undefined -- optional
stmtToZ3 (Seq stmt1 stmt2) = do
  undefined
stmtToZ3 (IfThenElse e stmt1 stmt2) = undefined
stmtToZ3 (While e stmt) = undefined
stmtToZ3 (Block vars stmt) = undefined
stmtToZ3 (TryCatch str stmt1 stmt2) = undefined

exprToZ3 :: (MonadZ3 z3) => Expr -> z3 AST
exprToZ3 LitNull = undefined -- optional
exprToZ3 (Forall str e) = do
  qVar <- mkFreshIntVar str
  pat <- mkPattern [qVar]
  z3Expr <- exprToZ3 e
  mkForallConst [pat] [] z3Expr
exprToZ3 (Parens e) = exprToZ3 e
exprToZ3 (ArrayElem e1 i) = do
  z3Arr <- exprToZ3 e1
  z3Index <- exprToZ3 i
  mkSelect z3Arr z3Index
exprToZ3 (OpNeg e) = do
  z3Expr <- exprToZ3 e
  mkUnaryMinus z3Expr
exprToZ3 (Exists str e) = do
  qVar <- mkFreshIntVar str
  pat <- mkPattern [qVar]
  z3Expr <- exprToZ3 e
  mkExistsConst [pat] [] z3Expr
exprToZ3 (SizeOf e) = mkIntNum $ sizeOfExpr e
exprToZ3 (RepBy arr i v) = do
  arrZ3 <- exprToZ3 arr
  indexZ3 <- exprToZ3 i
  valueZ3 <- exprToZ3 v
  mkStore arrZ3 indexZ3 valueZ3
exprToZ3 (Cond g e1 e2) = do
  guardZ3 <- exprToZ3 g
  thenZ3 <- exprToZ3 e1
  elseZ3 <- exprToZ3 e2
  mkIte guardZ3 thenZ3 elseZ3
exprToZ3 (NewStore e) = undefined -- optional
exprToZ3 (Dereference str) = undefined -- optional
exprToZ3 (LitI i) = mkInteger (toInteger i)
exprToZ3 (LitB True) = mkTrue
exprToZ3 (LitB False) = mkFalse
exprToZ3 (Var n) = mkFreshIntVar n -- All vars are integers right?
exprToZ3 (BinopExpr op e1 e2) = do
  z3e1 <- exprToZ3 e1
  z3e2 <- exprToZ3 e2
  case op of
    Plus -> mkAdd [z3e1, z3e2]
    Minus -> mkSub [z3e1, z3e2]
    Multiply -> mkMul [z3e1, z3e2]
    Divide -> mkDiv z3e1 z3e2
    LessThan -> mkLt z3e1 z3e2
    LessThanEqual -> mkLe z3e1 z3e2
    GreaterThan -> mkGt z3e1 z3e2
    GreaterThanEqual -> mkGe z3e1 z3e2
    Equal -> mkEq z3e1 z3e2
    And -> mkAnd [z3e1, z3e2]
    Or -> mkOr [z3e1, z3e2]
    Implication -> mkImplies z3e1 z3e2
    Alias -> undefined

sizeOfExpr :: Expr -> Int
sizeOfExpr (Var _) = 1
sizeOfExpr (LitI _) = 1
sizeOfExpr (LitB _) = 1
sizeOfExpr LitNull = 1
sizeOfExpr (Parens e) = sizeOfExpr e
sizeOfExpr (ArrayElem e1 e2) = 1 + sizeOfExpr e1 + sizeOfExpr e2
sizeOfExpr (OpNeg e) = 1 + sizeOfExpr e
sizeOfExpr (BinopExpr _ e1 e2) = 1 + sizeOfExpr e1 + sizeOfExpr e2
sizeOfExpr (Forall _ e) = 1 + sizeOfExpr e
sizeOfExpr (Exists _ e) = 1 + sizeOfExpr e
sizeOfExpr (SizeOf e) = 1 + sizeOfExpr e
sizeOfExpr (RepBy e1 e2 e3) = 1 + sizeOfExpr e1 + sizeOfExpr e2 + sizeOfExpr e3
sizeOfExpr (Cond e1 e2 e3) = 1 + sizeOfExpr e1 + sizeOfExpr e2 + sizeOfExpr e3
sizeOfExpr (NewStore e) = 1 + sizeOfExpr e
sizeOfExpr (Dereference _) = 1

-- Function to recursively handle and pattern match on Stmt
processStmt :: Stmt -> IO ()
processStmt statement = case statement of
  Skip -> putStrLn "The statement is: Skip"
  Assert expr -> putStrLn $ "The statement is an Assert with condition: " ++ show expr
  Assume expr -> putStrLn $ "The statement is an Assume with condition: " ++ show expr
  Assign var expr -> putStrLn $ "The statement is an Assign: " ++ var ++ " := " ++ show expr
  AAssign var idx expr -> putStrLn $ "The statement is an Array Assign: " ++ var ++ "[" ++ show idx ++ "] := " ++ show expr
  Seq s1 s2 -> do
    putStrLn "The statement is a Sequence of two statements:"
    processStmt s1
    processStmt s2
  IfThenElse guard s1 s2 -> do
    putStrLn $ "The statement is an IfThenElse with guard: " ++ show guard
    putStrLn "Then branch:"
    processStmt s1
    putStrLn "Else branch:"
    processStmt s2
  While guard body -> do
    putStrLn $ "The statement is a While loop with guard: " ++ show guard
    putStrLn "Body:"
    processStmt body
  Block vars body -> do
    putStrLn $ "The statement is a Block with variables: " ++ show vars
    processStmt body
  TryCatch e s1 s2 -> do
    putStrLn $ "The statement is a Try-Catch block with exception: " ++ e
    putStrLn "Try block:"
    processStmt s1
    putStrLn "Catch block:"
    processStmt s2
  _ -> putStrLn "Other statement type"

-- Example for how we can run z3
runZ3 :: IO ()
runZ3 = evalZ3 $ do
  -- Create variables x, y, z
  x <- mkFreshIntVar "x"
  y <- mkFreshIntVar "y"
  z <- mkFreshIntVar "z"

  -- Apply the function to x, y, z
  minFunction x y z

  -- Check satisfiability
  result <- check
  liftIO $ putStrLn $ "Satisfiable: " ++ show result

  -- Get model if satisfiable
  case result of
    Sat -> do
      mbModel <- getModel -- getModel returns Maybe Model
      case mbModel of
        (_, Just model) -> do
          modelStr <- modelToString model -- Convert model to string
          liftIO $ putStrLn "Model:"
          liftIO $ putStrLn modelStr
        (_, Nothing) -> liftIO $ putStrLn "No model found"
    _ -> return ()

-- min(x, y | z) in z3
minFunction :: (MonadZ3 m) => AST -> AST -> AST -> m ()
minFunction x y z = do
  -- z := x ;
  assign <- mkEq z x

  -- if y < x then { z := y } else { skip } ;
  cond <- mkLt y x
  thenCase <- mkEq z y
  elseCase <- mkTrue

  -- Combine conditions
  ifThenElse <- mkIte cond thenCase elseCase

  -- assert ((z = x) || (z = y)) && (z <= x) && (z <= y)
  zEqualX <- mkEq z x
  zEqualY <- mkEq z y
  orCondition <- mkOr [zEqualX, zEqualY] -- mkOr returns m AST
  leX <- mkLe z x
  leY <- mkLe z y
  combinedAssert <- mkAnd [orCondition, leX, leY] -- mkAnd returns m AST

  -- Assert the combined condition
  finalAssertion <- mkAnd [assign, ifThenElse, combinedAssert]
  assert finalAssertion