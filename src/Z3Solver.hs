module Z3Solver where

import qualified Data.Map as M
import GCLParser.GCLDatatype (BinOp (..), Expr (..))
import Z3.Monad

type Env = M.Map String AST

createEnv :: (MonadZ3 z3) => Expr -> Env -> z3 Env
createEnv LitNull cEnv = return cEnv
createEnv (Forall str e) cEnv = case M.lookup str cEnv of
  Just _ -> createEnv e cEnv
  Nothing -> do
    freshVar <- mkFreshIntVar str
    createEnv e $ M.insert str freshVar cEnv
createEnv (Exists str e) cEnv = case M.lookup str cEnv of
  Just _ -> createEnv e cEnv
  Nothing -> do
    freshVar <- mkFreshIntVar str
    createEnv e $ M.insert str freshVar cEnv
createEnv (Var n) cEnv = case M.lookup n cEnv of
  Just _ -> return cEnv
  Nothing -> do
    freshVar <- mkFreshIntVar n
    return $ M.insert n freshVar cEnv
createEnv (ArrayElem e1 i) cEnv = do
  env' <- createEnv e1 cEnv
  createEnv i env'
createEnv (Parens e) cEnv = createEnv e cEnv
createEnv (OpNeg e) cEnv = createEnv e cEnv
createEnv (BinopExpr _ e1 e2) cEnv = do
  env1 <- createEnv e1 cEnv
  createEnv e2 env1
createEnv (RepBy arr i v) cEnv = do
  env1 <- createEnv arr cEnv
  env2 <- createEnv i env1
  createEnv v env2
createEnv (Cond g e1 e2) cEnv = do
  env1 <- createEnv g cEnv
  env2 <- createEnv e1 env1
  createEnv e2 env2
createEnv (SizeOf e) cEnv = createEnv e cEnv
createEnv (NewStore e) cEnv = createEnv e cEnv
createEnv (Dereference _) cEnv = return cEnv -- Handle as needed or leave it as is
createEnv (LitI _) cEnv = return cEnv
createEnv (LitB _) cEnv = return cEnv

exprToZ3 :: (MonadZ3 z3) => Expr -> Env -> z3 AST
exprToZ3 LitNull env = undefined -- optional
exprToZ3 (Forall str e) env = case M.lookup str env of
  Just z3var -> do
    pat <- mkPattern [z3var]
    z3Expr <- exprToZ3 e env
    mkForallConst [pat] [] z3Expr
  Nothing -> do
    error "dit hoort niet te gebeuren"
exprToZ3 (Parens e) env = exprToZ3 e env
exprToZ3 (ArrayElem e1 i) env = do
  z3Arr <- exprToZ3 e1 env
  z3Index <- exprToZ3 i env
  mkSelect z3Arr z3Index
exprToZ3 (OpNeg e) env = do
  z3Expr <- exprToZ3 e env
  mkNot z3Expr 
exprToZ3 (Exists str e) env = do
  qVar <- mkFreshIntVar str
  pat <- mkPattern [qVar]
  z3Expr <- exprToZ3 e env
  mkExistsConst [pat] [] z3Expr
exprToZ3 (SizeOf e) env = mkIntNum $ sizeOfExpr e
exprToZ3 (RepBy arr i v) env = do
  arrZ3 <- exprToZ3 arr env
  indexZ3 <- exprToZ3 i env
  valueZ3 <- exprToZ3 v env
  mkStore arrZ3 indexZ3 valueZ3
exprToZ3 (Cond g e1 e2) env = do
  guardZ3 <- exprToZ3 g env
  thenZ3 <- exprToZ3 e1 env
  elseZ3 <- exprToZ3 e2 env
  mkIte guardZ3 thenZ3 elseZ3
exprToZ3 (NewStore e) env = undefined -- optional
exprToZ3 (Dereference str) env = undefined -- optional
exprToZ3 (LitI i) env = mkInteger (toInteger i)
exprToZ3 (LitB True) env = mkTrue
exprToZ3 (LitB False) env = mkFalse
exprToZ3 (Var n) env = do
  case M.lookup n env of
    Just z3Var -> return z3Var -- Return existing Z3 variable if found
    Nothing -> do
      error "dit hoort niet te gebeuren (Var n)"
exprToZ3 (BinopExpr op e1 e2) env = do
  z3e1 <- exprToZ3 e1 env
  z3e2 <- exprToZ3 e2 env
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
