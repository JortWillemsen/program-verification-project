module Z3Solver where

import Control.Monad (foldM)
import qualified Data.Map as M
import GCLParser.GCLDatatype (BinOp (..), Expr (..), PrimitiveType (..), Stmt (..), Type (..), VarDeclaration (VarDeclaration))
import Types (Env, Path (Path), Statement (Decl))
import Z3.Monad
  ( AST,
    MonadZ3,
    mkAdd,
    mkAnd,
    mkArraySort,
    mkBoolSort,
    mkDiv,
    mkEq,
    mkExistsConst,
    mkFalse,
    mkForall,
    mkForallConst,
    mkFreshBoolVar,
    mkFreshConst,
    mkFreshIntVar,
    mkFreshVar,
    mkGe,
    mkGt,
    mkImplies,
    mkIntNum,
    mkIntSort,
    mkInteger,
    mkIte,
    mkLe,
    mkLt,
    mkMul,
    mkNot,
    mkOr,
    mkSelect,
    mkStore,
    mkStringSymbol,
    mkSub,
    mkTrue,
    toApp,
  )

buildEnv :: (MonadZ3 z3) => [Statement] -> [VarDeclaration] -> z3 Env
buildEnv path decls = do
  let declarations = decls ++ getVarDeclarations path
  foldM (flip addDeclToEnv) M.empty declarations

-- Add a single variable declaration to the environment
addDeclToEnv :: (MonadZ3 z3) => VarDeclaration -> Env -> z3 Env
addDeclToEnv (VarDeclaration str typ) env = case typ of
  PType prim -> case prim of
    -- Create an integer variable
    PTInt -> do
      freshInt <- mkFreshIntVar str
      return $ M.insert str freshInt env
    -- Create a boolean variable
    PTBool -> do
      freshBool <- mkFreshBoolVar str
      return $ M.insert str freshBool env
  -- Handle reference types here if needed
  RefType -> undefined
  -- Handle array types
  AType prim -> case prim of
    -- Create an integer array
    PTInt -> do
      domain <- mkIntSort
      range <- mkIntSort
      arraySort <- mkArraySort domain range
      freshArray <- mkFreshConst str arraySort

      let sizeName = '#' : str
      sizeInt <- mkFreshIntVar sizeName

      return $ M.insert str freshArray $ M.insert sizeName sizeInt env
    -- Create a boolean array
    PTBool -> do
      domain <- mkIntSort
      range <- mkBoolSort

      let sizeName = '#' : str
      sizeInt <- mkFreshIntVar sizeName

      arraySort <- mkArraySort domain range
      freshArray <- mkFreshConst str arraySort
      return $ M.insert str freshArray $ M.insert sizeName sizeInt env

-- Build the environment from the given expression
-- addExprToEnv :: (MonadZ3 z3) => Expr -> Env -> z3 Env
-- addExprToEnv LitNull cEnv = return cEnv
-- addExprToEnv (Forall str e) cEnv = case M.lookup str cEnv of
--   Just _ -> addExprToEnv e cEnv
--   Nothing -> do
--     freshVar <- mkFreshIntVar str
--     addExprToEnv e $ M.insert str freshVar cEnv
-- addExprToEnv (Exists str e) cEnv = case M.lookup str cEnv of
--   Just _ -> addExprToEnv e cEnv
--   Nothing -> do
--     freshVar <- mkFreshIntVar str
--     addExprToEnv e $ M.insert str freshVar cEnv
-- addExprToEnv (Var n) cEnv = case M.lookup n cEnv of
--   Just _ -> return cEnv
--   Nothing -> do
--     freshVar <- mkFreshIntVar n
--     return $ M.insert n freshVar cEnv
-- addExprToEnv (ArrayElem e1 i) cEnv = do
--   env' <- addExprToEnv e1 cEnv
--   addExprToEnv i env'
-- addExprToEnv (Parens e) cEnv = addExprToEnv e cEnv
-- addExprToEnv (OpNeg e) cEnv = addExprToEnv e cEnv
-- addExprToEnv (BinopExpr _ e1 e2) cEnv = do
--   env1 <- addExprToEnv e1 cEnv
--   addExprToEnv e2 env1
-- addExprToEnv (RepBy arr i v) cEnv = do
--   env1 <- addExprToEnv arr cEnv
--   env2 <- addExprToEnv i env1
--   addExprToEnv v env2
-- addExprToEnv (Cond g e1 e2) cEnv = do
--   env1 <- addExprToEnv g cEnv
--   env2 <- addExprToEnv e1 env1
--   addExprToEnv e2 env2
-- addExprToEnv (SizeOf e) cEnv = addExprToEnv e cEnv
-- addExprToEnv (NewStore e) cEnv = addExprToEnv e cEnv
-- addExprToEnv (Dereference _) cEnv = return cEnv -- Handle as needed or leave it as is
-- addExprToEnv (LitI _) cEnv = return cEnv
-- addExprToEnv (LitB _) cEnv = return cEnv

getVarDeclarations :: [Statement] -> [VarDeclaration]
getVarDeclarations [] = []
getVarDeclarations ((Decl decls) : xs) = decls ++ getVarDeclarations xs
getVarDeclarations (x : xs) = getVarDeclarations xs

exprToZ3 :: (MonadZ3 z3) => Expr -> Env -> z3 AST
exprToZ3 LitNull env = undefined -- optional
exprToZ3 (Forall str e) env = case M.lookup str env of
  Just z3var -> do
    z3Expr <- exprToZ3 e env
    intSort <- mkIntSort
    symbol <- mkStringSymbol str
    -- [Pattern] -> [Symbol] -> [Sort] -> AST -> z3 AST
    mkForall [] [symbol] [intSort] z3Expr -- Use the App for bound variable
  Nothing -> error "dit hoort niet te gebeuren"
exprToZ3 (Parens e) env = exprToZ3 e env
exprToZ3 (ArrayElem e1 i) env = do
  z3Index <- exprToZ3 i env
  z3Arr <- exprToZ3 e1 env
  mkSelect z3Arr z3Index
exprToZ3 (OpNeg e) env = do
  z3Expr <- exprToZ3 e env
  mkNot z3Expr
exprToZ3 (Exists str e) env = do
  qVar <- mkFreshIntVar str
  app <- toApp qVar
  z3Expr <- exprToZ3 e env
  mkExistsConst [] [app] z3Expr -- Faulty
exprToZ3 (SizeOf e) env = do
  let str = '#' : findStr e
  case M.lookup str env of
    Just z3Var -> return z3Var -- Return existing Z3 variable if found
    Nothing -> do
      error $ "dit hoort niet te gebeuren #" ++ show str
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

findStr :: Expr -> String
findStr (Var n) = n
findStr (Parens e) = findStr e
findStr (OpNeg e) = findStr e
findStr (RepBy e _ _) = findStr e