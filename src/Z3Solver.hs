module Z3Solver where

import Z3.Monad
import Types (Env, Path (..))
import GCLParser.GCLDatatype (Expr(..), BinOp (..), VarDeclaration (..), Type (..), PrimitiveType (..))
import qualified Data.Map as M
import GCLHelper (getVarName)
import Control.Monad (foldM)
import WlpGenerator (wlp, conjunctive)

buildEnv :: [VarDeclaration] -> Z3 Env
buildEnv = foldM buildEnv' M.empty
  where
    buildEnv' :: Env -> VarDeclaration -> Z3 Env
    buildEnv' env (VarDeclaration str typ) =
      case typ of
        PType prim -> case prim of
          PTInt -> do
            freshInt <- mkFreshIntVar str
            return $ M.insert str freshInt env
          PTBool -> do
            freshBool <- mkFreshBoolVar str
            return $ M.insert str freshBool env
        RefType -> error "We do not support RefType"
        AType prim -> case prim of
          PTInt -> do
            symb <- mkStringSymbol str
            intSort <- mkIntSort
            arraySort <- mkArraySort intSort intSort
            integerArray <- mkVar symb arraySort

            let env' = M.insert str integerArray env
            let sizeName = '#' : str
            sizeInt <- mkFreshIntVar sizeName

            return $ M.insert sizeName sizeInt env'
          PTBool -> do
            symb <- mkStringSymbol str
            intSort <- mkIntSort
            boolSort <- mkBoolSort
            arraySort <- mkArraySort intSort boolSort
            booleanArray <- mkVar symb arraySort

            let env' = M.insert str booleanArray env
            let sizeName = '#' : str
            sizeInt <- mkFreshIntVar sizeName

            return $ M.insert sizeName sizeInt env'

exprToZ3 :: Env -> Expr -> Z3 AST
exprToZ3 env (Var str) = do
  case M.lookup str env of
    Just z3Var -> return z3Var
    Nothing -> error "(Var): Env was not constructed properly"
exprToZ3 _ (LitB b) = mkBool b
exprToZ3 _ (LitI i) = mkIntNum i
exprToZ3 _ LitNull = undefined -- Optional
exprToZ3 env (Parens e) = exprToZ3 env e
exprToZ3 env (ArrayElem array index) = do
    z3Arr <- exprToZ3 env array
    z3Index <- exprToZ3 env index
    --liftIO $ putStrLn $ "We select array " ++ show array
    --liftIO $ putStrLn $ "We select index " ++ show index
    mkSelect z3Arr z3Index
exprToZ3 env (OpNeg e) = exprToZ3 env e >>= mkNot
exprToZ3 env (BinopExpr op e1 e2) = do
  z3e1 <- exprToZ3 env e1 
  z3e2 <- exprToZ3 env e2 
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

  -- where
  --   isRepBy :: Env -> Expr -> Z3 AST
  --   isRepBy env' (RepBy _ _ v) = exprToZ3 env' (updateRepBy v)
  --   isRepBy env' e = exprToZ3 env' (updateRepBy e)

  --   updateRepBy :: Expr -> Expr
  --   updateRepBy (RepBy _ _ v) = v
  --   updateRepBy (BinopExpr op' e1' e2') = BinopExpr op' (updateRepBy e1') (updateRepBy e2')
  --   updateRepBy (OpNeg e) = OpNeg (updateRepBy e)
  --   updateRepBy e = e
exprToZ3 env (Forall str e) = do
  case M.lookup str env of
    Just var -> do
      x <- toApp var
      body <- exprToZ3 env e
      mkForallConst [] [x] body
    Nothing -> error "(Forall): Env was not constructed properly"
exprToZ3 env (Exists str e) = do
  case M.lookup str env of
    Just var -> do
      x <- toApp var
      body <- exprToZ3 env e
      mkExistsConst [] [x] body
    Nothing -> error "(Exists): Env was not constructed properly"
exprToZ3 _ (SizeOf e) = mkStringSymbol ('#' : getVarName e) >>= mkIntVar
exprToZ3 env (RepBy array index value) = do
  e1Z3 <- exprToZ3 env array
  e2Z3 <- exprToZ3 env index
  e3Z3 <- exprToZ3 env value
  mkStore e1Z3 e2Z3 e3Z3
exprToZ3 env (Cond guard ifBranch elseBranch) = do
  e1Z3 <- exprToZ3 env guard
  e2Z3 <- exprToZ3 env ifBranch
  e3Z3 <- exprToZ3 env elseBranch
  mkIte e1Z3 e2Z3 e3Z3
exprToZ3 _ (NewStore _) = undefined -- Optional
exprToZ3 _ (Dereference _) = undefined -- Optional 

-- | This function checks if an expression is satisfiable
isSatisfiableExpr :: Env -> Expr -> Z3 Bool
isSatisfiableExpr env e = do
  verdict <- checker env e
  case verdict of
    Sat -> return True -- Formula is satisfiable
    Unsat -> return False -- Formula is UNsatisfiable
    Undef -> error "Undefined satisfiable result"

isValidPath :: Path -> Z3 Bool
isValidPath (Path stmts env) = do
  let calculatedWlp = wlp stmts
  isValidExpr env calculatedWlp

isSatisfiablePath :: Path -> Z3 Bool
isSatisfiablePath (Path stmts env) = do
  let conj = conjunctive stmts
  isSatisfiableExpr env conj

-- | This function checks if an expression is valid.
isValidExpr :: Env -> Expr -> Z3 Bool
isValidExpr env e = do
  verdict <- inverseChecker env e
  case verdict of
    Sat -> return False -- Formula is NOT valid
    Unsat -> return True -- Formula is valid
    Undef -> error "Undefined satisfiable result"

-- | Converts WLP Expr to Z3 expression and checks its satisfiability
checker :: Env -> Expr -> Z3 Result
checker env e = do
  eZ3 <- exprToZ3 env e
  assert eZ3
  check

-- | Converts WLP Expr to Z3 expression, inverts it and checks its satisfiability (Validity of original WLP Expr)
inverseChecker :: Env -> Expr -> Z3 Result
inverseChecker env e = do
  eZ3 <- exprToZ3 env e
  neZ3 <- mkNot eZ3
  assert neZ3
  check
