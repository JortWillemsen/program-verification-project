module Z3Solver where

import Z3.Monad
import Types (Env, Path)
import GCLParser.GCLDatatype (Expr(..), BinOp (..), VarDeclaration (..), Type (..), PrimitiveType (..))
import qualified Data.Map as M
import WlpGenerator (wlp, conjunctive)
import PreProcessor as PP
import GHC.IO (unsafePerformIO)
import GCLHelper (getVarName)
import qualified PreProcessor as PP

buildEnv :: [VarDeclaration] -> Env
buildEnv = foldl buildEnv' M.empty
  where
    buildEnv' :: Env -> VarDeclaration -> Env
    buildEnv' env (VarDeclaration str typ) = 
      case typ of
        PType _ -> M.insert str typ env
        RefType -> error "We do not support RefType"
        AType _ -> do
            let sizeName = '#' : str
            let env' = M.insert str typ env
          
            M.insert sizeName (PType PTInt) env'

exprToZ3 :: Env -> Expr -> Z3 AST
exprToZ3 env (Var s) = case M.lookup s env of
  (Just (PType PTInt)) -> mkStringSymbol s >>= mkIntVar
  (Just (PType PTBool)) -> mkStringSymbol s >>= mkBoolVar
  (Just (AType PTInt)) -> do
    symb <- mkStringSymbol s
    intSort <- mkIntSort
    arraySort <- mkArraySort intSort intSort
    mkVar symb arraySort
  (Just (AType PTBool)) -> do
    symb <- mkStringSymbol s
    intSort <- mkIntSort
    boolSort <- mkBoolSort
    arraySort <- mkArraySort intSort boolSort
    mkVar symb arraySort
  (Just (_)) -> error "Z3Solver: RefType not supported (yet)"
  Nothing -> error $ "VarZ3Solver: Var type not found: " ++ s
exprToZ3 _ (LitI i) = mkIntNum i -- is mkIntNum correct? requires integral
exprToZ3 _ (LitB b) = mkBool b
exprToZ3 _ LitNull = undefined
exprToZ3 env (Parens e) = exprToZ3 env e
exprToZ3 env (ArrayElem varE indexE) = do
  e1Z3 <- exprToZ3 env varE
  e2Z3 <- exprToZ3 env indexE
  mkSelect e1Z3 e2Z3
exprToZ3 env (OpNeg e) = exprToZ3 env e >>= mkNot
exprToZ3 env (BinopExpr op e1 e2) = do
  e1Z3 <- isRepBy env e1
  e2Z3 <- isRepBy env e2
  case op of
    And -> mkAnd [e1Z3, e2Z3]
    Or -> mkOr [e1Z3, e2Z3]
    Implication -> mkImplies e1Z3 e2Z3
    LessThan -> mkLt e1Z3 e2Z3
    LessThanEqual -> mkLe e1Z3 e2Z3
    GreaterThan -> mkGt e1Z3 e2Z3
    GreaterThanEqual -> mkGe e1Z3 e2Z3
    Equal -> mkEq e1Z3 e2Z3
    Minus -> mkSub [e1Z3, e2Z3]
    Plus -> mkAdd [e1Z3, e2Z3]
    Multiply -> mkMul [e1Z3, e2Z3]
    Divide -> mkDiv e1Z3 e2Z3
    Alias -> mkEq e1Z3 e2Z3
  where
    isRepBy :: Env -> Expr -> Z3 AST
    isRepBy env' (RepBy _ _ v) = exprToZ3 env' (updateRepBy v)
    isRepBy env' e = exprToZ3 env' (updateRepBy e)

    updateRepBy :: Expr -> Expr
    updateRepBy (RepBy _ _ v) = v
    updateRepBy (BinopExpr op' e1' e2') = BinopExpr op' (updateRepBy e1') (updateRepBy e2')
    updateRepBy (OpNeg e) = OpNeg (updateRepBy e)
    updateRepBy e = e
exprToZ3 env (Forall x e) = do
  x' <- mkFreshIntVar x
  x'' <- toApp x'
  body <- exprToZ3 env e
  mkForallConst [] [x''] body
exprToZ3 env (Exists x e) = do
  x' <- mkFreshIntVar x
  x'' <- toApp x'
  body <- exprToZ3 env e
  mkExistsConst [] [x''] body
exprToZ3 _ (SizeOf e) = mkStringSymbol ('#' : getVarName e) >>= mkIntVar
exprToZ3 env (RepBy var index value) = do
  e1Z3 <- exprToZ3 env var
  e2Z3 <- exprToZ3 env index
  e3Z3 <- exprToZ3 env value
  mkStore e1Z3 e2Z3 e3Z3
exprToZ3 env (Cond guard trueCon falseCon) = do
  guardZ3 <- exprToZ3 env guard
  trueZ3 <- exprToZ3 env trueCon
  falseZ3 <- exprToZ3 env falseCon
  mkIte guardZ3 trueZ3 falseZ3
exprToZ3 _ (NewStore _) = error "new store not defined yet"
exprToZ3 _ (Dereference _) = error " dereference (optional)"


findStr :: Expr -> String
findStr (Var n) = n
findStr (Parens e) = findStr e
findStr (OpNeg e) = findStr e
findStr (RepBy e _ _) = findStr e
findStr _ = error "This is not supported"

isValidPath :: Env -> Path -> IO Bool
isValidPath env path = do
  let calculatedWlp = PP.simplify $ wlp path
  return $ isValidExpr env calculatedWlp

isSatisfiablePath :: Env -> Path -> IO Bool
isSatisfiablePath env path = do
  let conj = PP.simplify $ conjunctive path
  isSatisfiableExpr env conj

-- | Checks if an expression is satisfiable without exposing IO in the return type
isSatisfiableExpr :: Env -> Expr -> IO Bool
isSatisfiableExpr env e = do
  verdict <- evalZ3 $ checker env e
  return $ case verdict of
    Sat -> True -- Formula is satisfiable
    Unsat -> False -- Formula is UNsatisfiable
    Undef -> error "Undefined satisfiable result"

-- | This function checks if an expression is valid.
isValidExpr :: Env -> Expr -> Bool
isValidExpr env e = unsafePerformIO $ do
  verdict <- evalZ3 $ inverseChecker env e
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
