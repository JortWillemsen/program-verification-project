module Z3Solver where

import Z3.Monad
import Types (Env, Path (..), Options (..))
import GCLParser.GCLDatatype (Expr(..), BinOp (..), VarDeclaration (..), Type (..), PrimitiveType (..))
import qualified Data.Map as M
import Control.Monad ( foldM )
import WlpGenerator (wlp, conjunctive)
import Control.Monad.Cont (MonadIO(liftIO))
import Debug.Trace (trace)

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

exprToZ3 ::  Expr -> Env -> Z3 AST
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
      error $ "dit hoort niet te gebeuren var " ++ n
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

isValidPath :: Path -> IO Bool
isValidPath (Path stmts env) = do
  let calculatedWlp = wlp stmts
  isValidExpr env calculatedWlp

isSatisfiablePath :: Path -> IO Bool
isSatisfiablePath (Path stmts env) = do
  let conj = conjunctive stmts
  isSatisfiableExpr env conj

-- | This function checks if an expression is satisfiable
isSatisfiableExpr :: Env -> Expr -> IO Bool
isSatisfiableExpr env e = do
  verdict <- evalZ3 $ checker env e
  liftIO $ print verdict
  case verdict of
    Sat -> return True -- Formula is satisfiable
    Unsat -> return False -- Formula is UNsatisfiable
    Undef -> error "Undefined satisfiable result"

-- | This function checks if an expression is valid.
isValidExpr :: Env -> Expr -> IO Bool
isValidExpr env e = do
  verdict <- evalZ3 $ inverseChecker env e
  case verdict of
    Sat -> return False -- Formula is NOT valid
    Unsat -> return True -- Formula is valid
    Undef -> error "Undefined satisfiable result"

-- | Converts WLP Expr to Z3 expression and checks its satisfiability
checker :: Env -> Expr -> Z3 Result
checker env e = do
  eZ3 <- exprToZ3 e env
  liftIO $ putStrLn $ "Made env"
  assert eZ3
  check

-- | Converts WLP Expr to Z3 expression, inverts it and checks its satisfiability (Validity of original WLP Expr)
inverseChecker :: Env -> Expr -> Z3 Result
inverseChecker env e = do
  eZ3 <- exprToZ3 e env
  neZ3 <- mkNot eZ3
  assert neZ3
  check
