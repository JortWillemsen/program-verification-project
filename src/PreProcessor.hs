module PreProcessor where

import qualified Data.Map as M
import GCLParser.GCLDatatype (BinOp (..), Expr (..), Program (..), Stmt (..))

preprocess :: Program -> Bool -> Bool -> Bool -> Program
preprocess program fold removeUnreach simplify =
  let stmt' = stmt program
      stmt''' = if fold then foldConstantsInStmt stmt' else stmt'
      stmt'''' = if removeUnreach then removeUnreachable stmt''' else stmt'''
      stmt''''' = if simplify then simplifyStmt stmt'''' else stmt''''
   in program {stmt = stmt'''''}

-- Normalize expressions within the program
simplifyExpr :: Expr -> Expr
simplifyExpr (Parens e) = simplifyExpr e -- Remove unnecessary parentheses
simplifyExpr (BinopExpr Plus (LitI 0) e) = simplifyExpr e -- x + 0 -> x
simplifyExpr (BinopExpr Plus e (LitI 0)) = simplifyExpr e -- 0 + x -> x
simplifyExpr (BinopExpr Multiply (LitI 1) e) = simplifyExpr e -- x * 1 -> x
simplifyExpr (BinopExpr Multiply e (LitI 1)) = simplifyExpr e -- 1 * x -> x
simplifyExpr (BinopExpr Multiply (LitI 0) _) = LitI 0 -- x * 0 -> 0
simplifyExpr (BinopExpr Multiply _ (LitI 0)) = LitI 0 -- 0 * x -> 0
simplifyExpr (BinopExpr op e1 e2) = BinopExpr op (simplifyExpr e1) (simplifyExpr e2)
simplifyExpr e = e

simplifyStmt :: Stmt -> Stmt
simplifyStmt Skip = Skip
simplifyStmt (Assign var expr) = Assign var (simplifyExpr expr)
simplifyStmt (Seq s1 s2) = Seq (simplifyStmt s1) (simplifyStmt s2)
simplifyStmt s = s

foldConstants :: Expr -> Expr
foldConstants (BinopExpr Plus (LitI a) (LitI b)) = LitI (a + b)
foldConstants (BinopExpr Minus (LitI a) (LitI b)) = LitI (a - b)
foldConstants (BinopExpr Multiply (LitI a) (LitI b)) = LitI (a * b)
foldConstants (BinopExpr Divide (LitI a) (LitI b)) = LitI (a `div` b)
foldConstants (BinopExpr op e1 e2) = BinopExpr op (foldConstants e1) (foldConstants e2)
foldConstants e = e

foldConstantsInStmt :: Stmt -> Stmt
foldConstantsInStmt Skip = Skip
foldConstantsInStmt (Assign var expr) = Assign var (foldConstants expr)
foldConstantsInStmt (Seq s1 s2) = Seq (foldConstantsInStmt s1) (foldConstantsInStmt s2)
foldConstantsInStmt s = s

removeUnreachable :: Stmt -> Stmt
removeUnreachable Skip = Skip
removeUnreachable (Assert (LitB False)) = Skip -- Anything after assert false is unreachable
removeUnreachable (Seq s1 s2) = case removeUnreachable s1 of
  Skip -> Skip -- Skip the rest of the sequence if s1 is unreachable
  _ -> Seq s1 (removeUnreachable s2)
removeUnreachable s = s

-- Function to unfold while loops a fixed number of times
unfoldWhile :: Int -> Stmt -> Stmt
unfoldWhile 0 (While _ _) = Skip -- Stop unfolding after the limit
unfoldWhile n (While guard body) =
  IfThenElse
    guard
    (Seq body (unfoldWhile (n - 1) (While guard body))) -- unfold one iteration
    Skip -- If the guard is false, exit the loop
unfoldWhile n (Seq s1 s2) = Seq (unfoldWhile n s1) (unfoldWhile n s2) -- Recurse through sequences
unfoldWhile n (Block e s1) = Block e (unfoldWhile n s1)
unfoldWhile _ s = s -- For all other statements, do nothing

-- |
-- | Below is to make sure that variables are unique
-- |
-- |
-- |
-- |
-- |
-- |

-- Function to ensure unique variables for forall
makeUniqueForall :: Program -> Program
makeUniqueForall prog = prog {stmt = renameStmt (stmt prog) M.empty 0}

-- Helper function to rename variables in statements
renameStmt :: Stmt -> M.Map String String -> Int -> Stmt
renameStmt Skip env count = Skip
renameStmt (Assert e) env count = Assert (renameExpr e env count)
renameStmt (Assume e) env count = Assume (renameExpr e env count)
renameStmt (Assign var e) env count = Assign var (renameExpr e env count)
renameStmt (DrefAssign var e) env count = DrefAssign var (renameExpr e env count)
renameStmt (AAssign var i e) env count = AAssign var (renameExpr i env count) (renameExpr e env count)
renameStmt (Seq s1 s2) env count = Seq (renameStmt s1 env count) (renameStmt s2 env (count + 100))
renameStmt (IfThenElse g s1 s2) env count = IfThenElse (renameExpr g env count) (renameStmt s1 env count) (renameStmt s2 env count)
renameStmt (While g s) env count = While (renameExpr g env count) (renameStmt s env count)
renameStmt (Block vars s) env count = Block vars (renameStmt s env count)
renameStmt (TryCatch e s1 s2) env count = TryCatch e (renameStmt s1 env count) (renameStmt s2 env count)

-- Helper function to rename variables in expressions
renameExpr :: Expr -> M.Map String String -> Int -> Expr
renameExpr (Var var) env _ = Var (renameVar var env) -- Rename variable if in env
renameExpr (LitI x) _ _ = LitI x
renameExpr (LitB b) _ _ = LitB b
renameExpr LitNull _ _ = LitNull
renameExpr (Parens e) env count = Parens (renameExpr e env count)
renameExpr (ArrayElem e1 e2) env count = ArrayElem (renameExpr e1 env count) (renameExpr e2 env count)
renameExpr (OpNeg e) env count = OpNeg (renameExpr e env count)
renameExpr (BinopExpr op e1 e2) env count = BinopExpr op (renameExpr e1 env count) (renameExpr e2 env count)
renameExpr (Forall var p) env count =
  let newVar = generateNewVar var count
      newEnv = M.insert var newVar env
   in Forall newVar (renameExpr p newEnv count) -- Rename var in p using newEnv
renameExpr (Exists var p) env count =
  let newVar = generateNewVar var count
      newEnv = M.insert var newVar env
   in Exists newVar (renameExpr p newEnv count) -- Rename var in p using newEnv
renameExpr (SizeOf e) env count = SizeOf (renameExpr e env count)
renameExpr (RepBy e1 e2 e3) env count = RepBy (renameExpr e1 env count) (renameExpr e2 env count) (renameExpr e3 env count)
renameExpr (Cond g e1 e2) env count = Cond (renameExpr g env count) (renameExpr e1 env count) (renameExpr e2 env count)
renameExpr (NewStore e) env count = NewStore (renameExpr e env count)
renameExpr (Dereference var) env _ = Dereference (renameVar var env) -- Rename dereferenced var if in env

-- Helper function to rename variables based on environment
renameVar :: String -> M.Map String String -> String
renameVar var = M.findWithDefault var var

-- Helper function to generate new variable names for forall/exists
generateNewVar :: String -> Int -> String
generateNewVar var count = var ++ "_" ++ show count