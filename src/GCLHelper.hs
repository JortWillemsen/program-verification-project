module GCLHelper where

import qualified Data.Map as M
import GCLParser.GCLDatatype (Expr (..), Program (..), Stmt (..), VarDeclaration (..), Type (..), PrimitiveType (..))

-- Replace Experiment Parameter
replaceExperimentParam :: Program -> String -> Int -> Program
replaceExperimentParam prog s n = prog {stmt = replaceInStmt (stmt prog) s n}

-- Helper function to replace variable in statements
replaceInStmt :: Stmt -> String -> Int -> Stmt
replaceInStmt Skip _ _ = Skip
replaceInStmt (Assert e) s n = Assert (replaceInExpr e s n)
replaceInStmt (Assume e) s n = Assume (replaceInExpr e s n)
replaceInStmt (Assign var e) s n = Assign var (replaceInExpr e s n)
replaceInStmt (DrefAssign var e) s n = DrefAssign var (replaceInExpr e s n)
replaceInStmt (AAssign var i e) s n = AAssign var (replaceInExpr i s n) (replaceInExpr e s n)
replaceInStmt (Seq s1 s2) s n = Seq (replaceInStmt s1 s n) (replaceInStmt s2 s n)
replaceInStmt (IfThenElse g s1 s2) s n = IfThenElse (replaceInExpr g s n) (replaceInStmt s1 s n) (replaceInStmt s2 s n)
replaceInStmt (While g s') s n = While (replaceInExpr g s n) (replaceInStmt s' s n)
replaceInStmt (Block vars s') s n = Block vars (replaceInStmt s' s n)
replaceInStmt (TryCatch e s1 s2) s n = TryCatch e (replaceInStmt s1 s n) (replaceInStmt s2 s n)

-- Helper function to replace variable in expressions
replaceInExpr :: Expr -> String -> Int -> Expr
replaceInExpr var@(Var v) s n
  | v == s = LitI n
  | otherwise = var
replaceInExpr (Parens e) s n = Parens (replaceInExpr e s n)
replaceInExpr (ArrayElem e1 e2) s n = ArrayElem (replaceInExpr e1 s n) (replaceInExpr e2 s n)
replaceInExpr (OpNeg e) s n = OpNeg (replaceInExpr e s n)
replaceInExpr (BinopExpr op e1 e2) s n = BinopExpr op (replaceInExpr e1 s n) (replaceInExpr e2 s n)
replaceInExpr (Forall v e) s n = Forall v (replaceInExpr e s n)
replaceInExpr (Exists v e) s n = Exists v (replaceInExpr e s n)
replaceInExpr (SizeOf e) s n = SizeOf (replaceInExpr e s n)
replaceInExpr (RepBy e1 e2 e3) s n = RepBy (replaceInExpr e1 s n) (replaceInExpr e2 s n) (replaceInExpr e3 s n)
replaceInExpr (Cond e1 e2 e3) s n = Cond (replaceInExpr e1 s n) (replaceInExpr e2 s n) (replaceInExpr e3 s n)
replaceInExpr (NewStore _) _ _ = error "New store size counting not implemented (optional)"
replaceInExpr (Dereference _) _ _ = error "Dereference counting not implemented (optional)"
replaceInExpr e _ _ = e

-- Function to ensure unique variables for forall and exists
makeUnique :: Program -> Program
makeUnique prog = prog {stmt = renameStmt (stmt prog) M.empty 0}

-- Helper function to rename variables in statements
renameStmt :: Stmt -> M.Map String String -> Int -> Stmt
renameStmt Skip _ _ = Skip
renameStmt (Assert e) env count = Assert (renameExpr e env count)
renameStmt (Assume e) env count = Assume (renameExpr e env count)
renameStmt (Assign var e) env count = Assign var (renameExpr e env count)
renameStmt (DrefAssign var e) env count = DrefAssign var (renameExpr e env count)
renameStmt (AAssign var i e) env count = AAssign var (renameExpr i env count) (renameExpr e env count)
renameStmt (Seq s1 s2) env count = Seq (renameStmt s1 env count) (renameStmt s2 env (count + 1))
renameStmt (IfThenElse g s1 s2) env count = IfThenElse (renameExpr g env count) (renameStmt s1 env count) (renameStmt s2 env count)
renameStmt (While g s) env count = While (renameExpr g env count) (renameStmt s env count)
renameStmt (Block vars s) env count = Block vars (renameStmt s env count)
renameStmt (TryCatch e s1 s2) env count = TryCatch e (renameStmt s1 env count) (renameStmt s2 env count)

-- Helper function to rename variables in expressions
renameExpr :: Expr -> M.Map String String -> Int -> Expr
renameExpr (Var var) env _ = Var (renameVar var env)
renameExpr (LitI x) _ _ = LitI x
renameExpr (LitB b) _ _ = LitB b
renameExpr LitNull _ _ = LitNull
renameExpr (Parens e) env count = Parens (renameExpr e env count)
renameExpr (ArrayElem e1 e2) env count = ArrayElem (renameExpr e1 env count) (renameExpr e2 env count)
renameExpr (OpNeg e) env count = OpNeg (renameExpr e env count)
renameExpr (BinopExpr op e1 e2) env count = BinopExpr op (renameExpr e1 env count) (renameExpr e2 env count)
renameExpr (Forall var p) env count = Forall (generateNewVar var count) (renameExpr p (modifyEnv env var count) count)
renameExpr (Exists var p) env count = Exists (generateNewVar var count) (renameExpr p (modifyEnv env var count) count)
renameExpr (SizeOf e) env count = SizeOf (renameExpr e env count)
renameExpr (RepBy e1 e2 e3) env count = RepBy (renameExpr e1 env count) (renameExpr e2 env count) (renameExpr e3 env count)
renameExpr (Cond g e1 e2) env count = Cond (renameExpr g env count) (renameExpr e1 env count) (renameExpr e2 env count)
renameExpr (NewStore e) env count = NewStore (renameExpr e env count)
renameExpr (Dereference var) env _ = Dereference (renameVar var env)

-- Helper function to rename variables based on the environment
renameVar :: String -> M.Map String String -> String
renameVar var = M.findWithDefault var var

-- Helper function to generate unique variable names
generateNewVar :: String -> Int -> String
generateNewVar var count = var ++ "_" ++ show count

-- Helper function to update environment with new variable mapping
modifyEnv :: M.Map String String -> String -> Int -> M.Map String String
modifyEnv env var count = M.insert var (generateNewVar var count) env

-- | Get variable name of if type Var
getVarName :: Expr -> String
getVarName (Var x) = x
getVarName (Parens e) = getVarName e
getVarName _ = error "Varname was not found"

getVarDeclarationsProgram :: Program -> [VarDeclaration]
getVarDeclarationsProgram program = getVarDeclarationsStmt (stmt program) ++ input program ++ output program

getVarDeclarationsStmt :: Stmt -> [VarDeclaration]
getVarDeclarationsStmt Skip = []
getVarDeclarationsStmt (Assert e) = getVarsDeclarationsExpr e
getVarDeclarationsStmt (Assume e) = getVarsDeclarationsExpr e
getVarDeclarationsStmt (Assign _ _) = []
getVarDeclarationsStmt (DrefAssign _ _) = []
getVarDeclarationsStmt (AAssign {}) = []
getVarDeclarationsStmt (Seq s1 s2) = getVarDeclarationsStmt s1 ++ getVarDeclarationsStmt s2
getVarDeclarationsStmt (IfThenElse _ s1 s2) = getVarDeclarationsStmt s1 ++ getVarDeclarationsStmt s2
getVarDeclarationsStmt (While _ s) = getVarDeclarationsStmt s
getVarDeclarationsStmt (Block vars s) = vars ++ getVarDeclarationsStmt s
getVarDeclarationsStmt (TryCatch _ s1 s2) = getVarDeclarationsStmt s1 ++ getVarDeclarationsStmt s2

getVarsDeclarationsExpr :: Expr -> [VarDeclaration]
getVarsDeclarationsExpr (Parens e) = getVarsDeclarationsExpr e
getVarsDeclarationsExpr (Forall str e) = VarDeclaration str (PType PTInt) : getVarsDeclarationsExpr e
getVarsDeclarationsExpr (Exists str e) = VarDeclaration str (PType PTInt) : getVarsDeclarationsExpr e
getVarsDeclarationsExpr (BinopExpr _ e1 e2) = getVarsDeclarationsExpr e1 ++ getVarsDeclarationsExpr e2
getVarsDeclarationsExpr _ = []
