module Helper where

import GCLParser.GCLDatatype (Expr (..), Stmt (..))
import Types (PostCondition)

-- | postcondition: Current post condition
-- | string: variable name
-- | expr: thing that goes in the variable
-- |
replaceAssign :: PostCondition -> String -> Expr -> PostCondition
replaceAssign (Var curStr) str e
  | curStr == str = e
replaceAssign (Parens e1) str e = Parens (replaceAssign e1 str e)
replaceAssign (ArrayElem e1 e2) str e = ArrayElem (replaceAssign e1 str e) (replaceAssign e2 str e)
replaceAssign (OpNeg e1) str e = OpNeg (replaceAssign e1 str e)
replaceAssign (BinopExpr op e1 e2) str e = BinopExpr op (replaceAssign e1 str e) (replaceAssign e2 str e)
replaceAssign (Forall str1 e1) str e = Forall str1 (replaceAssign e1 str e)
replaceAssign (Exists str1 e1) str e = Exists str1 (replaceAssign e1 str e)
replaceAssign (SizeOf e1) str e = SizeOf (replaceAssign e1 str e)
replaceAssign (RepBy e1 e2 e3) str e = RepBy (replaceAssign e1 str e) (replaceAssign e2 str e) (replaceAssign e3 str e)
replaceAssign (Cond e1 e2 e3) str e = Cond (replaceAssign e1 str e) (replaceAssign e2 str e) (replaceAssign e3 str e)
replaceAssign (NewStore e1) str e = NewStore (replaceAssign e1 str e)
replaceAssign other _ _ = other

replaceAssignStmt :: Stmt -> String -> Expr -> Stmt
replaceAssignStmt Skip _ _ = Skip
replaceAssignStmt (Assert e) str expr = Assert (replaceAssign e str expr)
replaceAssignStmt (Assume e) str expr = Assume (replaceAssign e str expr)
replaceAssignStmt (Assign var e) str expr =
  Assign var (replaceAssign e str expr)
replaceAssignStmt (AAssign arrName indexExpr valueExpr) str expr =
  AAssign arrName (replaceAssign indexExpr str expr) (replaceAssign valueExpr str expr)
replaceAssignStmt (DrefAssign var e) str expr =
  DrefAssign var (replaceAssign e str expr)
replaceAssignStmt (Seq s1 s2) str expr =
  Seq (replaceAssignStmt s1 str expr) (replaceAssignStmt s2 str expr)
replaceAssignStmt (IfThenElse cond s1 s2) str expr =
  IfThenElse (replaceAssign cond str expr) (replaceAssignStmt s1 str expr) (replaceAssignStmt s2 str expr)
replaceAssignStmt (While cond body) str expr =
  While (replaceAssign cond str expr) (replaceAssignStmt body str expr)
replaceAssignStmt (Block decls body) str expr =
  Block decls (replaceAssignStmt body str expr)
replaceAssignStmt (TryCatch e s1 s2) str expr =
  TryCatch e (replaceAssignStmt s1 str expr) (replaceAssignStmt s2 str expr)
