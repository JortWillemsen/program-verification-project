module Helper where

import GCLParser.GCLDatatype (Expr (..), Stmt (..))
import Types (PostCondition)

-- | postcondition: Current post condition
-- | string: variable name
-- | expr: thing that goes in the variable
-- |
replaceAssign :: String -> Expr -> PostCondition -> PostCondition
replaceAssign  str e (Var curStr)
  | curStr == str = e
replaceAssign  str e (Parens e1) = Parens (replaceAssign str e e1)
replaceAssign str e (ArrayElem e1 e2) = ArrayElem (replaceAssign str e e1) (replaceAssign str e e2)
replaceAssign str e (OpNeg e1) = OpNeg (replaceAssign str e e1)
replaceAssign str e (BinopExpr op e1 e2) = BinopExpr op (replaceAssign str e e1) (replaceAssign str e e2)
replaceAssign str e (Forall str1 e1) = Forall str1 (replaceAssign str e e1)
replaceAssign str e (Exists str1 e1) = Exists str1 (replaceAssign str e e1)
replaceAssign str e (SizeOf e1) = SizeOf (replaceAssign str e e1)
replaceAssign str e (RepBy e1 e2 e3) = RepBy (replaceAssign str e e1) (replaceAssign str e e2) (replaceAssign str e e3)
replaceAssign str e (Cond e1 e2 e3) = Cond (replaceAssign str e e1) (replaceAssign str e e2) (replaceAssign str e e3)
replaceAssign str e (NewStore e1) = NewStore (replaceAssign str e e1)
replaceAssign  _ _ other = other

replaceAssignStmt :: Stmt -> String -> Expr -> Stmt
replaceAssignStmt Skip _ _ = Skip
replaceAssignStmt (Assert e) str expr = Assert (replaceAssign str expr e)
replaceAssignStmt (Assume e) str expr = Assume (replaceAssign str expr e)
replaceAssignStmt (Assign var e) str expr =
  Assign var (replaceAssign str expr e)
replaceAssignStmt (AAssign arrName indexExpr valueExpr) str expr =
  AAssign arrName (replaceAssign str expr indexExpr) (replaceAssign str expr valueExpr)
replaceAssignStmt (DrefAssign var e) str expr =
  DrefAssign var (replaceAssign str expr e)
replaceAssignStmt (Seq s1 s2) str expr =
  Seq (replaceAssignStmt s1 str expr) (replaceAssignStmt s2 str expr)
replaceAssignStmt (IfThenElse cond s1 s2) str expr =
  IfThenElse (replaceAssign str expr cond) (replaceAssignStmt s1 str expr) (replaceAssignStmt s2 str expr)
replaceAssignStmt (While cond body) str expr =
  While (replaceAssign str expr cond) (replaceAssignStmt body str expr)
replaceAssignStmt (Block decls body) str expr =
  Block decls (replaceAssignStmt body str expr)
replaceAssignStmt (TryCatch e s1 s2) str expr =
  TryCatch e (replaceAssignStmt s1 str expr) (replaceAssignStmt s2 str expr)
