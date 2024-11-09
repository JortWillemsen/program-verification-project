module Helper where

import GCLParser.GCLDatatype (Expr (..), Stmt (..))
import Types (PostCondition)

-- | postcondition: Current post condition
-- | string: variable name
-- | expr: thing that goes in the variable
-- |


replace :: String -> Expr -> Expr -> Expr
replace n e1 (Var n')
  | n == n' = e1
  | otherwise = Var n'
replace n e1 (Parens e2) = Parens (replace n e1 e2)
replace n e1 (ArrayElem e2 e3) = ArrayElem (replace n e1 e2) (replace n e1 e3)
replace n e1 (OpNeg e2) = OpNeg (replace n e1 e2)
replace n e1 (BinopExpr op e2 e3) = BinopExpr op (replace n e1 e2) (replace n e1 e3)
replace n e1 (Forall s e2) = Forall s (replace n e1 e2)
replace n e1 (Exists s e2) = Exists s (replace n e1 e2)
replace n e1 (SizeOf e2) = SizeOf (replace n e1 e2)
replace n e1 (RepBy e2 e3 e4) = RepBy (replace n e1 e2) (replace n e1 e3) (replace n e1 e4)
replace n e1 (Cond e2 e3 e4) = Cond (replace n e1 e2) (replace n e1 e3) (replace n e1 e4)
replace n e1 (NewStore e2) = NewStore (replace n e1 e2)
replace n e1 e2 = e2


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
