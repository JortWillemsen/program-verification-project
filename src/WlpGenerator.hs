module WlpGenerator where

import qualified GCLParser.GCLDatatype as GCL
import PreProcessor (simplify)
import Types (Statement (..))
import qualified Types as US

-- | Conjuncts all assumes together (For feasibility checks)
conjunctive :: [US.Statement] -> GCL.Expr
conjunctive [] = GCL.LitB True
conjunctive (US.Assume e : ss) = GCL.BinopExpr GCL.And e (conjunctive ss)
conjunctive (US.Assign s e : ss) = replace s e $ conjunctive ss
conjunctive (US.AAssign s e1 e2 : ss) = replace s (GCL.RepBy (GCL.Var s) e1 e2) (conjunctive ss)
conjunctive (_ : ss) = conjunctive ss

-- | Replaces all occurences of a variable using repby
-- | str = name of variable
-- | e   = new value
-- replace :: String -> GCL.Expr -> GCL.Expr -> GCL.Expr
-- replace n e1 (GCL.Var n')
--   | n == n' = e1
--   | otherwise = GCL.Var n'
-- replace n e1 (GCL.Parens e2) = GCL.Parens (replace n e1 e2)
-- replace n e1 (GCL.ArrayElem e2 e3) = GCL.ArrayElem (replace n e1 e2) (replace n e1 e3)
-- replace n e1 (GCL.OpNeg e2) = GCL.OpNeg (replace n e1 e2)
-- replace n e1 (GCL.BinopExpr op e2 e3) = GCL.BinopExpr op (replace n e1 e2) (replace n e1 e3)
-- replace n e1 (GCL.Forall s e2) = GCL.Forall s (replace n e1 e2)
-- replace n e1 (GCL.Exists s e2) = GCL.Exists s (replace n e1 e2)
-- replace _ _  (GCL.SizeOf e2) = GCL.SizeOf e2
-- replace n e1 (GCL.RepBy e2 e3 e4) = GCL.RepBy e2 (replace n e1 e3) (replace n e1 e4)
-- replace n e1 (GCL.Cond e2 e3 e4) = GCL.Cond (replace n e1 e2) (replace n e1 e3) (replace n e1 e4)
-- replace n e1 (GCL.NewStore e2) = GCL.NewStore (replace n e1 e2)
-- replace _ _ e2 = e2
modify :: Statement -> GCL.Expr -> GCL.Expr
modify (Assume e1) = GCL.BinopExpr GCL.Implication e1
modify (Assert e1) = GCL.BinopExpr GCL.And e1
modify (Assign n e1) = replace n e1
modify (AAssign s e1 e2) = replace s (GCL.RepBy (GCL.Var s) e1 e2)

wlp :: [Statement] -> GCL.Expr
wlp = foldr (\i e -> simplify $ modify i e) (GCL.LitB True)

replace :: String -> GCL.Expr -> GCL.Expr -> GCL.Expr
replace n e1 (GCL.Var n')
  | n == n' = e1
  | otherwise = GCL.Var n'
replace n e1 (GCL.Parens e2) = GCL.Parens (replace n e1 e2)
replace n e1 (GCL.ArrayElem e2 e3) = GCL.ArrayElem (replace n e1 e2) (replace n e1 e3)
replace n e1 (GCL.OpNeg e2) = GCL.OpNeg (replace n e1 e2)
replace n e1 (GCL.BinopExpr op e2 e3) = GCL.BinopExpr op (replace n e1 e2) (replace n e1 e3)
replace n e1 (GCL.Forall s e2) = GCL.Forall s (replace n e1 e2)
replace n e1 (GCL.Exists s e2) = GCL.Exists s (replace n e1 e2)
replace n e1 (GCL.SizeOf e2) = GCL.SizeOf (replace n e1 e2)
replace n e1 (GCL.RepBy e2 e3 e4) = GCL.RepBy (replace n e1 e2) (replace n e1 e3) (replace n e1 e4)
replace n e1 (GCL.Cond e2 e3 e4) = GCL.Cond (replace n e1 e2) (replace n e1 e3) (replace n e1 e4)
replace n e1 (GCL.NewStore e2) = GCL.NewStore (replace n e1 e2)
replace n e1 e2 = e2

-- wlp :: [Statement] -> GCL.Expr
-- wlp [] = GCL.LitB True
-- wlp (Assume e : ps) = GCL.BinopExpr GCL.Implication e (wlp ps)
-- wlp (Assert e : ps) = GCL.BinopExpr GCL.And e (wlp ps)
-- wlp (Assign s e : ps) = replace s e $ wlp ps
-- wlp (AAssign s e1 e2 : ps) = replace s (GCL.RepBy (GCL.Var s) e1 e2) (wlp ps)
