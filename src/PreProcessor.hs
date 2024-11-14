module PreProcessor where

import qualified GCLParser.GCLDatatype as GCL

-- | Simplify the wlp
simplify :: GCL.Expr -> GCL.Expr
simplify (GCL.Parens e) = simplify e
simplify (GCL.ArrayElem e1 e2) = GCL.ArrayElem (simplify e1) (simplify e2)
simplify (GCL.OpNeg e) = GCL.OpNeg $ simplify e
simplify (GCL.BinopExpr GCL.Plus (GCL.BinopExpr GCL.Plus e1 (GCL.LitI a)) (GCL.LitI b)) = simplifyBinop GCL.Plus (simplify e1) (GCL.LitI $ a + b) -- (x + 1) + 1 = x + 2
simplify (GCL.BinopExpr GCL.Minus (GCL.BinopExpr GCL.Minus e1 (GCL.LitI a)) (GCL.LitI b)) = simplifyBinop GCL.Minus (simplify e1) (GCL.LitI $ a + b) -- (x - 1) - 1 = x - 2
simplify (GCL.BinopExpr GCL.Implication e1 (GCL.BinopExpr GCL.Implication e2 e3)) = simplifyBinop GCL.Implication (simplifyBinop GCL.And (simplify e1) (simplify e2)) (simplify e3) -- A -> B -> C == A ^ B -> C
simplify (GCL.BinopExpr o e1 e2) = simplifyBinop o (simplify e1) (simplify e2)
simplify (GCL.Forall s e) = GCL.Forall s $ simplify e
simplify (GCL.Exists s e) = GCL.Exists s $ simplify e
simplify (GCL.SizeOf e) = GCL.SizeOf $ simplify e
simplify (GCL.RepBy e1 e2 e3) = GCL.RepBy (simplify e1) (simplify e2) (simplify e3)
simplify (GCL.Cond e1 e2 e3) = GCL.Cond (simplify e1) (simplify e2) (simplify e3)
simplify (GCL.NewStore e) = GCL.NewStore $ simplify e
simplify e = e

-- | Simplify a binary operation
simplifyBinop :: GCL.BinOp -> GCL.Expr -> GCL.Expr -> GCL.Expr
-- shortcut implication
simplifyBinop GCL.Implication (GCL.LitB True) e2 = e2
simplifyBinop GCL.Implication (GCL.LitB False) e2 = GCL.LitB True
simplifyBinop GCL.Implication e1 (GCL.LitB True) = GCL.LitB True
-- shortcut and
simplifyBinop GCL.And (GCL.LitB True) e2 = e2
simplifyBinop GCL.And e1 (GCL.LitB True) = e1
simplifyBinop GCL.And (GCL.LitB False) e2 = GCL.LitB False
simplifyBinop GCL.And e1 (GCL.LitB False) = GCL.LitB False
-- shortcut or
simplifyBinop GCL.Or (GCL.LitB False) e2 = e2
simplifyBinop GCL.Or e1 (GCL.LitB False) = e1
simplifyBinop GCL.Or (GCL.LitB True) e2 = GCL.LitB True
simplifyBinop GCL.Or e1 (GCL.LitB True) = GCL.LitB True
-- apply comparisons
simplifyBinop GCL.LessThan (GCL.LitI a) (GCL.LitI b) = GCL.LitB $ a < b
simplifyBinop GCL.LessThanEqual (GCL.LitI a) (GCL.LitI b) = GCL.LitB $ a <= b
simplifyBinop GCL.GreaterThan (GCL.LitI a) (GCL.LitI b) = GCL.LitB $ a > b
simplifyBinop GCL.GreaterThanEqual (GCL.LitI a) (GCL.LitI b) = GCL.LitB $ a >= b
simplifyBinop GCL.Equal (GCL.LitI a) (GCL.LitI b) = GCL.LitB $ a == b
-- apply math ops
simplifyBinop GCL.Minus (GCL.LitI a) (GCL.LitI b) = GCL.LitI $ a - b
simplifyBinop GCL.Plus (GCL.LitI a) (GCL.LitI b) = GCL.LitI $ a + b
simplifyBinop GCL.Multiply (GCL.LitI a) (GCL.LitI b) = GCL.LitI $ a * b
-- don't do the rest
simplifyBinop o e1 e2 = GCL.BinopExpr o e1 e2
