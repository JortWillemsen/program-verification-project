module PreProcessor where

import GCLParser.GCLDatatype (BinOp (..), Expr (..), Program (..), Stmt (..))

run :: Program -> Program
run program = program {stmt = normalizeStmt (stmt program)}

-- Normalize expressions within the program
normalizeExpr :: Expr -> Expr
normalizeExpr (Parens e) = normalizeExpr e -- Remove unnecessary parentheses
normalizeExpr (BinopExpr Plus (LitI 0) e) = normalizeExpr e -- x + 0 -> x
normalizeExpr (BinopExpr Plus e (LitI 0)) = normalizeExpr e -- 0 + x -> x
normalizeExpr (BinopExpr Multiply (LitI 1) e) = normalizeExpr e -- x * 1 -> x
normalizeExpr (BinopExpr Multiply e (LitI 1)) = normalizeExpr e -- 1 * x -> x
normalizeExpr (BinopExpr Multiply (LitI 0) _) = LitI 0 -- x * 0 -> 0
normalizeExpr (BinopExpr Multiply _ (LitI 0)) = LitI 0 -- 0 * x -> 0
normalizeExpr (BinopExpr op e1 e2) = BinopExpr op (normalizeExpr e1) (normalizeExpr e2)
normalizeExpr e = e

normalizeStmt :: Stmt -> Stmt
normalizeStmt Skip = Skip
normalizeStmt (Assign var expr) = Assign var (normalizeExpr expr)
normalizeStmt (Seq s1 s2) = Seq (normalizeStmt s1) (normalizeStmt s2)
normalizeStmt s = s

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