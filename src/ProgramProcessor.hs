{-# LANGUAGE AllowAmbiguousTypes #-}

module ProgramProcessor where

import GCLParser.GCLDatatype (BinOp (..), Expr (..), PrimitiveType (..), Program (..), Type (..), VarDeclaration (..))
import Helper (replaceAssign, replace)
import Types (Path (Path), PostCondition, Statement (..))

negateExpr :: Expr -> Expr
negateExpr = OpNeg

-- wlp :: Stmt -> PostCondition -> PostCondition
-- wlp Skip pc = pc
-- wlp (Assert e) pc = BinopExpr And e pc
-- wlp (Assume e) pc = BinopExpr Implication e pc
-- wlp (Assign str e) pc = replaceAssign pc str e
-- wlp (AAssign str e1 e2) pc = wlp (Assign str (RepBy (Var str) e1 e2)) pc
-- wlp (DrefAssign s e) pc = undefined -- optional
-- wlp (Seq s1 s2) pc = wlp s1 (wlp s2 pc)
-- wlp (IfThenElse e s1 s2) pc =
--   BinopExpr
--     Or
--     (BinopExpr And e (wlp s1 pc))
--     (BinopExpr And (OpNeg e) (wlp s2 pc))
-- wlp s@(While {}) pc = reduceLoop s pc
-- wlp (Block decls s) pc = wlp s pc -- must we append the vars here? I dont think so
-- wlp (TryCatch e s1 s2) pc =
--   BinopExpr
--     Or
--     (wlp s1 pc) -- No exception
--     (wlp (replaceAssignStmt s2 e (Var e)) pc) -- We do have an exception

wlp :: Path -> Expr
wlp (Path stmts _) = simplify $ wlp' stmts
  where
    wlp' [] = LitB True
    wlp' ((Assert e) : xs) = BinopExpr And e (wlp' xs)
    wlp' ((Assume e) : xs) = BinopExpr Implication e (wlp' xs)
    wlp' ((Assign str e) : xs) = replace str e (wlp' xs)
    wlp' ((AAssign str e1 e2) : xs) = replace str (RepBy (Var str) e1 e2) (wlp' xs)
    wlp' ((Decl x) : xs) = wlp' xs

-- | Simplify the wlp
simplify :: Expr -> Expr
simplify (Parens e) = simplify e
simplify (ArrayElem e1 e2) = ArrayElem (simplify e1) (simplify e2)
simplify (OpNeg e) = OpNeg $ simplify e
simplify (BinopExpr Plus (BinopExpr Plus e1 (LitI a)) (LitI b)) = simplify $ simplifyBinop Plus (simplify e1) (LitI $ a + b) -- (x + 1) + 1 = x + 2
simplify (BinopExpr Minus (BinopExpr Minus e1 (LitI a)) (LitI b)) = simplify $ simplifyBinop Minus (simplify e1) (LitI $ a + b) -- (x - 1) - 1 = x - 2
simplify (BinopExpr Implication e1 (BinopExpr Implication e2 e3)) = simplify $ simplifyBinop Implication (simplifyBinop And (simplify e1) (simplify e2)) (simplify e3) -- A -> B -> C == A ^ B -> C
simplify (BinopExpr o e1 e2) = simplifyBinop o (simplify e1) (simplify e2)
simplify (Forall s e) = Forall s $ simplify e
simplify (Exists s e) = Exists s $ simplify e
simplify (SizeOf e) = SizeOf $ simplify e
simplify (RepBy e1 e2 e3) = RepBy (simplify e1) (simplify e2) (simplify e3)
simplify (Cond e1 e2 e3) = Cond (simplify e1) (simplify e2) (simplify e3)
simplify (NewStore e) = NewStore $ simplify e
simplify e = e

-- | Simplify a binary operation
simplifyBinop :: BinOp -> Expr -> Expr -> Expr
-- shortcut implication
simplifyBinop Implication (LitB True) e2 = e2
simplifyBinop Implication (LitB False) e2 = LitB True
simplifyBinop Implication e1 (LitB True) = LitB True
-- shortcut and
simplifyBinop And (LitB True) e2 = e2
simplifyBinop And e1 (LitB True) = e1
simplifyBinop And (LitB False) e2 = LitB False
simplifyBinop And e1 (LitB False) = LitB False
-- shortcut or
simplifyBinop Or (LitB False) e2 = e2
simplifyBinop Or e1 (LitB False) = e1
simplifyBinop Or (LitB True) e2 = LitB True
simplifyBinop Or e1 (LitB True) = LitB True
-- apply comparisons
simplifyBinop LessThan (LitI a) (LitI b) = LitB $ a < b
simplifyBinop LessThanEqual (LitI a) (LitI b) = LitB $ a <= b
simplifyBinop GreaterThan (LitI a) (LitI b) = LitB $ a > b
simplifyBinop GreaterThanEqual (LitI a) (LitI b) = LitB $ a >= b
simplifyBinop Equal (LitI a) (LitI b) = LitB $ a == b
-- apply math ops
simplifyBinop Minus (LitI a) (LitI b) = LitI $ a - b
simplifyBinop Plus (LitI a) (LitI b) = LitI $ a + b
simplifyBinop Multiply (LitI a) (LitI b) = LitI $ a * b
-- don't do the rest
simplifyBinop o e1 e2 = BinopExpr o e1 e2

-- data Stmt
--   = Skip
--   | Assert Expr
--   | Assume Expr
--   | Assign String Expr
--   | AAssign String Expr Expr
--   | DrefAssign String Expr
--   | Seq Stmt Stmt
--   | IfThenElse Expr Stmt Stmt
--   | While Expr Stmt
--   | Block [VarDeclaration] Stmt
--   | TryCatch String Stmt Stmt
--

-- data Expr
--   = Var String
--   | LitI Int
--   | LitB Bool
--   | LitNull
--   | Parens Expr
--   | ArrayElem Expr Expr
--   | OpNeg Expr
--   | BinopExpr BinOp Expr Expr
--   | Forall String Expr
--   | Exists String Expr
--   | SizeOf Expr
--   | RepBy Expr Expr Expr
--   | Cond Expr Expr Expr
--   | NewStore Expr
--   | Dereference String
--   deriving (Eq)

-- data BinOp
--   = And
--   | Or
--   | Implication
--   | LessThan
--   | LessThanEqual
--   | GreaterThan
--   | GreaterThanEqual
--   | Equal
--   | Minus
--   | Plus
--   | Multiply
--   | Divide
--   | Alias
--   deriving (Eq)