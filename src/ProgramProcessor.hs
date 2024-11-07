{-# LANGUAGE AllowAmbiguousTypes #-}

module ProgramProcessor where

import GCLParser.GCLDatatype (BinOp (..), Expr (..), PrimitiveType (..), Program (..), Type (..), VarDeclaration (..))
import Helper (replaceAssign, replaceAssignStmt)
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
wlp (Path stmts _) = wlp' stmts
  where
    wlp' [] = LitB True
    wlp' ((Assert e) : xs) = BinopExpr And e (wlp' xs)
    wlp' ((Assume e) : xs) = BinopExpr Implication e (wlp' xs)
    wlp' ((Assign str e) : xs) = replaceAssign str e (wlp' xs)
    wlp' ((AAssign str e1 e2) : xs) = replaceAssign str (RepBy (Var str) e1 e2) (wlp' xs)
    wlp' ((Decl x) : xs) = wlp' xs

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