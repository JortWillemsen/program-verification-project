{-# LANGUAGE AllowAmbiguousTypes #-}

module FormulaProcessor where

import GCLParser.GCLDatatype (Expr (..), Program (..), Stmt (..), opAnd, opImplication)

type PostCondition = Expr

processAST :: Program -> PostCondition
processAST p = wlp (stmt p) (LitB True)

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

wlp :: Stmt -> PostCondition -> PostCondition
wlp (Seq s1 s2) pc = wlp s1 $ wlp s2 pc
wlp Skip pc = pc
wlp (Assume e) pc = opImplication e pc -- e -> pc
wlp (Assert e) pc = opAnd e pc -- e /\ pc
wlp (Assign str e) pc = undefined
wlp (AAssign str e1 e2) pc = undefined
wlp (DrefAssign str e) pc = undefined -- optional
wlp (IfThenElse e s1 s2) pc = undefined
wlp (While e s) pc = undefined
wlp (Block vars s) pc = undefined
wlp (TryCatch str s1 s2) pc = undefined

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