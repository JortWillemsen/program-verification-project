{-# LANGUAGE AllowAmbiguousTypes #-}

module FormulaProcessor where

import Data.Map (Map)
import Data.Map.Lazy (insert)
import GCLParser.GCLDatatype (BinOp (And, Equal), Expr (..), Program (..), Stmt (..), opAnd, opEqual, opImplication)
import Text.Printf (vFmt)

type PostCondition = Expr

processAST :: Program -> PostCondition
processAST p = wlp (stmt p) (LitB True)

type Value = Expr

type Ident = String

type Environment = Map Ident Value

wlp :: Stmt -> PostCondition -> PostCondition
wlp (Assign str e) pc = replaceAssign pc str e

-- | postcondition: Current post condition
-- | string: variable name
-- | expr: thing that goes in the variable
-- |
replaceAssign :: PostCondition -> String -> Expr -> PostCondition
replaceAssign (Var curStr) str e
  | curStr == str = e
replaceAssign (Parens e1) str e = (Parens (replaceAssign e1 str e))
replaceAssign (ArrayElem e1 e2) str e = (ArrayElem (replaceAssign e1 str e) (replaceAssign e2 str e))
replaceAssign (OpNeg e1) str e = (OpNeg (replaceAssign e1 str e))
replaceAssign (BinopExpr op e1 e2) str e = BinopExpr op (replaceAssign e1 str e) (replaceAssign e2 str e)
replaceAssign (Forall str1 e1) str e = (Forall str1 (replaceAssign e1 str e))
replaceAssign (Exists str1 e1) str e = (Exists str1 (replaceAssign e1 str e))
replaceAssign (SizeOf e1) str e = (SizeOf (replaceAssign e1 str e))
replaceAssign (RepBy e1 e2 e3) str e = (RepBy (replaceAssign e1 str e) (replaceAssign e2 str e) (replaceAssign e3 str e))
replaceAssign (Cond e1 e2 e3) str e = (Cond (replaceAssign e1 str e) (replaceAssign e2 str e) (replaceAssign e3 str e))
replaceAssign (NewStore e1) str e = (NewStore (replaceAssign e1 str e))
replaceAssign other str e = other
--
--
--
--
--
--
--
--
transform :: Expr -> String -> Expr -- first function transforms a var to another var

transform s v@(Var curStr)
    | s == curStr
    | otherwise = v

--  Var                String
--     | LitI               Int
--     | LitB               Bool
--     | LitNull
--     | Parens             Expr
--     | ArrayElem          Expr   Expr
--     | OpNeg              Expr
--     | BinopExpr          BinOp  Expr   Expr
--     | Forall             String Expr
--     | Exists             String Expr
--     | SizeOf             Expr
--     | RepBy              Expr   Expr   Expr
--     | Cond               Expr   Expr   Expr
--     | NewStore           Expr
--     | Dereference        String

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

-- replaceAssign (Var idtje) str e =
-- wlp env (Seq s1 s2) pc = wlp (snd calc2) s1 $ fst calc2
--   where
--     calc2 = wlp env s2 pc
-- wlp env (Assign str e) pc = (opEqual (Var str) e, insert str e env) -- x = e
-- wlp env (AAssign str e1 e2) pc = undefined -- x[e1] = e2
-- wlp env (DrefAssign str e) pc = undefined -- optional
-- wlp env (IfThenElse e s1 s2) pc = undefined -- if (e) then s1 else s2
-- wlp env (While e s) pc = undefined -- while (e) s
-- wlp env (Block vars s) pc = undefined -- {vars s}
-- wlp env (TryCatch str s1 s2) pc = undefined -- ?
-- wlp _ Skip pc = pc
-- wlp _ (Assume e) pc = opImplication e pc -- e -> pc
-- wlp _ (Assert e) pc = opAnd e pc -- e /\ pc

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