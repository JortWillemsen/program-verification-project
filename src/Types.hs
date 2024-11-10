{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Types where

import qualified Data.Map as M
import Z3.Base (AST)
import qualified GCLParser.GCLDatatype as GCL

-- Directed Call Graph (Tree structure)
data DCG a
  = Node (DCG a) a (DCG a)
  | Leaf a
  | SeqNode a (DCG a)
  | Empty

type Env = M.Map String AST

data Statement
  = Assert GCL.Expr
  | Assume GCL.Expr
  | Assign String GCL.Expr
  | AAssign String GCL.Expr GCL.Expr

data Path = Path [Statement] Env


data Options = Options 
  { verbose :: Bool
  , k :: Int
  , n :: Int
  , pruneLen :: Int
  }

instance Show Statement where
  show :: Statement -> String
  show (Assert e) = "assert " ++ show e
  show (Assume e) = "assume " ++ show e
  show (Assign str e) = str ++ " := " ++ show e
  show (AAssign str e1 e2) = str ++ "[" ++ show e1 ++ "] := " ++ show e2

instance Show Path where
  show :: Path -> String
  show (Path stmts _) = 
    "Path: \n" ++ 
    "   "      ++ 
    show stmts ++ 
    "\n   "    ++ 
    "\n\n"

instance (Show a) => Show (DCG a) where
  show :: Show a => DCG a -> String
  show = printDCG

printDCG :: (Show a) => DCG a -> String
printDCG dcg = printDCG' dcg 0

printDCG' :: (Show a) => DCG a -> Int -> String
printDCG' (SeqNode x c) d =
  "Seq: "
    ++ show x
    ++ "\n"
    ++ concat (replicate (d + 1) "  ")
    ++ "Child: "
    ++ printDCG' c (d + 1)
    ++ "\n"
printDCG' (Node l x r) d =
  "Node: "
    ++ show x
    ++ "\n"
    ++ concat (replicate (d + 1) "  ")
    ++ "Child 1: "
    ++ printDCG' l (d + 1)
    ++ "\n"
    ++ concat (replicate (d + 1) "  ")
    ++ "Child 2: "
    ++ printDCG' r (d + 1)
    ++ "\n"
printDCG' (Leaf x) d = "Leaf: " ++ show x
printDCG' Empty d = "Empty"

-- | Transforms a GCL statement into our own type
gclToStatement :: GCL.Stmt -> Statement
gclToStatement (GCL.Assert e) = Assert e
gclToStatement (GCL.Assume e) = Assume e
gclToStatement (GCL.Assign s e) = Assign s e
gclToStatement (GCL.AAssign s e1 e2) = AAssign s e1 e2