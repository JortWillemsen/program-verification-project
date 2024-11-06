module Types where

import qualified GCLParser.GCLDatatype as GCL
import Z3.Base (AST)
import Data.Map as M
import GCLParser.GCLDatatype (VarDeclaration)
type PostCondition = GCL.Expr

type Env = M.Map String AST


data Statement 
  = Assert GCL.Expr
  | Assume GCL.Expr
  | Assign String GCL.Expr
  | AAssign String GCL.Expr GCL.Expr
  | Decl [GCL.VarDeclaration]

instance Show Statement where
  show (Assert e) = "assert " ++ show e
  show (Assume e) = "assume " ++ show e
  show (Assign str e) = str ++ " := " ++ show e
  show (AAssign str e1 e2) = str ++ "[" ++ show e1 ++ "] := " ++ show e2
  show (Decl vars) = "DECLS"

data Path = Path [Statement] Env

instance Show Path where
  show (Path stmts env) = "Path: \n" ++ "   " ++ show stmts ++ "\n   " ++ show env ++ "\n   " ++"\n\n"


gclToStatement :: GCL.Stmt -> Statement
gclToStatement (GCL.Assert e) = Assert e
gclToStatement (GCL.Assume e) = Assume e
gclToStatement (GCL.Assign s e) = Assign s e
gclToStatement (GCL.AAssign s e1 e2) = AAssign s e1 e2
