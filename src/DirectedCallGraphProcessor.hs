module DirectedCallGraphProcessor where

import GCLParser.GCLDatatype (BinOp (..), Expr (..), PrimitiveType (..), Program (..), Stmt (..), Type (..), VarDeclaration (..))

import ProgramProcessor
import Types (PostCondition)
import DCG
import Helper (replaceAssignStmt)
import Z3Solver (Env, exprToZ3, buildEnv, getVarDeclarations)
import Z3.Monad


programDCG :: [Stmt] -> DCG Stmt
programDCG l = programDCG' $ reverse l

programDCG' :: [Stmt] -> DCG Stmt
programDCG' [] = Empty
programDCG' [x] = Leaf x  -- Base case: single element becomes the root
programDCG' (x:xs) =
  case x of
    IfThenElse cond thenStmt elseStmt ->
      -- Two children: one for 'then' and one for 'else' branches
      Node (programDCG' (reverse [thenStmt] ++ xs)) x (programDCG' (reverse [elseStmt] ++ xs))
    TryCatch _ tryStmt catchStmt ->
      -- Two children: one for 'try' and one for 'catch' blocks
      Node (programDCG' (reverse [tryStmt] ++ xs)) x (programDCG' (reverse [catchStmt] ++ xs))
    _ ->
      -- Chain the current node to the rest of the list
      SeqNode x (programDCG' xs)


wlpDCG :: DCG Stmt -> DCG PostCondition
wlpDCG dcg = wlpDCG' (dcg, LitB True)
  where
    wlpDCG' :: (DCG Stmt, PostCondition) -> DCG PostCondition
    wlpDCG' (Empty, _) = Empty
    wlpDCG' (Leaf x, pc) = Leaf (wlp x pc)
    wlpDCG' (SeqNode x c, pc) = 
      SeqNode pc' (wlpDCG' (c, pc'))
      where
        pc' = wlp x pc
    wlpDCG' (Node l (IfThenElse e _ _) r, pc) = 
      Node 
        (wlpDCG' (l, pcLeft)) 
        pc 
        (wlpDCG' (r, pcRight))
        where
          pcLeft  = BinopExpr And e pc
          pcRight = BinopExpr And (OpNeg e) pc
    wlpDCG' (Node l (TryCatch e s1 s2) r, pc) = 
      Node 
        (wlpDCG' (l, pc)) 
        pc 
        (wlpDCG' (r, pcError))
      where
        pcError = wlp (replaceAssignStmt s2 e (Var e)) pc
    
    wlpDCG' (Node l x r, pc) = Node (wlpDCG' (l, pc')) pc' (wlpDCG' (r, pc'))
      where
        pc' = wlp x pc

solveZ3DCG :: (MonadZ3 z3) => DCG PostCondition -> Env -> z3 [Bool]
solveZ3DCG Empty env = return [True]
solveZ3DCG (Node l x r) env = do
  l' <- solveZ3DCG l env 
  r' <- solveZ3DCG r env
  return $ l' ++ r'
solveZ3DCG (SeqNode x c) env = do
  c' <- solveZ3DCG c env
  return c'

solveZ3DCG (Leaf x) env = do
  z3Expr <- exprToZ3 x env
  
  z3ExprNot <- mkNot (z3Expr)

  assert z3ExprNot
  result <- check

  case result of
    Sat -> return [False]
    Unsat -> return [True]
    _ -> return [False]
