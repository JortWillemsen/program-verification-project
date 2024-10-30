module DirectedCallGraphProcessor where

import GCLParser.GCLDatatype (BinOp (..), Expr (..), PrimitiveType (..), Program (..), Stmt (..), Type (..), VarDeclaration (..))
import Control.Monad.IO.Class (liftIO)
import ProgramProcessor
import Types (PostCondition)
import DCG
import Helper (replaceAssignStmt)
import Z3Solver (Env, exprToZ3, buildEnv, getVarDeclarations)
import Z3.Monad

programDCG :: Stmt -> DCG Stmt
programDCG Skip = Empty
programDCG stmt@(Assert _) = Leaf stmt
programDCG stmt@(Assume _) = Leaf stmt
programDCG stmt@(Assign _ _) = Leaf stmt
programDCG stmt@(AAssign _ _ _) = Leaf stmt
programDCG (Seq (Skip) s2) = programDCG s2
programDCG (Seq s1 (Skip)) = programDCG s1
programDCG (Seq s1 s2) = combineDCG (programDCG s1) (programDCG s2)
programDCG stmt@(IfThenElse e s1 s2) = Node (SeqNode (Assume e) (programDCG s1)) stmt (SeqNode (Assume (OpNeg e)) (programDCG s2))
programDCG stmt@(While e s) = Node (SeqNode (Assume (OpNeg e)) Empty) stmt (SeqNode (Assume e) (SeqNode s (programDCG stmt)))
programDCG stmt@(Block _ s) = programDCG s

combineDCG :: DCG Stmt -> DCG Stmt -> DCG Stmt
combineDCG (Leaf x) t2 = (SeqNode x t2)
combineDCG (Empty) t2 = t2
combineDCG (Node l x r) t2 = Node (combineDCG l t2) x (combineDCG r t2)
combineDCG (SeqNode x c) t2 = SeqNode x (combineDCG c t2)

dcgToPaths :: DCG a -> Int -> [DCG a]
dcgToPaths dcg k = dcgToPathsToK dcg 0 k

dcgToPathsToK :: DCG a -> Int -> Int -> [DCG a]
dcgToPathsToK Empty i k = []
dcgToPathsToK (Leaf a) i k = [Leaf a]
dcgToPathsToK (SeqNode a dcg) i k
  | i <= k =  [SeqNode a path | path <- dcgToPathsToK dcg (i+1) k]
  | otherwise = [Leaf a]
dcgToPathsToK (Node left a right) i k
  | i <= k =
    [path | path <- dcgToPathsToK left (i+1) k] ++ 
    [path | path <- dcgToPathsToK right (i+1) k]
  | otherwise = [Leaf a]

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

solveZ3DCGs :: (MonadZ3 z3) => [DCG PostCondition] -> Env -> z3 [Bool]
solveZ3DCGs l env = mapM (\dcg -> solveZ3DCG dcg env) l

solveZ3DCG :: (MonadZ3 z3) => DCG PostCondition -> Env -> z3 Bool
solveZ3DCG Empty env = return True
solveZ3DCG (Node l x r) env = do
  l' <- solveZ3DCG l env 
  r' <- solveZ3DCG r env
  return $ l' && r'
solveZ3DCG (SeqNode x c) env = do
  c' <- solveZ3DCG c env
  return c'

solveZ3DCG (Leaf x) env = do
  z3Expr <- exprToZ3 x env
  
  z3ExprNot <- mkNot (z3Expr)


  assert z3ExprNot
  result <- check

  case result of
    Sat -> return False
    Unsat -> return True
    _ -> return False
