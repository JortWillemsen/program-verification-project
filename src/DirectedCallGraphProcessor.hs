module DirectedCallGraphProcessor where

import Control.Monad.IO.Class (liftIO)
import DCG (DCG (..))
import GCLParser.GCLDatatype (BinOp (..), Expr (..), PrimitiveType (..), Program (..), Stmt (..), Type (..), VarDeclaration (..))
import Helper (replaceAssignStmt)
import ProgramProcessor (wlp)
import Types (PostCondition)
import Z3.Monad
  ( MonadZ3,
    Result (Sat, Unsat),
    assert,
    check,
    mkNot,
  )
import Z3Solver (Env, buildEnv, exprToZ3, getVarDeclarations)

programDCG :: Stmt -> DCG Stmt
programDCG Skip = Empty
programDCG stmt@(Assert _) = Leaf stmt
programDCG stmt@(Assume _) = Leaf stmt
programDCG stmt@(Assign _ _) = Leaf stmt
programDCG stmt@(AAssign {}) = Leaf stmt
programDCG (Seq Skip s2) = programDCG s2
programDCG (Seq s1 Skip) = programDCG s1
programDCG (Seq s1 s2) = combineDCG (programDCG s1) (programDCG s2)
programDCG stmt@(IfThenElse e s1 s2) = Node (SeqNode (Assume e) (programDCG s1)) stmt (SeqNode (Assume (OpNeg e)) (programDCG s2))
programDCG stmt@(While e s) = programDCG $ programWhile stmt 10
programDCG stmt@(Block _ s) = programDCG s

programWhile :: Stmt -> Int -> Stmt
programWhile stmt@(While e s) k
  | k == 0 = Assume (OpNeg e)
  | k > 0 = IfThenElse e (Seq s (programWhile stmt (k - 1))) Skip

combineDCG :: DCG Stmt -> DCG Stmt -> DCG Stmt
combineDCG (Leaf x) t2 = SeqNode x t2
combineDCG Empty t2 = t2
combineDCG (Node l x r) t2 = Node (combineDCG l t2) x (combineDCG r t2)
combineDCG (SeqNode x c) t2 = SeqNode x (combineDCG c t2)

dcgToPaths :: DCG a -> [DCG a]
dcgToPaths Empty = []
dcgToPaths (Leaf a) = [Leaf a]
dcgToPaths (SeqNode a dcg) = [SeqNode a path | path <- dcgToPaths dcg]
dcgToPaths (Node left a right) =
  dcgToPaths left
    ++ dcgToPaths right

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
        pcLeft = BinopExpr And e pc
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
solveZ3DCGs l env = mapM (`solveZ3DCG` env) l

solveZ3DCG :: (MonadZ3 z3) => DCG PostCondition -> Env -> z3 Bool
solveZ3DCG Empty env = return True
solveZ3DCG (Node l x r) env = do
  l' <- solveZ3DCG l env
  r' <- solveZ3DCG r env
  return $ l' && r'
solveZ3DCG (SeqNode x c) env = do
  solveZ3DCG c env
solveZ3DCG (Leaf x) env = do
  z3Expr <- exprToZ3 x env

  z3ExprNot <- mkNot z3Expr

  assert z3ExprNot
  result <- check

  case result of
    Sat -> return False
    Unsat -> return True
    _ -> return False
