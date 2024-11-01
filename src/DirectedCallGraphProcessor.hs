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

programDCG :: (MonadZ3 z3) => Env -> Stmt -> z3 (DCG Stmt)
programDCG env Skip = return Empty
programDCG env stmt@(Assert _) = return $ Leaf stmt
programDCG env stmt@(Assume _) = return $ Leaf stmt
programDCG env stmt@(Assign _ _) = return $ Leaf stmt
programDCG env stmt@(AAssign {}) = return $ Leaf stmt
programDCG env (Seq Skip s2) = programDCG env s2
programDCG env (Seq s1 Skip) = programDCG env s1
programDCG env (Seq s1 s2) = do
  x1 <- programDCG env s1
  x2 <- programDCG env s2

  return $ combineDCG x1 x2
programDCG env stmt@(IfThenElse e s1 s2) = do
  x1 <- programDCG env s1
  p1 <- prunePath env x1
  x2 <- programDCG env s2
  p2 <- prunePath env x2

  return $ Node (SeqNode (Assume e) p1) stmt (SeqNode (Assume (OpNeg e)) p2)
programDCG env stmt@(While e s) = programDCG env (programWhile stmt 10)
programDCG env stmt@(Block _ s) = programDCG env s

prunePath :: (MonadZ3 z3) => Env -> DCG Stmt -> z3 (DCG Stmt)
prunePath env dcg = do
  -- Calculate WLP of path
  wlp <- wlpDCG dcg

  -- Check if path is feasible
  feasible <- feasibleZ3 wlp env

  -- Return True if path is not feasible
  -- Return False if feasible (As to not prune it)
  if feasible 
    then return dcg 
    else return Empty

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

wlpDCG :: (MonadZ3 z3) => DCG Stmt -> z3 (DCG PostCondition)
wlpDCG dcg = return $ wlpDCG' (dcg, LitB True)
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
solveZ3DCGs l env = mapM (\x ->do f <- x `feasibleZ3` env; return $ not f) l

feasibleZ3 :: (MonadZ3 z3) => DCG PostCondition -> Env -> z3 Bool
feasibleZ3 Empty env = return True
feasibleZ3 (Node l x r) env = do
  l' <- feasibleZ3 l env
  r' <- feasibleZ3 r env
  return $ l' && r'
feasibleZ3 (SeqNode x c) env = do
  feasibleZ3 c env
feasibleZ3 (Leaf x) env = do
  z3Expr <- exprToZ3 x env

  z3ExprNot <- mkNot z3Expr

  assert z3ExprNot
  result <- check

  case result of
    Sat -> return True
    Unsat -> return False
    _ -> return True
