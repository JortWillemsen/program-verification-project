module DirectedCallGraphProcessor where

import Control.Monad.IO.Class (liftIO)
import DCG (DCG (..), printDCG)
import qualified GCLParser.GCLDatatype as GCL
import Helper (replaceAssign, replace)
import ProgramProcessor (wlp, simplify)
import Types (PostCondition, Path (Path), Statement (Assume, Assign, AAssign, Decl), Env, gclToStatement)
import Debug.Trace
import Z3.Monad
  ( MonadZ3,
    Result (Sat, Unsat),
    assert,
    check,
    mkNot, getModel, showModel,
  )
import Z3Solver (buildEnv, getVarDeclarations, exprToZ3)
import Control.Monad (when)
import Data.Map (valid)
import GCLParser.GCLDatatype (VarDeclaration)

programDCG :: GCL.Stmt -> DCG GCL.Stmt
programDCG GCL.Skip = Empty
programDCG stmt@(GCL.Assert _) = Leaf (stmt)
programDCG stmt@(GCL.Assume _) = Leaf stmt
programDCG stmt@(GCL.Assign _ _) = Leaf stmt
programDCG stmt@(GCL.AAssign {}) = Leaf stmt
programDCG (GCL.Seq GCL.Skip s2) = programDCG s2
programDCG (GCL.Seq s1 GCL.Skip) = programDCG s1
programDCG (GCL.Seq s1 s2) = combineDCG (programDCG s1) (programDCG s2)
programDCG stmt@(GCL.IfThenElse e s1 s2) = Node (SeqNode (GCL.Assume e) (programDCG s1)) stmt (SeqNode (GCL.Assume ((GCL.OpNeg e))) (programDCG s2))
programDCG stmt@(GCL.While e s) = programDCG $ programWhile stmt 20
programDCG stmt@(GCL.Block decls s) =  DeclNode decls $ programDCG s

-- Function that returns true if a path does not need to be pruned
prunePath :: (MonadZ3 z3) => Path -> z3 Bool
prunePath (Path stmts env) =
  do
    let conj = simplify $ makeConjunction (reverse stmts) (GCL.LitB True)

    feasible <- feasibleZ3 conj env

    --liftIO (putStrLn $ if (feasible) then "PRUNING" ++ show conj else "")
    return feasible


-- Function that creates a conjunction of the path
-- meaning all assumptions should be true
makeConjunction :: [Statement] -> GCL.Expr -> GCL.Expr
makeConjunction [] = id
makeConjunction (Assume e : xs) = makeConjunction xs . GCL.BinopExpr GCL.And e
makeConjunction ((Assign s e): xs) = makeConjunction xs . replace s e
makeConjunction ((AAssign s e1 e2) : xs) = makeConjunction xs . replace s (GCL.RepBy (GCL.Var s) e1 e2)
makeConjunction (x : xs) = makeConjunction xs

programWhile :: GCL.Stmt -> Int -> GCL.Stmt
programWhile stmt@(GCL.While e s) k
  | k <= 0 = GCL.Assume (GCL.OpNeg e)
  | k > 0 = GCL.IfThenElse e (GCL.Seq s (programWhile stmt (k - 1))) GCL.Skip

combineDCG :: DCG GCL.Stmt -> DCG GCL.Stmt -> DCG GCL.Stmt
combineDCG (Leaf x) t2 = SeqNode x t2
combineDCG Empty t2 = t2
combineDCG (Node l x r) t2 = Node (combineDCG l t2) x (combineDCG r t2)
combineDCG (SeqNode x c) t2 = SeqNode x (combineDCG c t2)
combineDCG (DeclNode x c) t2 = DeclNode x (combineDCG c t2)

dcgToStatements :: (MonadZ3 z3) => DCG GCL.Stmt -> [VarDeclaration] -> [Statement] -> z3 [Path]
dcgToStatements Empty decls currentPath = do
  env <- buildEnv currentPath decls
  return [Path currentPath env]

dcgToStatements (Leaf x) decls currentPath = do
  env <- buildEnv currentPath decls
  liftIO $ putStrLn $ "found complete path: " ++ show (currentPath ++ [gclToStatement x])
  return [Path (currentPath ++ [gclToStatement x]) env]

dcgToStatements (SeqNode x c) decls currentPath = do
  let newPath = currentPath ++ [gclToStatement x]
  dcgToStatements c decls newPath

dcgToStatements (Node l (GCL.IfThenElse e _ _) r) decls currentPath = do
      env <- buildEnv currentPath decls
      let conjTrue = makeConjunction (reverse $ currentPath ++ [Assume e]) (GCL.LitB True)
      let conjFalse = makeConjunction (reverse $ currentPath ++ [Assume (GCL.OpNeg e)]) (GCL.LitB True)

      leftFeasible <- feasibleZ3 conjTrue env
      rightFeasible <- feasibleZ3 conjFalse env

      if leftFeasible
        then do
          liftIO $ putStrLn (show conjTrue)
          liftIO $ putStrLn $ (show conjFalse) ++ " PRUNING";
          dcgToStatements l decls currentPath
        else if rightFeasible 
          then do
          liftIO $ putStrLn $ (show conjTrue) ++ " PRUNING"
          liftIO $ putStrLn (show conjFalse)
          dcgToStatements r decls currentPath
        else do
          liftIO $ putStrLn $ (show conjTrue) ++ " PRUNING"
          liftIO $ putStrLn $ (show conjFalse) ++ " PRUNING"
          return []
dcgToStatements (Node l x r) decls currentPath = do
      leftPaths <- dcgToStatements l decls currentPath
      rightPaths <- dcgToStatements r decls currentPath
      return $ leftPaths ++ rightPaths
dcgToStatements (DeclNode decls c) decls' currentPath = do
  let newPath = currentPath
  dcgToStatements c (decls' ++ decls) newPath

-- statementsToPath :: (MonadZ3 z3) => [[Statement]] -> [VarDeclaration] -> z3 [Path]
-- statementsToPath [] _ = return []
-- statementsToPath (p : ps) decls = do
--   env <- buildEnv p decls
--   rest <- statementsToPath ps decls
--   return $ Path p env : rest

-- dcgToPaths :: (MonadZ3 z3) => DCG Stmt -> Env -> z3 ([DCG Stmt], Int)
-- dcgToPaths Empty env = do
--   return ([], 0)
-- dcgToPaths (Leaf a) env = do
--   return ([Leaf a], 0)
-- dcgToPaths (SeqNode a dcg) env = do
--   (paths, prunedCount) <- dcgToPaths dcg env
--   return ([SeqNode a path | path <- paths], prunedCount)
-- dcgToPaths (Node left a right) env = do
--   (lPaths, lPrunedCount) <- dcgToPaths left env
--   (rPaths, rPrunedCount) <- dcgToPaths right env

--   lPruned <- mapM (prunePath env) lPaths
--   rPruned <- mapM (prunePath env) rPaths

--   let (lFiltered, lNumPruned) = filterPaths lPruned
--   let (rFiltered, rNumPruned) = filterPaths rPruned

--   return (lFiltered ++ rFiltered, lNumPruned + rNumPruned + lPrunedCount + rPrunedCount)


-- wlpDCG :: (MonadZ3 z3) => DCG Stmt -> z3 (DCG PostCondition)
-- wlpDCG dcg = return $ wlpDCG' (dcg, LitB True)
--   where
--     wlpDCG' :: (DCG Stmt, PostCondition) -> DCG PostCondition
--     wlpDCG' (Empty, _) = Empty
--     wlpDCG' (Leaf x, pc) = Leaf (wlp x pc)
--     wlpDCG' (SeqNode x c, pc) =
--       SeqNode pc' (wlpDCG' (c, pc'))
--       where
--         pc' = wlp x pc
--     wlpDCG' (Node l (IfThenElse e _ _) r, pc) =
--       Node
--         (wlpDCG' (l, pcLeft))
--         pc
--         (wlpDCG' (r, pcRight))
--       where
--         pcLeft = BinopExpr And e pc
--         pcRight = BinopExpr And (OpNeg e) pc
--     wlpDCG' (Node l (TryCatch e s1 s2) r, pc) =
--       Node
--         (wlpDCG' (l, pc))
--         pc
--         (wlpDCG' (r, pcError))
--       where
--         pcError = wlp (replaceAssignStmt s2 e (Var e)) pc
--     wlpDCG' (Node l x r, pc) = Node (wlpDCG' (l, pc')) pc' (wlpDCG' (r, pc'))
--       where
--         pc' = wlp x pc

-- solveZ3DCGs :: (MonadZ3 z3) => [DCG PostCondition] -> Env -> z3 [Bool]
-- solveZ3DCGs l env = trace (show (length l)) $ mapM (\x ->do x `validZ3` env;) l

validatePath :: (MonadZ3 z3) => Path -> z3 Bool
validatePath p@(Path stmts env) = do
  let expr = wlp p
  validZ3 expr env

feasiblePath :: (MonadZ3 z3) => Path -> z3 Bool
feasiblePath p@(Path stmts env) = do
  let expr  = makeConjunction (reverse stmts) (GCL.LitB True)

  feasibleZ3 expr env

validZ3 :: (MonadZ3 z3) => GCL.Expr -> Env -> z3 Bool
validZ3 e env = do
  z3Expr <- exprToZ3 e env

  z3ExprNot <- mkNot z3Expr

  assert z3ExprNot
  result <- check

  case result of
    Sat -> return False
    Unsat -> do liftIO $ putStrLn (show e); return True
    _ -> error "Result is weird (validity)"

feasibleZ3 :: (MonadZ3 z3) => GCL.Expr -> Env -> z3 Bool
feasibleZ3 e env = do
  z3Expr <- exprToZ3 e env

  assert z3Expr
  result <- check

  case result of
    Sat -> return True
    Unsat -> return False
    _ -> error "Result is weird (feasible)"
