module DirectedCallGraphProcessor where

import GCLParser.GCLDatatype (BinOp (..), Expr (..), PrimitiveType (..), Program (..), Stmt (..), Type (..), VarDeclaration (..))

import ProgramProcessor
import Types (PostCondition)
import DCG
import Helper (replaceAssignStmt)
import Z3Solver (Env, exprToZ3, buildEnv, getVarDeclarations)
import Z3.Monad (check, Result (Unsat, Sat), evalZ3, assert, paramsToString)


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
      Node (programDCG' xs) x Empty

flattenProgram :: Stmt -> [Stmt]
flattenProgram Skip = []
flattenProgram stmt@(Assert _) = [stmt]
flattenProgram stmt@(Assume _) = [stmt]
flattenProgram stmt@(Assign _ _) = [stmt]
flattenProgram stmt@(AAssign _ _ _) = [stmt]
flattenProgram stmt@(DrefAssign _ _) = [stmt]
flattenProgram (Seq s1 s2) = flattenProgram s1 ++ flattenProgram s2
flattenProgram stmt@(IfThenElse _ _ _) = [stmt] -- Leave IfThenElse unchanged
flattenProgram stmt@(While _ _) = [stmt] -- Leave While unchanged
flattenProgram stmt@(Block _ _) = [stmt] -- Leave Block unchanged
flattenProgram stmt@(TryCatch _ _ _) = [stmt] -- Leave TryCatch unchanged

wlpDCG :: DCG Stmt -> DCG PostCondition
wlpDCG dcg = wlpDCG' (dcg, LitB True)
  where
    wlpDCG' :: (DCG Stmt, PostCondition) -> DCG PostCondition
    wlpDCG' (Empty, _) = Empty
    wlpDCG' (Leaf x, pc) = Leaf (wlp x pc)
    wlpDCG' (Node l (IfThenElse e _ _) r, pc) = Node (wlpDCG' (l, pcLeft)) pc (wlpDCG' (r, pcRight))
        where
          pcLeft  = BinopExpr And e pc
          pcRight = BinopExpr And (OpNeg e) pc
    wlpDCG' (Node l (TryCatch e s1 s2) r, pc) = Node (wlpDCG' (l, pc)) pc (wlpDCG' (r, pcError))
      where
        pcError = wlp (replaceAssignStmt s2 e (Var e)) pc
    wlpDCG' (Node l x r, pc) = Node (wlpDCG' (l, pc')) pc' (wlpDCG' (r, pc'))
      where
        pc' = wlp x pc

solveZ3DCG :: DCG PostCondition -> Program -> Env -> IO Bool
solveZ3DCG Empty _ env = return True
solveZ3DCG (Node l x r) p env = do
  l <- solveZ3DCG l p env 
  r <- solveZ3DCG r p env

  return $ l && r
  
solveZ3DCG (Leaf x) p env =
  evalZ3 $ do
    z3Expr <- exprToZ3 (negateExpr x) env
    env1 <- buildEnv (input p ++ output p ++ getVarDeclarations (stmt p)) (wlp (stmt p) (LitB True)) env

    assert z3Expr
    result <- check

    case result of
      Sat -> return False
      Unsat -> return True
      _ -> return False
