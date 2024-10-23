module DirectedCallGraphProcessor where

import GCLParser.GCLDatatype (Expr (LitB), Stmt (..))
import ProgramProcessor
import Types (PostCondition)
import DCG

-- Function to create a DCG from a Stmt, skipping Skip nodes and allowing all
-- other statements to have possible children
-- Function to create a DCG from a Stmt, skipping Skip nodes and allowing all
-- other statements to have possible children
programDCG :: Stmt -> DCG Stmt
programDCG Skip = Empty
programDCG (Assign var expr) = Node Empty (Assign var expr) Empty
programDCG (Assert expr) = Node Empty (Assert expr) Empty
programDCG (Assume expr) = Node Empty (Assume expr) Empty
programDCG (AAssign arr idx val) = Node Empty (AAssign arr idx val) Empty
programDCG (DrefAssign var expr) = Node Empty (DrefAssign var expr) Empty

programDCG (Seq s1 s2) =
  let rightDCG = programDCG s2
  in case rightDCG of
    Empty -> programDCG s1
    _     -> Node (programDCG s1) (Seq s1 s2) rightDCG

programDCG (IfThenElse cond s1 s2) =
  Node (programDCG s1) (IfThenElse cond s1 s2) (programDCG s2)

programDCG (While cond body) = 
  Node (programDCG body) (While cond body) Empty

programDCG (Block _ stmt) = programDCG stmt

programDCG (TryCatch _ tryBlock catchBlock) =
  Node (programDCG tryBlock) (TryCatch "" tryBlock catchBlock) (programDCG catchBlock)

-- Helper function to reverse the DCG tree by swapping left and right subtrees
reverseDCG :: DCG a -> DCG a
reverseDCG Empty = Empty
reverseDCG (Node left stmt right) =
  Node (reverseDCG right) stmt (reverseDCG left)  -- Swap the left and right subtrees

-- Function to first generate the DCG, then reverse it
reversedProgramDCG :: Stmt -> DCG Stmt
reversedProgramDCG stmt = reverseDCG (programDCG stmt)

wlpDCG :: DCG Stmt -> DCG PostCondition
wlpDCG dcg = wlpDCG' (dcg, LitB True)
  where
    wlpDCG' :: (DCG Stmt, PostCondition) -> DCG PostCondition
    wlpDCG' (Empty, _) = Empty
    wlpDCG' (Node l x r, pc) = Node (wlpDCG' (l, pc')) pc' (wlpDCG' (r, pc'))
      where
        pc' = wlp x pc