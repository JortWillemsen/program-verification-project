module DCG where

import Types

data DCG a
  = Node (DCG a) a (DCG a)
  | SeqNode a (DCG a)
  | Leaf a
  | Empty

reverseDCG :: DCG a -> DCG a
reverseDCG = go []  -- Use an empty list as an accumulator
  where
    -- Accumulate nodes in a list, then convert back to a DCG structure at the end
    go acc (SeqNode x next) = go (x : acc) next
    go acc (Leaf x)         = toDCG (x : acc)

    -- Convert a list of elements back to the DCG structure
    toDCG []     = error "Empty list - should not happen"
    toDCG [x]    = Leaf x
    toDCG (x:xs) = SeqNode x (toDCG xs)

printDCG :: (Show a) => DCG a -> String
printDCG dcg = printDCG' dcg 0

printDCG' :: (Show a) => DCG a -> Int -> String
printDCG' (SeqNode x c) d = 
    "Seq: " ++ show x ++ "\n"
      ++ concat (replicate (d+1) "  ") ++ "Child: " ++ printDCG' c (d+1) ++ "\n"
printDCG' (Node l x r) d =
    "Node: " ++ show x ++ "\n"
      ++ concat (replicate (d+1) "  ") ++ "Child 1: " ++ printDCG' l (d+1) ++ "\n"
      ++ concat (replicate (d+1) "  ") ++ "Child 2: " ++ printDCG' r (d+1) ++ "\n"
printDCG' (Leaf x) d = "Leaf: " ++ show x
printDCG' Empty d = "Empty"