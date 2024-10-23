module DCG where

import Types

data DCG a
  = Node (DCG a) a (DCG a)
  | Empty


printDCG :: (Show a) => DCG a -> String
printDCG dcg = printDCG' dcg 0

printDCG' :: (Show a) => DCG a -> Int -> String
printDCG' (Node l x r) d =
    "Node: " ++ show x ++ "\n"
      ++ concat (replicate (d+1) "  ") ++ "Child 1: " ++ printDCG' l (d+1) ++ "\n"
      ++ concat (replicate (d+1) "  ") ++ "Child 2: " ++ printDCG' r (d+1) ++ "\n"
printDCG' Empty d = "Empty"