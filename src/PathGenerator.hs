{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module PathGenerator where

import qualified GCLParser.GCLDatatype as GCL
import Types (DCG (Empty, Leaf, SeqNode, Node), Options (k, pruneLen), Statement (Assume), Path (Path), Env, gclToStatement)
import Z3.Monad (Z3)
import Z3Solver (isSatisfiableExpr)
import WlpGenerator (conjunctive)

-- | Convert the Program to a DCG
programToDCG :: GCL.Stmt -> Options -> DCG GCL.Stmt
programToDCG GCL.Skip _ = Empty
programToDCG s@(GCL.Assert {}) _          = Leaf s
programToDCG s@(GCL.Assume {}) _          = Leaf s
programToDCG s@(GCL.Assign {}) _          = Leaf s
programToDCG s@(GCL.AAssign {}) _         = Leaf s
programToDCG s@(GCL.Seq GCL.Skip s2) o    = programToDCG s2 o
programToDCG s@(GCL.Seq s1 GCL.Skip) o    = programToDCG s1 o
programToDCG   (GCL.Seq s1 s2) o          = combineDCG (programToDCG s1 o) (programToDCG s2 o)
programToDCG s@(GCL.IfThenElse _ s1 s2) o = Node (programToDCG s1 o) s (programToDCG s2 o)
programToDCG s@(GCL.While {}) o           = programToDCG (unfold s (k o)) o
programToDCG   (GCL.Block _ c) o          = programToDCG c o

-- | Replace emptys and leafs in tree 1 with tree 2
combineDCG :: DCG GCL.Stmt -> DCG GCL.Stmt -> DCG GCL.Stmt
combineDCG Empty t2 = t2
combineDCG (Leaf x) t2 = SeqNode x t2
combineDCG (SeqNode x c) t2 = SeqNode x (combineDCG c t2)
combineDCG (Node l x r) t2 = Node (combineDCG l t2) x (combineDCG r t2)

-- | Unfolding a while to an if, k times
unfold :: GCL.Stmt -> Int -> GCL.Stmt
unfold s@(GCL.While e b) k
  | k <= 0  = GCL.Assume (GCL.OpNeg e)
  | k > 0   = GCL.IfThenElse e s1 s2
  where
    s1 = GCL.Seq b $ unfold s (k-1)
    s2 = GCL.Skip

-- | Converts the DCG to a list of paths
dcgToPaths :: DCG GCL.Stmt -> Env -> Options -> Z3 [Path]
dcgToPaths dcg decls o = dcgToPaths' dcg decls []
  where
    dcgToPaths' :: DCG GCL.Stmt -> Env -> [Statement] -> Z3 [Path]
    dcgToPaths' Empty _ _ = error "There should be no empty here"
    dcgToPaths' (Leaf x) env cur = return [Path (cur ++ [gclToStatement x]) env]
    dcgToPaths' (SeqNode x c) env cur = dcgToPaths' c env $ cur ++ [gclToStatement x]
    dcgToPaths' (Node l (GCL.IfThenElse g _ _) r) env cur = do
      -- make conjunctions
      
      let conjThen = conjunctive $ cur ++ [Assume g]
      let conjElse = conjunctive $ cur ++ [Assume (GCL.OpNeg g)]
      
      --when (verbose o) $ liftIO $ print conjThen
      --when (verbose o) $ liftIO $ print conjElse

      satThen <- isSatisfiableExpr env conjThen
      satElse <- isSatisfiableExpr env conjElse

      -- liftIO $ "length "

      if length cur > pruneLen o
        then do
          pathThen <- if satThen then dcgToPaths' l env (cur ++ [Assume g]) else return []
          pathElse <- if satElse then dcgToPaths' r env (cur ++ [Assume (GCL.OpNeg g)]) else return []
          return $ pathThen ++ pathElse
        else do 
          pathL <- dcgToPaths' l env (cur ++ [Assume g])
          pathR <- dcgToPaths' r env (cur ++ [Assume (GCL.OpNeg g)])
          return $ pathL ++ pathR

      -- when (length cur) < (pruneLen o) 
      --   $ return (dcgToPaths' l env (cur ++ [Assume g])) 
      --   ++ 
      --   (dcgToPaths' r env (cur ++ [Assume (GCL.OpNeg g)]))

      -- pathThen <- if satThen then dcgToPaths' l env (cur ++ [Assume g]) else return []
      -- pathElse <- if satElse then dcgToPaths' r env (cur ++ [Assume (GCL.OpNeg g)]) else return []

      -- return $ pathThen ++ pathElse