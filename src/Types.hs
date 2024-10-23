module Types where

import GCLParser.GCLDatatype (Expr)

-- Define a type alias for postconditions as expressions
type PostCondition = Expr

-- Define a data structure for a binary tree with an empty case
data DCG a
  = Node (DCG a) a (DCG a) -- A node with left and right children
  | Leaf a -- A leaf node with a value
  | Empty -- An empty node
  deriving (Show)