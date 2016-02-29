module EvalNode (
  eval
  ) where

import Types
import LLVMRepresentation
import CodeBuilders

eval :: Primitive a => NodeBuilder (Node a) -> a
eval = read . run . compile
