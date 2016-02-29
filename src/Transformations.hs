{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transformations (
  compile
) where

import           CodeBuilders
import           Control.Applicative ((<$>))
import           Control.Monad       (foldM, liftM2)
import           LLVMRepresentation
import           Types

type CodeTransformer = forall a. Node a -> NodeBuilder a

mapCode :: CodeTransformer -> CodeTransformer
mapCode fn node = do
  recursed <- case node of
    Block codes final ->
      liftM2 Block (mapM (\(AnyNode n) -> AnyNode <$> mapCode fn n) codes)
                   (mapCode fn final)
    _ -> return node
  fn recursed

-- mapCodeUntyped :: (forall a b. Code a -> Code b) -> Code c -> Code d
-- mapCodeUntyped fn code = fn $ code { node = recursed }
--   where re = mapCode fn
--         recursed = case node code of
--           MkBlock codes final -> MkBlock (map (mapAny $ mapCode fn) codes) (re final)
--           MkArithOp left op right -> MkArithOp (re left) op (re right)
--           MkIf cond thn els -> MkIf (mapCode fn cond) (re thn) (re els)
--           _ -> node code

eliminateBlocks :: AnyNode -> [AnyNode]
eliminateBlocks (AnyNode (Block codes code )) =
  concatMap eliminateBlocks $ codes ++ [AnyNode code]
eliminateBlocks otw = [otw]

transformSequence :: [Node a -> NodeBuilder a]
transformSequence =
  let cons = (:) :: CodeTransformer -> [CodeTransformer] -> [CodeTransformer] in
  map mapCode [] -- $ cons eliminateRets
              -- $ cons eliminateIfs
              -- $ cons eliminateBinOps []

compile :: Type a => NodeBuilder (Node a) -> CodeBuilder [AnyNode]
compile code = (eliminateBlocks . AnyNode) <$>
               (flip (foldM (flip ($))) transformSequence =<< code)
