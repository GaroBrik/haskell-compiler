{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ArbitraryGens (
  arith,
  cond
  ) where

import           CodeBuilders
import           Control.Applicative ((<$>), (<*>))
import           LLVMRepresentation
import           Test.QuickCheck
import           Types


arith :: Arith a => Gen (NodeBuilder a)
arith = sized tree'
  where tree' 0 = return . Const <$> arbitrary
        tree' n = oneof [return . Const <$> arbitrary,
                         mkArithM <$> split 2 <*> arbitrary <*> split 2
                        ]
          where split m = tree' (n `div` m)

cond :: Gen (NodeBuilder Bool)
cond = sized tree'
  where tree' 0 = return . Const <$> arbitrary
        tree' n = oneof [return . Const <$> arbitrary,
                         mkCondM <$> split 2 <*> arbitrary <*> split 2
                        ]
          where split m = resize (n `div` m)
                                 (arith :: Gen (NodeBuilder Int))
