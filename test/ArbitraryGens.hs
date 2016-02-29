{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ArbitraryGens (
  arith,
  bool
  ) where

import           CodeBuilders
import           Control.Applicative ((<$>), (<*>))
import           LLVMRepresentation
import           Test.QuickCheck
import           Types

arith :: forall a. Arith a => Gen (NodeBuilder a)
arith = sized tree'
  where tree' 0 = return . Const <$> arbitrary
        tree' n = oneof [return . Const <$> arbitrary,
                         mkArithM <$> split 2 <*> arbitrary <*> split 2,
                         mkIfM <$> subBool 3 <*> split 3 <*> split 3
                        ]
          where split m = tree' (n `div` m)
                subBool m = resize (n `div` m) (bool (getType :: a))

bool :: forall a. Arith a => a -> Gen (NodeBuilder Bool)
bool _ = sized tree'
  where tree' 0 = return . Const <$> arbitrary
        tree' n = oneof [return . Const <$> arbitrary,
                         mkCondM <$> subCond <*> arbitrary <*> subCond,
                         mkIfM <$> subBool <*> subBool <*> subBool
                        ]
          where subCond = resize (n `div` 2) (arith :: Gen (NodeBuilder a))
                subBool = tree' (n `div` 3)
