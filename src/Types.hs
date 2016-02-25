{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Types (
  Type, getType, showType, hasResult,
  Primitive, showVal, readVal,
  NonVoid, Arith,
  Pointer(MkPointer)
) where

import Data.Char
import Test.QuickCheck

-- The "Type" type class encapsulates LLVM types.
-- The types are defined in this way instead of as a data type enumeration so
-- that haskell values can be identified with LLVM types easily, and so that
-- we can provide compile time guarantees of correctness of the compiler itself,
-- as this type class in addition to the definitions of "Code"s provides a sort
-- of weak dependent typing.

class (Show a, Eq a) => Type a where
  showType :: a -> String
  getType :: Type a => a
  hasResult :: a -> Bool

class Type a => NonVoid a where

class (Arbitrary a, NonVoid a) => Primitive a where
  showVal :: a -> String
  readVal :: String -> a

class (Integral a, Primitive a) => Arith a where

instance Type Bool where
  showType _ = "i1"
  getType = True
  hasResult _ = True
instance NonVoid Bool
instance Primitive Bool where
  showVal = map toLower . show
  readVal = read

instance Type Int where
  showType _ = "i32"
  getType = 0
  hasResult _ = True
instance Primitive Int where
  showVal = show
  readVal = read
instance NonVoid Int where
instance Arith Int where

instance Type () where
  showType _ = "void"
  getType = ()
  hasResult _ = False

newtype Pointer a = MkPointer a deriving (Eq, Show)

instance forall a. NonVoid a => Type (Pointer a) where
  showType (MkPointer a) = '*':showType a
  getType = MkPointer (getType :: a)
  hasResult _ = True

instance forall a. NonVoid a => NonVoid (Pointer a) where
