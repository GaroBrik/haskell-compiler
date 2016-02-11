{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Types (
  Type,
  getType,
  showType,
  showVal,
  hasResult,
  Pointer(MkPointer)
) where

import Data.Char

-- The "Type" type class encapsulates LLVM types.
-- The types are defined in this way instead of as a data type enumeration so
-- that haskell values can be identified with LLVM types easily, and so that
-- we can provide compile time guarantees of correctness of the compiler itself,
-- as this type class in addition to the definitions of "Code"s provides a sort
-- of weak dependent typing.

class (Show a, Eq a) => Type a where
  showType :: a -> String
  showVal :: a -> String
  getType :: Type a => a
  hasResult :: a -> Bool
  
instance Type Bool where
  showType _ = "i1"
  showVal = map toLower . show
  getType = True
  hasResult _ = True

instance Type Int where
  showType _ = "i32"
  showVal = show
  getType = 0
  hasResult _ = True

instance Type () where
  showType _ = "void"
  showVal _ = "voidVal"
  getType = ()
  hasResult _ = False

newtype Pointer a = MkPointer a deriving (Eq, Show)

instance forall a. Type a => Type (Pointer a) where
  showType (MkPointer a) = '*':showType a
  showVal _ = error "show value of pointer"
  getType = MkPointer (getType :: a)
  hasResult _ = True
