module Types (
  Type,
  deriveType,
  getType,
  showType
) where

-- The "Type" type class encapsulates LLVM types.
-- The types are defined in this way instead of as a data type enumeration so
-- that haskell values can be identified with LLVM types easily, and so that
-- we can provide compile time guarantees of correctness of the compiler itself,
-- as this type class in addition to the definitions of "Code"s provides a sort
-- of weak dependent typing.

class Type a where
  showType :: a -> String
  getType :: a
  deriveType :: d a -> a
  deriveType _ = getType
  
instance Type Bool where
  showType _ = "i1"
  getType = True

instance Type Int where
  showType _ = "i32"
  getType = 0

instance Type () where
  showType _ = "void"
  getType = ()
