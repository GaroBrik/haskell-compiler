{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVMRepresentation (
  Module(MkModule), decls, globals,
  Global,
  Function(MkFunction), name, ret, body, args,
  Node(MkBinOp, MkConst, MkBlock, MkIf, MkInstr, MkEmpty),
  Op(Plus, Times, Minus, Div),
  Code(MkCode), node, result,
  AnyCode(MkAnyCode),
  mapNode, mapAny,
  mkAssign
) where

import Types
import Text.Printf

data Module = MkModule { decls :: [String],
                         globals :: [AnyGlobal] }

data AnyGlobal where
  AnyGlobal :: Global a => a -> AnyGlobal

class Global a
instance Global (Function b)

data Function a where
  MkFunction :: Type a => { name :: String,
                            ret :: a,
                            args :: [TypeString],
                            body :: Code b } -> Function a

data TypeString where
  MkTypeString :: Type a => a -> String -> TypeString

data Op = Plus | Times | Minus | Div deriving Show

data Node a where
  MkBinOp :: Type a => Code a -> Op -> Code a -> Node a
  MkConst :: Type a => a -> Node a
  MkBlock :: Type a => [AnyCode] -> Code a -> Node a
  MkIf :: Type a => (Code Bool) -> (Code a) -> (Code a) -> (Node a)
  MkEmpty :: Node ()
  MkInstr :: Type a => String -> Node a
deriving instance Show (Node a)

data Code a where
  MkCode :: Type a => { node :: Node a, result :: String } -> Code a
deriving instance Show (Code a)

data AnyCode where 
  MkAnyCode :: Type a => Code a -> AnyCode
deriving instance Show AnyCode

showTypedOp :: Type a => a -> Op -> String
showTypedOp a Plus = printf "%s %s" "add" (showType a)
showTypedOp a Times = printf "%s %s" "mul" (showType a)
showTypedOp a Minus = printf "%s %s" "sub" (showType a)
showTypedOp a Div = printf "%s %s" "div" (showType a)

mkAssign :: forall a. Type a => String -> Op -> String -> String -> Node a
mkAssign res op arg1 arg2 = MkInstr $
  printf  "%%%s = %s %s, %s" res (showTypedOp (getType::a) op) arg1 arg2

mapAny :: (forall a. Code a -> Code a) -> AnyCode -> AnyCode
mapAny f (MkAnyCode code) = MkAnyCode $ f code

mapNode :: Type b => (Node a -> Node b) -> Code a -> Code b
mapNode f c = MkCode { node = f (node c), result = result c }
