{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVMRepresentation (
  Module(MkModule), decls, globals,
  AnyGlobal(MkAnyGlobal),
  Global(MkFunction), name, ret, body, args,
  Node(ArithOp, Comp, Const, Block, If, Instr, Ret, Label),
  ArithOp(Plus, Times, Minus, Div),
  Cond(Eq, Ne, Sgt, Sge, Slt, Sle),
  Code(MkCode), node, result,
  AnyCode(MkAnyCode),
  InstrArg(MkVal, MkId), Identifier,
  showAsArg,
  mapNode, mapAny, appAny
) where

import Types
import Test.QuickCheck

type Identifier = IORef String

data Module = MkModule { decls :: [String],
                         globals :: [AnyGlobal] }

data AnyGlobal where
  MkAnyGlobal :: Type a => Global a -> AnyGlobal

data Global a where
  MkFunction :: Type a => { name :: String,
                            ret :: a,
                            args :: [TypeString],
                            body :: [AnyCode] } -> Global a

data TypeString where
  MkTypeString :: Type tpe => tpe -> Identifier -> TypeString
showAsArg :: TypeString -> String
showAsArg (MkTypeString tpe nme) = nme ++ " " ++ showType tpe

data ArithOp = Plus | Times | Minus | Div deriving (Show, Eq)
data Cond = Eq | Ne | Sgt | Sge | Slt | Sle deriving (Show, Eq)

data Node a where
  ArithOp :: Primitive a => Code a -> ArithOp -> Code a -> Node a
  Comp :: Primitive a => Code a -> Cond -> Code a -> Node Bool
  Const :: Primitive a => a -> Node a
  Block :: Type a => [AnyCode] -> Code a -> Node a
  If :: Type a => (Code Bool) -> (Code a) -> (Code a) -> (Node a)
  Instr :: Type a => String -> Node a
  Ret :: Type a => Code a -> Node ()
  Label :: Node ()
deriving instance Show (Node a)

data Code a where
  MkCode :: Type a => { node :: Node a, result :: Identifier } -> Code a
deriving instance Show (Code a)

data AnyCode where 
  MkAnyCode :: Type a => Code a -> AnyCode
deriving instance Show AnyCode

data InstrArg a where
  MkVal :: Type a => a -> InstrArg a
  MkId :: Type a => Identifier -> InstrArg a
  
mapAny :: (forall a. Code a -> Code a) -> AnyCode -> AnyCode
mapAny f (MkAnyCode code) = MkAnyCode $ f code

appAny :: (forall a. Code a -> b) -> AnyCode -> b
appAny f (MkAnyCode c) = f c

mapNode :: Type b => (Node a -> Node b) -> Code a -> Code b
mapNode f c = MkCode { node = f (node c), result = result c }
