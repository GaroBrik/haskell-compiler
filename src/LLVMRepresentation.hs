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
newtype Identifier = Identifier String
instance Show Identifier where
  show (Identifier str) = str

data Module = MkModule { decls :: [String],
                         globals :: [AnyGlobal] }

data AnyGlobal where
  MkAnyGlobal :: Type a => Global a -> AnyGlobal

data Global a where
  MkFunction :: Type a => { name :: String,
                            ret :: a,
                            args :: [TypeString],
                            body :: [AnyNode] } -> Global a

data TypeString where
  MkTypeString :: Type tpe => tpe -> Identifier -> TypeString
showAsArg :: TypeString -> String
showAsArg (MkTypeString tpe nme) = show nme ++ " " ++ showType tpe

data ArithOp = Plus | Times | Minus | Div deriving (Show, Eq)
data Cond = Eq | Ne | Sgt | Sge | Slt | Sle deriving (Show, Eq)

data Node a where
  Const :: Primitive a => a -> Node a
  Block :: Type a => [AnyNode] -> Node a -> Node a
  Instr :: NonVoid a => String -> Identifier -> Node a
  VoidInstr :: String -> Node ()
  Label :: Identifier -> Node ()
deriving instance Show (Node a)

data AnyNode where
  AnyNode :: Type a => Node a -> AnyNode
instance Show AnyNode where
  show (AnyNode node) = show node

getResult :: NonVoid a => Node a -> Identifier
getResult (Const val) = showVal val
getResult (Block nodes node) = getResult node
getResult (Instr str id) = id

data InstrArg a where
  MkVal :: Type a => a -> InstrArg a
  MkId :: Type a => Identifier -> InstrArg a
