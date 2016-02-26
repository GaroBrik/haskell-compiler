{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVMRepresentation (
  Module(MkModule), decls, globals,
  AnyGlobal(MkAnyGlobal),
  Global(MkFunction), name, ret, body, args,
  Node(..),
  ArithOp(Plus, Times, Minus, Div),
  Cond(Eq, Ne, Sgt, Sge, Slt, Sle),
  AnyNode(..),
  InstrArg, getResult, showArg,
  Identifier(..),
  showAsArg
) where

import Types
import Data.String (IsString)

newtype Identifier = Identifier String deriving (IsString, Show)
newtype NonVoid a => InstrArg a = InstrArg String deriving (IsString, Show)

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
showAsArg (MkTypeString tpe (Identifier nme)) = nme ++ " " ++ showType tpe

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

getResult :: NonVoid a => Node a -> InstrArg a
getResult (Const val) = InstrArg $ showVal val
getResult (Block _ node) = getResult node
getResult (Instr _ (Identifier str)) = InstrArg str
getResult _ = undefined

showArg :: InstrArg a -> String
showArg (InstrArg str) = str
