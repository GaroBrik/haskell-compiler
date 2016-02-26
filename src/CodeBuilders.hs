{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CodeBuilders (
  buildCode,
  CodeBuilder,
  getId,
  mkRet, mkBr, mkJmp, mkAlloca, mkStore, mkLoad
) where

import Text.Printf
import Control.Monad.State (State, runState, get, put, state)
import Control.Monad(liftM)
import LLVMRepresentation
import Types

type CodeBuilder a = State Integer a

getId :: String -> CodeBuilder Identifier
getId str = state $ \i -> (Identifier $ str ++ show i, i+1)

buildCode :: Type a => (Identifier -> Node a) -> String -> CodeBuilder (Node a)
buildCode fn = liftM fn . getId

showOp :: ArithOp -> String
showOp Plus = "add"
showOp Times = "mul"
showOp Minus = "sub"
showOp Div = "div"

showTypedOp :: Type a => a -> ArithOp -> String
showTypedOp a op = printf "%s %s" (showOp op) (showType a)

showCond :: Cond -> String
showCond Eq = "eq"
showCond Ne = "ne"
showCond Sgt = "sgt"
showCond Sge = "sge"
showCond Slt = "slt"
showCond Sle = "sle"

showTypedCond :: Type a => a -> Cond -> String
showTypedCond a cond = printf "icmp %s %s" (showCond cond) (showType a)

mkRet :: forall a. NonVoid a => InstrArg a -> Node ()
mkRet arg = VoidInstr $ printf "ret %s %s" (showType (getType::a))
                                           (showArg arg)

mkBr :: InstrArg Bool -> Identifier -> Identifier -> Node ()
mkBr arg (Identifier thn) (Identifier els) = VoidInstr $
  printf "br i1 %s, label %%%s, label %%%s" (showArg arg) thn els

mkJmp :: Identifier -> Node ()
mkJmp (Identifier idn) = VoidInstr $ printf "br label %%%s" idn

mkAlloca :: forall a. NonVoid a => CodeBuilder (Node (Pointer a))
mkAlloca = do
  allocaId@(Identifier str) <- getId "alloca"
  return $ Instr (printf "%%%s = alloca %s" str (showType (getType::a))) allocaId

mkStore :: forall a. Type a => Identifier -> InstrArg a -> Node ()
mkStore (Identifier idn) arg =
  VoidInstr $ printf "store %s %s, %s* %s" tpeString (showArg arg) tpeString idn
  where
    tpeString = showType (getType :: a)

mkLoad :: forall a. NonVoid a => InstrArg (Pointer a) -> Identifier -> Node a
mkLoad ptr load@(Identifier lstr) =
  Instr (printf "%s = load %s, %s %s" lstr valType ptrType $ showArg ptr) $ load
  where
    valType = showType (getType :: a)
    ptrType = showType (getType :: (Pointer a))
