{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CodeBuilders (
  buildCode,
  CodeBuilder,
  getId,
  mkBinOp, mkRet, mkBr, mkJmp, mkAlloca, mkStore, mkLoad,
  mkVoidCode
) where

import Text.Printf
import Control.Monad.State (State, runState, get, put, state)
import LLVMRepresentation
import Types

type CodeBuilder a = State Integer a

getId :: String -> CodeBuilder String
getId str = state $ \i -> (str ++ show i, i+1)

buildCode :: Type a => String -> Node a -> CodeBuilder (Code a)
buildCode nme nod = do
  idn <- getId nme
  return (MkCode { node = nod, result = idn })

showTypedOp :: Type a => a -> Op -> String
showTypedOp a Plus = printf "%s %s" "add" (showType a)
showTypedOp a Times = printf "%s %s" "mul" (showType a)
showTypedOp a Minus = printf "%s %s" "sub" (showType a)
showTypedOp a Div = printf "%s %s" "div" (showType a)

showInstrArg :: InstrArg a -> String
showInstrArg (MkVal v) = showVal v
showInstrArg (MkId str) = '%':str

mkVoidCode :: Node () -> Code ()
mkVoidCode n = MkCode { node = n, result = "" }

mkBinOp :: forall a b c. (Type a, Type b, Type c) =>
           Identifier -> Op -> InstrArg b -> InstrArg c -> Node a
mkBinOp res op arg1 arg2 = MkInstr $
  printf  "%%%s = %s %s, %s" res
                             (showTypedOp (getType::a) op)
                             (showInstrArg arg1)
                             (showInstrArg arg2)

mkRet :: forall a. Type a => InstrArg a -> Node a
mkRet arg = MkInstr $ printf "ret %s %s" (showType (getType::a))
                                         (showInstrArg arg)

mkBr :: InstrArg Bool -> Identifier -> Identifier -> Code ()
mkBr arg thn els = mkVoidCode $ MkInstr $
  printf "br i1 %s, label %%%s, label %%%s" (showInstrArg arg) thn els

mkJmp :: Identifier -> Code ()
mkJmp lbl = mkVoidCode $ MkInstr $ printf "br label %%%s" lbl

mkAlloca :: forall a. Type a => CodeBuilder (Code (Pointer a))
mkAlloca = do
  allocaId <- getId "alloca"
  let instr = MkInstr$printf "%%%s = alloca %s" allocaId (showType (getType::a))
  return $ MkCode { node = instr, result = allocaId }

mkStore :: forall a. Type a => Identifier -> InstrArg a -> Code ()
mkStore idn arg = mkVoidCode instr where
  instr = MkInstr $ printf "store %s %s, %s* %s"
          tpeString (showInstrArg arg) tpeString idn
  tpeString = showType (getType :: a)

mkLoad :: forall a. Type a => Identifier -> Identifier -> Code a
mkLoad ptr load = MkCode { node = instr, result = load } where
  instr = MkInstr $ printf "%s = load %s, %s* %s"
          load tpeString tpeString ptr
  tpeString = showType (getType :: a)
