{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transformations (
  compile
) where

import LLVMRepresentation
import Types
import Data.Maybe (catMaybes)
import CodeBuilders
import Control.Monad (liftM, liftM2, liftM3, foldM)

type CodeTransformer = forall a. Code a -> CodeBuilder (Code a)

mapCode :: CodeTransformer -> CodeTransformer
mapCode fn code = do
  let re = mapCode fn
  recursed <- case node code of
    MkBinOp left op right -> liftM3 MkBinOp (re left) (return op) (re right)
    MkBlock codes final ->
      liftM2 MkBlock (mapM (\(MkAnyCode c) -> liftM MkAnyCode $ re c) codes)
                     (re final)
    MkIf cond thn els -> liftM3 MkIf (re cond) (re thn) (re els)
    MkRet c -> liftM MkRet $ re c
    _ -> return $ node code
  fn $ code { node = recursed }

-- mapCodeUntyped :: (forall a b. Code a -> Code b) -> Code c -> Code d
-- mapCodeUntyped fn code = fn $ code { node = recursed }
--   where re = mapCode fn
--         recursed = case node code of
--           MkBlock codes final -> MkBlock (map (mapAny $ mapCode fn) codes) (re final)
--           MkBinOp left op right -> MkBinOp (re left) op (re right)
--           MkIf cond thn els -> MkIf (mapCode fn cond) (re thn) (re els)
--           _ -> node code

checkForConst :: Type a => Code a -> (Maybe AnyCode, InstrArg a)
checkForConst MkCode { node = MkConst val } =
  (Nothing, MkVal val)
checkForConst otw = (Just $ MkAnyCode otw, MkId $ result otw)

eliminateBinOps :: CodeTransformer
eliminateBinOps code@(MkCode { node = MkBinOp left op right, result = res }) =
  return $ code { node = MkBlock (catMaybes [leftCode, rightCode]) instr }
  where (leftCode, leftRes) = checkForConst left
        (rightCode, rightRes) = checkForConst right
        instr = code { node = mkBinOp res op leftRes rightRes }
eliminateBinOps otw = return otw

eliminateRets :: CodeTransformer
eliminateRets code@(MkCode { node = MkRet arg }) =
  return $ code { node = MkBlock (catMaybes [argCode]) instr }
  where (argCode, argRes) = checkForConst arg
        instr = code { node = mkRet argRes }
eliminateRets otw = return otw

eliminateIfs :: forall a. Code a -> CodeBuilder (Code a)
eliminateIfs code@(MkCode { node = MkIf cond thn els, result = res }) = do
  thenLabel@(MkCode { result = thenId }) <- buildCode "then" MkLabel
                                            :: CodeBuilder (Code a)
  elseLabel@(MkCode { result = elseId }) <- buildCode "else" MkLabel
                                            :: CodeBuilder (Code a)
  endLabel@(MkCode { result = endId }) <- buildCode "endIf" MkLabel
  allocaCode@(MkCode { result = allocaId }) <- mkAlloca
                                               :: CodeBuilder (Code (Pointer a))
  let has = hasResult (getType :: a)
      ifHas = if has then Just else const Nothing
      (condCode, condRes) = checkForConst cond
      (thnCode, thnRes) = checkForConst thn
      (elsCode, elsRes) = checkForConst els
      branch = Just . MkAnyCode $ mkBr condRes thenId elseId
      jump = Just . MkAnyCode $ mkJmp endId
      storeThen = ifHas . MkAnyCode $ mkStore allocaId thnRes
      storeElse = ifHas .MkAnyCode $ mkStore allocaId elsRes
      finalInstr = if has then mkLoad res allocaId else endLabel
      node = MkBlock (catMaybes [condCode, ifHas $ MkAnyCode allocaCode, branch,
                                 Just $ MkAnyCode thenLabel, thnCode, storeThen,
                                 jump, Just $ MkAnyCode elseLabel, elsCode,
                                 storeElse, ifHas $ MkAnyCode endLabel])
                     finalInstr
  return code { node = node }

eliminateBlocks :: AnyCode -> [AnyCode]
eliminateBlocks (MkAnyCode MkCode { node = MkBlock codes code }) =
  foldl (++) [] . map eliminateBlocks $ codes ++ [MkAnyCode code]
eliminateBlocks otw = [otw]

transformSequence :: [Code a -> CodeBuilder (Code a)]
transformSequence =
  let cons = (:) :: CodeTransformer -> [CodeTransformer] -> [CodeTransformer] in
  map mapCode $ cons eliminateRets $ cons eliminateBinOps []

compile :: Type a => Code a -> CodeBuilder [AnyCode]
compile code = liftM (eliminateBlocks . MkAnyCode) $
               foldM (flip ($)) code transformSequence
