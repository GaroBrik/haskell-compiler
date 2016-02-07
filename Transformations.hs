{-# LANGUAGE Rank2Types #-}

module Transformations (


) where

import LLVMRepresentation
import Types

mapCode :: (forall a. Code a -> Code a) -> Code b -> Code b
mapCode fn code = fn $ code { node = recursed }
  where re = mapCode fn
        recursed = case node code of
          MkBinOp left op right -> MkBinOp (re left) op (re right)
          MkBlock codes final -> MkBlock (map (mapAny re) codes) (re final)
          MkIf cond thn els -> MkIf (re cond) (re thn) (re els)
          _ -> node code

checkForConst :: Code a -> (Code a, String)
checkForConst c@(MkCode { node = MkConst val }) = (c, showVal val)
checkForConst otw = (otw, result otw)

eliminateBinOps :: Code a -> Code a
eliminateBinOps code@(MkCode { node = MkBinOp left op right, result = res }) =
  code { node = MkBlock [MkAnyCode leftCode, MkAnyCode rightCode] instr }
  where (leftCode, leftRes) = checkForConst left
        (rightCode, rightRes) = checkForConst right
        instr = code { node = mkAssign res op leftRes rightRes }
eliminateBinOps otw = otw
