module Main where

import System.Process as P
import LLVMRepresentation
import CodeGen
import CodeBuilders
import Types
import Transformations
import Control.Monad.State

testSeven :: CodeBuilder (Code Int)
testSeven = do
  three <- buildCode "three" $ MkConst 3
  four <- buildCode "four" $ MkConst 4
  seven <- buildCode "seven" $ MkArithOp three Plus four
  eight <- buildCode "eight" $ MkArithOp four Plus four
  fifteen <- buildCode "fifteen" $ MkArithOp seven Plus eight
  buildCode "" $ MkRet fifteen

main :: IO ()
main = do
  let fn = MkFunction { name = "main",
                        ret = getType::Int,
                        args = [],
                        body = evalState (compile =<< testSeven) 0 }
      modul = MkModule { decls = [], globals = [MkAnyGlobal fn] }
  putStrLn $ codeGen modul
  -- _ <- createProcess (P.proc "lli" ["prog.lli"])
  return ()
