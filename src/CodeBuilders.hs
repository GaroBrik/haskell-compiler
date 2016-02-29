{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CodeBuilders (
  buildCode,
  CodeBuilder, CodeBuilderT, NodeBuilder, NodeBuilderT,
  getId,
  mkRet, mkBr, mkJmp, mkAlloca, mkStore, mkLoad, mkArithOp, mkCond,
  mkArithM, mkCondM
) where

import           Control.Applicative ((<$>))
import           Control.Monad.State (State, StateT, get, put, runState, state)
import           Data.Maybe          (catMaybes)
import           LLVMRepresentation
import           Text.Printf
import           Types

type CodeBuilder a = State Integer a
type NodeBuilder a = CodeBuilder (Node a)
type CodeBuilderT m a = StateT Integer m a
type NodeBuilderT m a = CodeBuilderT m (Node a)
-- type NodeBuilder a = CodeBuilder (Node a)

getId :: String -> CodeBuilder Identifier
getId str = state $ \i -> (Identifier $ str ++ show i, i+1)

buildCode :: Type a => (Identifier -> Node a) -> String -> NodeBuilder a
buildCode fn = fmap fn . getId

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

mkStore :: forall a. Type a => InstrArg (Pointer a) -> InstrArg a -> Node ()
mkStore idn arg =
  VoidInstr $ printf "store %s %s, %s* %s" tpeString (showArg arg) tpeString $ showArg idn
  where
    tpeString = showType (getType :: a)

mkLoad :: forall a. NonVoid a => InstrArg (Pointer a) -> Identifier -> Node a
mkLoad ptr load@(Identifier lstr) =
  Instr (printf "%s = load %s, %s %s" lstr valType ptrType $ showArg ptr) load
  where
    valType = showType (getType :: a)
    ptrType = showType (getType :: (Pointer a))

mkArithOp :: forall a. Arith a => Node a -> ArithOp -> Node a -> NodeBuilder a
mkArithOp left op right = buildCode code "arith"
  where code idn@(Identifier idnStr) =
          Block (map AnyNode $ catMaybes [getCode left, getCode right])
                (Instr (printf "%s = %s %s, %s %s" idnStr
                                                   (showOp op)
                                                   (showType (getType :: a))
                                                   (showArg $ getResult left)
                                                   (showArg $ getResult right))
                 idn)

mkCond :: forall a. Arith a => Node a -> Cond -> Node a -> NodeBuilder Bool
mkCond left op right = buildCode code "comp"
  where code idn@(Identifier idnStr) =
          Block (map AnyNode $ catMaybes [getCode left, getCode right])
                (Instr (printf "%s = icmp %s %s, %s %s" idnStr
                        (showCond op)
                        (showType (getType :: a))
                        (showArg $ getResult left)
                        (showArg $ getResult right))
                 idn)

mkIf :: forall a. NonVoid a => Node Bool -> Node a -> Node a -> NodeBuilder a
mkIf cond thn els = do
  labelIds@[thenId, elseId, endId] <- mapM getId ["then", "else", "end"]
  let [thenLabel, elseLabel, endLabel] = map (Just . AnyNode . Label) labelIds
  alloca <- mkAlloca
  let branch = Just . AnyNode $ mkBr (getResult cond) thenId elseId
      jump = Just . AnyNode $ mkJmp endId
      [storeThen, storeElse] =
        map (Just . AnyNode . mkStore (getResult alloca) . getResult) [thn, els]
  finalInstr <- buildCode (mkLoad (getResult alloca)) "ifload"
  return $ Block (catMaybes [AnyNode <$> getCode alloca,
                             AnyNode <$> getCode cond,
                             branch, thenLabel, AnyNode <$> getCode thn,
                             storeThen, jump, elseLabel,
                             AnyNode <$> getCode els, storeElse,
                             endLabel])
                 finalInstr

mkVoidIf :: forall a b. (Type a, Type b) =>
            Node Bool -> Node a -> Node b -> NodeBuilder ()
mkVoidIf cond thn els = do
  labelIds@[thenId, elseId, endId] <- mapM getId ["then", "else", "end"]
  let [thenLabel, elseLabel, _] = map (Just . AnyNode . Label) labelIds
  let branch = Just . AnyNode $ mkBr (getResult cond) thenId elseId
      jump = Just . AnyNode $ mkJmp endId
  return $ Block (catMaybes [AnyNode <$> getCode cond,
                             branch, thenLabel, AnyNode <$> getCode thn,
                             jump, elseLabel, AnyNode <$> getCode els])
                 (Label endId)


mkArithM :: forall a. Arith a =>
            NodeBuilder a -> ArithOp -> NodeBuilder a -> NodeBuilder a
mkArithM n1 n2 n3 = do
  n1' <- n1
  n3' <- n3
  mkArithOp n1' n2 n3'

mkCondM :: forall a. Arith a =>
           NodeBuilder a -> Cond -> NodeBuilder a -> NodeBuilder Bool
mkCondM n1 n2 n3 = do
   n1' <- n1
   n3' <- n3
   mkCond n1' n2 n3'
