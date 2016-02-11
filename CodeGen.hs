module CodeGen (
  codeGen
) where

import LLVMRepresentation
import Types
import Data.List (intercalate)
import Text.Printf (printf)

class CodeGen a where
  codeGen :: a -> String

instance CodeGen Module where
  codeGen (MkModule { decls = ds, globals = gs }) = 
    "; Declarations:\n" ++ intercalate "\n" ds ++
    "\n\n; Globals:\n" ++ intercalate "\n" (map codeGen gs)

instance CodeGen AnyGlobal where
  codeGen (MkAnyGlobal global) = codeGen global

instance CodeGen (Global a) where
  codeGen (MkFunction { name = name, ret = ret, args = args, body = body }) =
    def ++ "\n" ++ bodyString ++ "\n}\n"
      where
        bodyString = intercalate "\n" $ map codeGen body
        argsString = intercalate "," $ map showAsArg args
        def = printf "define %s @%s(%s) {" (showType ret) name argsString

instance CodeGen AnyCode where
  codeGen (MkAnyCode c) = codeGen c

instance CodeGen (Code b) where
  codeGen MkCode { node = MkInstr str } = "  " ++ str
  codeGen _ = "reduce further"
