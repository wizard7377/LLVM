module Data.LLVM.Write

import Data.LLVM.Types
import Data.LLVM.Class

intercalate : Monoid b => b -> List b -> b 
intercalate sep [] = neutral
intercalate sep [x] = x
intercalate sep (x :: xs) = x <+> sep <+> intercalate sep xs

Encode LTypeF String 
Encode LType String where
    encode LPtr = "ptr"
    encode LVoid = "void"
    encode (LFun ret args) = encode ret ++ " ( " ++ encode ret ++ ", " ++ intercalate ", " (map encode args) ++ ")"
    encode (LFunVarArg ret args varArg) = encode ret ++ " ( " ++ intercalate ", " (map encode args) ++ "," ++ encode varArg ++ " ... )"
    encode LOpaque = "opaque"
    encode (LInt n) = "i" ++ show n
    encode (LFloating _) = "float"  -- Placeholder for actual float encoding
    encode (LVector s t) = "vector"  -- Placeholder for actual vector encoding
    encode LLabel = "label"
    encode LToken = "token"
    encode (LArray t n) = "[" ++ show n ++ " x " ++ encode t ++ "]"
    encode (LStruct ts) = "{" ++ intercalate ", " (map encode ts) ++ "}"

