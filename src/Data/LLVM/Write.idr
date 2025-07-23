module Data.LLVM.Write

import Data.LLVM.Types
import Data.LLVM.Class
%default covering
intercalate : Monoid b => b -> List b -> b 
intercalate sep [] = neutral
intercalate sep [x] = x
intercalate sep (x :: xs) = x <+> sep <+> intercalate sep xs

writeIf : Monoid b => (a -> b) -> Maybe a -> b
writeIf _ Nothing = neutral
writeIf f (Just x) = f x
encodeIf : Encode a b => Maybe a -> b
encodeIf = writeIf encode 
prefixed : Monoid m => m -> (m -> m)
prefixed p f = p <+> f
suffixed : Monoid m => m -> (m -> m)
suffixed s f = f <+> s
Encode a b => Encode (Maybe a) b where 
    encode Nothing = neutral
    encode (Just x) = encode x

seperated : Encode a String => String -> List a -> String
seperated sep vs = intercalate sep (map encode vs)
spaced : Encode a String => List a -> String
spaced vs = intercalate " " (map encode vs)
Monoid a => Encode a a where 
    encode = id
Encode Linkage String where
    encode Private = "private"
    encode Internal = "internal"
    encode Available = "available"
    encode LinkOnce = "linkonce"
    encode Weak = "weak"
    encode Common = "common"
    encode Appending = "appending"
    encode ExternWeak = "extern_weak"
    encode LinkOnceODR = "linkonce_odr"
    encode WeakODR = "weak_odr"
    encode External = "external"
Encode DLLStorage String where
    encode DLLExport = "dllexport"
    encode DLLImport = "dllimport"
Encode ThreadLocality String where
    encode LocalDynamic = "localdynamic"
    encode InitialExec = "initialexec"
    encode LocalExec = "localexec"
Encode Preemption String where
    encode Preemptible = "dso_preemptible"
    encode NonPreemptible = "dso_local"
Encode AddressSpace String where 
    encode (NamedSpace name) = "addrspace(" ++ name ++ ")"
    encode (UnnamedSpace n) = "addrspace(" ++ show n ++ ")"

Encode CallingConvention String where 
    encode C = "cc"
    encode Fast = "fastcc"
    encode Cold = "coldcc"
    encode GHC = "ghccc"
    encode CC11 = "cc11"
    encode AnyReg = "anyregcc"
    encode PreserveMost = "preserve_mostcc"
    encode PreserveAll = "preserve_allcc"
    encode PreserveNone = "preserve_nonecc"
    encode CxxFastTL = "cxx_fast_tlscc"
    encode Tail = "tailcc"
    encode Swift = "swiftcc"
    encode SwiftTail = "swift_tailcc"
    encode CFGuardCheck = "cfguard_checkcc"
    encode (CustomCC n) = "cc " ++ show n
Encode Visibility String where 
    encode Default = "default"
    encode Hidden = "hidden"
    encode Protected = "protected"
Encode Name String where
    encode (Local name) = "%" ++ name
    encode (Global name) = "@" ++ name
    encode (Special name) = "$" ++ name
    encode (CustomN name) = name
Encode LTypeF String where 
    encode Half = "half"
    encode Bfloat = "bfloat"
    encode LFloat = "float"
    encode LDouble = "double"
    encode FP128 = "fp128"
    encode X86_FP80 = "x86_fp80"
    encode PPC_FP128 = "ppc_fp128"

Encode SymbolInfo String where  
    encode (MkSymbolInfo linkage preemption visibility dllStorage) = 
        encode linkage ++ " " ++ encode preemption ++ " " ++ encode visibility ++ " " ++ encode dllStorage
Encode LType String where
    encode LPtr = "ptr"
    encode (LPtrAddr space) = "ptr " ++ encode space
    encode LVoid = "void"
    encode (LFun ret args) = encode ret ++ " ( " ++ encode ret ++ ", " ++ intercalate ", " (map encode args) ++ ")"
    encode (LFunVarArg ret args varArg) = encode ret ++ " ( " ++ intercalate ", " (map encode args) ++ "," ++ encode varArg ++ " ... )"
    encode LOpaque = "opaque"
    encode (LInt n) = "i" ++ show n
    encode (LFloating _) = "float"  -- Placeholder for actual float encoding
    encode (LVector s t) = "vector"  -- Placeholder for actual vector encoding
    encode LLabel = "label"
    encode LToken = "token"
    encode (LArray n t) = "[" ++ show n ++ " x " ++ encode t ++ "]"
    encode (LStruct ts) = "{" ++ intercalate ", " (map encode ts) ++ "}"
    encode (LPackedStruct ts) = "<{" ++ intercalate ", " (map encode ts) ++ "}>"
    encode LMetadata = "metadata"
    encode (LVectorScale s t) = "< vscale x " ++ show s ++ " x " ++ encode t ++ " >"
    encode LX86_AMX = "x86_amx"
Encode AddressInfo String where
    encode UnnamedGlobal = "unnamed_addr"
    encode UnamedLocal = "local_unnamed_addr"

Encode LTag String where
    encode (CTag s) = s
Encode GVarDef String where
    encode (MkGVarDef name symbolInfo threadLocality addressInfo addressSpace externallyInitialized global tpe init tags) =
        "@" ++ name ++ " = " ++ spaced [
            encode symbolInfo,
            encodeIf threadLocality,
            encodeIf addressInfo,
            encodeIf addressSpace,
            if externallyInitialized == Just True then "external" else "",
            if global then "global" else "constant",
            encode tpe,
            writeIf (\i => "initializer " ++ i) init,
            intercalate ", " (map encode tags)
        ]