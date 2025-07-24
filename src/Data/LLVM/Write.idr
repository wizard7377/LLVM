module Data.LLVM.Write

import Data.LLVM.Core
import Data.LLVM.Class
import Data.LLVM.Ops
import Data.LLVM.Program

%default covering

Encode Linkage VString where
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
export
Encode DLLStorage VString where
    encode DLLExport = "dllexport"
    encode DLLImport = "dllimport"
export
Encode ThreadLocality VString where
    encode LocalDynamic = "localdynamic"
    encode InitialExec = "initialexec"
    encode LocalExec = "localexec"
export
Encode Preemption VString where
    encode Preemptible = "dso_preemptible"
    encode NonPreemptible = "dso_local"
export
Encode AddressSpace VString where 
    encode (NamedSpace name) = "addrspace(" <+> cast name <+> ")"
    encode (UnnamedSpace n) = "addrspace(" <+> vshow n <+> ")"

export
Encode CallingConvention VString where 
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
    encode (CustomCC n) = "cc " <+> vshow n
export
Encode Visibility VString where 
    encode Default = "default"
    encode Hidden = "hidden"
    encode Protected = "protected"
export
Encode Name VString where
    encode (Local name) = "%" <+> cast name
    encode (Global name) = "@" <+> cast name
    encode (Special name) = "$" <+> cast name
    encode (MetadataN name) = "!" <+> cast name
    encode (CustomN name) = cast name
export
Encode LTypeF VString where 
    encode Half = "half"
    encode Bfloat = "bfloat"
    encode LFloat = "float"
    encode LDouble = "double"
    encode FP128 = "fp128"
    encode X86_FP80 = "x86_fp80"
    encode PPC_FP128 = "ppc_fp128"
export
Encode SymbolInfo VString where  
    encode (MkSymbolInfo linkage preemption visibility dllStorage) = 
        encode linkage <+> " " <+> encode preemption <+> " " <+> encode visibility <+> " " <+> encode dllStorage
export
Encode LType VString where
    encode LPtr = "ptr"
    encode (LPtrAddr space) = "ptr " <+> encode space
    encode LVoid = "void"
    encode (LFun ret args) = encode ret <+> " ( " <+> encode ret <+> ", " <+> intercalate ", " (map encode args) <+> ")"
    encode (LFunVarArg ret args varArg) = encode ret <+> " ( " <+> intercalate ", " (map encode args) <+> "," <+> encode varArg <+> " ... )"
    encode LOpaque = "opaque"
    encode (LInt n) = "i" <+> vshow n
    encode (LFloating _) = "float"  -- Placeholder for actual float encoding
    encode (LVector s t) = "vector"  -- Placeholder for actual vector encoding
    encode LLabel = "label"
    encode LToken = "token"
    encode (LArray n t) = "[" <+> vshow n <+> " x " <+> encode t <+> "]"
    encode (LStruct ts) = "{" <+> intercalate ", " (map encode ts) <+> "}"
    encode (LPackedStruct ts) = "<{" <+> intercalate ", " (map encode ts) <+> "}>"
    encode LMetadata = "metadata"
    encode (LVectorScale s t) = "< vscale x " <+> vshow s <+> " x " <+> encode t <+> " >"
    encode LX86_AMX = "x86_amx"

export
Encode AddressInfo VString where
    encode UnnamedGlobal = "unnamed_addr"
    encode UnamedLocal = "local_unnamed_addr"
export
Encode LTag VString where
    encode (CTag s) = cast s
export
Encode GVarDef VString where
    encode (MkGVarDef name symbolInfo threadLocality addressInfo addressSpace externallyInitialized global tpe init tags) =
        let r : VString = "@" <+> cast name <+> " = " <+> spaced' [
            encode symbolInfo,
            encodeIf threadLocality,
            encodeIf addressInfo,
            encodeIf addressSpace,
            if externallyInitialized == Just True then "external" else "",
            if global then "global" else "constant",
            encode tpe,
            cast $ writeIf (\i => "initializer " <+> i) init,
            intercalate ", " (map (encode) tags)
        ] in r





export
Encode Attribute VString where 
    encode ZeroExt = "zeroext"
    encode SignExt = "signext"
    encode NoExt = "noext"
    encode (ByVal t) = "byval(" <+> encode t <+> ")"
    encode (ByRef t) = "byref(" <+> encode t <+> ")"
    encode (Preallocated t) = "preallocated(" <+> encode t <+> ")"
    encode (Inalloca t) = "inalloca(" <+> encode t <+> ")"
    encode (SRet t) = "sret(" <+> encode t <+> ")"
    encode (Align n) = "align " <+> vshow n       
    encode NoAlias = "noalias"
    encode NoFree = "nofree"
    encode Nest = "nest"
    encode Returned = "returned"
    encode NoNull = "nonnull"
    encode (Dereferenceable n) = "dereferenceable(" <+> vshow n <+> ")"
    encode (DereferenceableOrNull n) = "dereferenceable_or_null(" <+> vshow n <+> ")"
    encode SwiftSelf = "swiftself"
    encode SwiftAsync = "swiftasync"
    encode SwiftError = "swifterror"
    encode ImmArg = "immarg"
    encode NoUndef = "nounwind"
    encode (AlignStack n) = "alignstack(" <+> vshow n <+> ")"
    encode AllocAlign = "allocalign"
    encode AllocPtr = "allocptr"
    encode ReadNone = "readnone"
    encode ReadOnly = "readonly"
    encode WriteOnly = "writeonly"
    encode Writeable = "writeable"
    encode DeadOnUnwind = "dead_on_unwind"
    encode DeadOnReturn = "dead_on_return"
    encode (NamedAttribute name) = cast name

export
Encode Metadata VString where
    encode = const "" -- TODO: 

export
Encode LConst VString where
    encode (LInt n) = cast $ show n
    encode (LFloat s) = cast $ s
    encode (LBool b) = if b then "true" else "false"
    encode LNull = "null"
    encode LToken = "none"
    encode (LString s) = "c\"" <+> cast s <+> "\""
    encode (LArray cs) = "[" <+> intercalate ", " (map (encode . value) cs) <+> "]"
    encode (LVector cs) = "<" <+> intercalate ", " (map (encode . value) cs) <+> ">"
    encode (LStruct cs) = "{" <+> intercalate ", " (map (encode . value) cs) <+> "}"
    encode LUndefined = "undef"
    encode LPoison = "poison"
    encode LZero = "zeroinitializer"
    encode (LMetadata m) = encode m
    encode (LPtr name) = encode name
export
Encode LExpr VString where
    encode (LConst c) = encode c
export
Encode CaseBranch VString where
    encode (MkCaseBranch tpe value label) =
        spaced [
            encode' tpe,
            encode' value,
            ", label",
            encode' label
        ]

export
Encode InvokeCall VString where
    encode (MkInvokeCall cc returnAttrs addressSpace tpe fnval args normal unwind) = 
        spaced [
            "invoke",
            encodeIf cc,
            intercalate ", " (map encode returnAttrs),
            encodeIf addressSpace,
            encode tpe,
            encode fnval,
            "(",
            intercalate ", " (map encode args),
            -- TODO:
            ")",
            -- TODO:
            "to label",
            encode normal,
            "unwind label",
            encode' unwind
        ]
export
Encode Wrapping VString where
    encode NoSigned = "nuw"
    encode NoUnsigned = "nsw"
    encode NoSignedUnsigned = "nuw nsw"
export
Encode UnaryOpcode VString where
    encode FNeg = "fneg"

export
Encode BinaryOpcode VString where
    encode Add = "add"
    encode (AddWrap wrapping) = "add " <+> encode wrapping
    encode (FAdd fastMath) = "fadd " -- TODO: <+> encode wrapping
    encode Sub = "sub"
    encode (SubWrap wrapping) = "sub " <+> encode wrapping
    encode (FSub fastMath) = "fsub " -- TODO: <+> encode fastMath
    encode Mul = "mul"
    encode (MulWrap wrapping) = "mul " <+> encode wrapping
    encode (FMul fastMath) = "fmul " -- TODO: <+> encode fastMath
    encode UDiv = "udiv"
    --encode (UDiv) = "udiv "
    encode (FDiv fastMath) = "fdiv " -- TODO: <+>   
    encode UDivExact = "udiv exact"
    encode (SDiv) = "sdiv " -- TODO: <+> encode fastMath
    encode (SDivExact) = "sdiv exact"
    --encode (FDiv fastMath) = "fdiv " -- TODO: <+>
    encode SRem = "srem"
    encode URem = "urem"
    encode (FRem fastMath) = "frem " -- TODO: <+>
    encode Shl = "shl"
    encode (ShlWrap wrapping) = "shl " <+> encode wrapping
    encode LShr = "lshr"
    encode LShrExact = "lshr exact"
    encode AShr = "ashr"
    encode AShrExact = "ashr exact"
    encode And = "and"
    encode Or = "or"
    encode Xor = "xor"
    encode DisjointOr = "or disjoint"

Encode BrCall VString where 
    
    encode (MkBrCall cc returnAttrs addressSpace tpe fnval args fallthrough indirect) = 
        spaced [
            "callbr",
            encodeIf cc,
            encode @{nosep} returnAttrs,
            encodeIf addressSpace,
            encode tpe,
            encode fnval,
            "(",
            encode @{each} args,
            ")",
            "to label",
            encode fallthrough,
            encode @{each} indirect
        ]

Encode CatchSwitch VString where
    encode (MkCatchSwitch name parent handler (Just caller)) =
        spaced [
            "catchswitch within",
            encode name,
            encode parent,
            encode @{each} $ (prefixed "label " . encode' <$> handler),
            "unwind label",
            encode caller
        ]
    encode (MkCatchSwitch name parent handler Nothing) =
        spaced [
            "catchswitch within",
            encode name,
            encode parent,
            encode @{each} $ (prefixed "label " . encode' <$> handler),
            "unwind to default"
        ]

Encode FastMathFlag VString where 
    encode FFast = "fast"
    encode NoNaNs = "nnan"
    encode NoInfs = "ninf"
    encode NoSignedZeros = "nsz"
Encode TailCall VString where 
    encode Tail = "tail"
    encode MustTail = "musttail"
    encode NoTail = "notail"
Encode FnCall VString where
    encode (MkFnCall tail fastMath cc returnAttrs addressSpace tpe fnval args) =
        spaced [
            encode tail,
            "call",
            encode @{nosep} fastMath,
            encodeIf cc,
            encode @{nosep} returnAttrs,
            encodeIf addressSpace,
            encode tpe,
            encode fnval,
            "(",
            encode @{each} args,
            ")"
        ]

Encode a VString => Encode (WithType a) VString where 
    encode (MkWithType t v) = encode t <+> " " <+> encode v
Encode Terminator VString where
    encode RetVoid = "ret void"
    encode (Ret ty expr) = "ret " <+> encode ty <+> " " <+> encode expr
    encode (CondBr cond true false) =
        "br i1 " <+> encode cond <+> ", label" <+> encode true <+> ", " <+> encode false 
    encode (JumpBr label) =
        "br label" <+> encode label
    encode (Switch ty expr defaultBranch cases) =
        "switch " <+> encode ty <+> encode expr <+> ", label" <+> encode defaultBranch <+> intercalate ", " (map encode cases)
    encode (IndirectBr address labels) = 
        "indirectbr ptr" <+>
        encode address <+> 
        ", [" <+>
        intercalate ", " (map (prefixed "label " . encode) labels) <+>
        "]"
    encode (Invoke invokeCall) =
        encode invokeCall
    encode (CallBR callBr) =
        encode callBr
    encode (Resume ty val) =
        "resume" <+> encode ty <+> encode val
    encode Unreachable =
        "unreachable"
    encode (CatchSwitchOp catchSwitch) =
        encode catchSwitch
    encode (CatchRet expr label) =
        "catchret from " <+> encode expr <+> " to label" <+> encode label
    encode (CleanupRetCaller expr) =
        "cleanupret from " <+> encode expr <+> "unwind to caller"
    encode (CleanupRet expr label) =
        "cleanupret from " <+> encode expr <+> " unwind label" <+> encode label