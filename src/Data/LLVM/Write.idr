||| LLVM IR serialization to textual representation.
|||
||| This module provides Encode instances for all LLVM IR data types,
||| allowing them to be converted to their textual LLVM IR representation.
||| It handles proper formatting, escaping, and syntax generation for
||| a complete LLVM IR module.
module Data.LLVM.Write

import Data.LLVM.IR
import Data.LLVM.Class
import Data.Walk
%default covering

export 
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
    encode (NamedSpace name) = "addrspace(" <+> go name <+> ")"
    encode (UnnamedSpace n) = "addrspace(" <+> vshow n <+> ")"

export
Encode CallingConvention VString where 
    encode C = "ccc"
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
    encode (Local name) = "%" <++> go name
    encode (Global name) = "@" <++> go name
    encode (IntrinsicN name) = "@llvm." <++> go name
    encode (Special name) = "$" <++> go name
    encode (MetadataN name) = "!" <+> go name
    encode (CustomN name) = go name
    encode (AttributeN name) = "#" <++> go name
    encode (LabelN name) =  go name <++> ":"
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
        encode linkage <+> encode preemption <+> encode visibility <+> encode dllStorage
export
Encode LType VString where
    encode LPtr = "ptr"
    encode (LPtrAddr space) = "ptr " <+> encode space
    encode LVoid = "void"
    encode (LFun ret args) = encode ret <+> "(" <+> encode ret <+> "," <+> intercalate "," (map encode args) <+> ")"
    encode (LFunVarArg ret args varArg) = encode ret <+> "(" <+> intercalate "," (map encode args) <+> "," <+> encode varArg <+> " ... )"
    encode LOpaque = "opaque"
    encode (LInt n) = "i" <++> vshow n
    encode (LFloating _) = "float"  -- Placeholder for actual float encoding
    encode (LVector s t) =  "<" <+> vshow s <+> "x" <+> encode t <+> ">"  -- Placeholder for actual vector encoding
    encode LLabel = "label"
    encode LToken = "token"
    encode (LArray n t) = "[" <+> vshow n <+> "x" <+> encode t <+> "]"
    encode (LStruct ts) = "{" <+> intercalate "," (map encode ts) <+> "}"
    encode (LPackedStruct ts) = "<{" <+> intercalate "," (map encode ts) <+> "}>"
    encode LMetadata = "metadata"
    encode (LVectorScale s t) = "< vscale x" <+> vshow s <+> "x" <+> encode t <+> ">"
    encode LX86_AMX = "x86_amx"
public export
[ewt] Encode a VString => Encode (WithType a) VString where 
    encode (MkWithType t v) = encode t <+> encode v

public export
Encode a VString => Encode (WithType a) VString where 
    encode (MkWithType t v) = encode t <+> encode v
export
Encode AddressInfo VString where
    encode UnnamedGlobal = "unnamed_addr"
    encode UnnamedLocal = "local_unnamed_addr"
export
Encode LTag VString where
    encode (CTag s) = go s

mutual 
    export
    Encode Metadata VString where
        encode (MetadataNamed name) = "!" <++> go name
        encode (MetadataString s) = "!\"" <++> go s <++> "\""
        encode (MetadataValue v) = encode v
        encode (MetadataTuple vals) = "!{" <+> encode @{each} vals <+> "}"
        encode (MetadataCustom s) = go s


    export
    Encode LConst VString where
        encode (LInt n) = go $ show n
        encode (LFloat s) = go $ s
        encode (LBool b) = if b then "true" else "false"
        encode LNull = "null"
        encode LToken = "none"
        encode (LString s) = "c\"" <+> go s <+> "\""
        encode (LArray cs) = "[" <+> intercalate "," (map encode cs) <+> "]"
        encode (LVector cs) = "<" <+> intercalate "," (map encode cs) <+> ">"
        encode (LStruct cs) = "{" <+> intercalate "," (map encode cs) <+> "}"
        encode LUndefined = "undef"
        encode LPoison = "poison"
        encode LZero = "zeroinitializer"
        encode (LMetadata m) = "metadata" <+> encode m
        encode (LPtr name) = encode name
export 
Encode GVarDef VString where
    encode (MkGVarDef name symbolInfo threadLocality addressInfo addressSpace externallyInitialized global tpe init tags) =
        let r : VString = "@" <++> go name <+> "=" <+> spaced' [
            encode symbolInfo,
            encodeIf threadLocality,
            encodeIf addressInfo,
            encodeIf addressSpace,
            if externallyInitialized == Just True then "external" else "",
            if global then "global" else "constant",
            encode tpe,
            encode @{just} init,
            intercalate "," (map (encode) tags)
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
    encode (OtherAttribute name) = go name


export
Encode LExpr VString where
    encode (LConstE c) = encode c
    encode (LVar name) = encode name
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
            intercalate "," (map encode returnAttrs),
            encodeIf addressSpace,
            encode tpe,
            encode fnval,
            "(",
            intercalate "," (map encode args),
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
public export
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
public export
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


public export
Encode FastMathFlag VString where 
    encode FFast = "fast"
    encode NoNaNs = "nnan"
    encode NoInfs = "ninf"
    encode NoSignedZeros = "nsz"
public export
Encode TailCall VString where 
    encode Tail = "tail"
    encode MustTail = "musttail"
    encode NoTail = "notail"
public export
Encode FnCall VString where
    encode (MkFnCall tail fastMath cc returnAttrs addressSpace tpe fnval args attrs) =
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
            ")",
            encode @{nosep} attrs
        ]




public export
Encode Terminator VString where
    encode RetVoid = "ret void"
    encode (Ret ty expr) = "ret " <+> encode ty <+> " " <+> encode expr
    encode (CondBr cond true false) =
        "br i1 " <+> encode cond <+> ", label" <+> encode true <+> "," <+> encode false 
    encode (JumpBr label) =
        "br label" <+> encode label
    encode (Switch ty expr defaultBranch cases) =
        "switch " <+> encode ty <+> encode expr <+> ", label" <+> encode defaultBranch <+> "[" <+> intercalate "\n" (map encode cases) <+> "]"
    encode (IndirectBr address labels) = 
        "indirectbr ptr" <+>
        encode address <+> 
        ", [" <+>
        intercalate "," (map (prefixed "label " . encode) labels) <+>
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
public export
Encode ConversionOpCode VString where
    encode (Trunc wrapping) = "trunc" <+> encode wrapping
    encode ZExt = "zext"
    encode SExt = "sext"
    encode (FPTrunc fastMath) = "fptrunc" --TODO:
    encode (FPExt fastMath) = "fpext" --TODO:
    encode FPToUi = "fptoui"
    encode FPToSi = "fptosi"
    encode UiToFP = "uitofp"
    encode SiToFP = "sitofp"
    --TODO: encode IntToPtr = "inttoptr"
    encode PtrToInt = "ptrtoint"
    encode BitCast = "bitcast"
    encode (AddrSpaceCast addressSpace) = "addrspacecast ptr" <+> encode addressSpace
public export
Encode VectorOpcode VString where
    -- TODO: Check this is right
    encode (ExtractElement ty index) = "extractelement " <+> encode ty <+> "," <+> encode index
    encode (InsertElement ty value index) = "insertelement " <+> encode ty <+> "," <+> encode value <+> "," <+> encode index
    encode (ShuffleVector vec1 vec2 mask) = 
        "shufflevector " <+> encode vec1 <+> "," <+> encode vec2 <+> "," <+> encode mask
export
Encode AggregateOpcode VString where
    encode (ExtractValue expr index) = "extractvalue " <+> encode expr <+> "," <+> vshow index
    encode (InsertValue expr value index) = "insertvalue " <+> encode expr <+> "," <+> encode value <+> "," <+> vshow index

export 
Encode MiscOpcode VString where
    -- TODO: Check this is right
    encode (Phi tpe pairs) = "phi " <+> encode tpe <+> "[" <+> intercalate "," (map (\(e, l) => encode e <+> ", label" <+> encode l) pairs) <+> "]"
    encode (Select fastMath cond ifTrue ifFalse) =
        "select " <+> encode @{nosep} fastMath <+> encode cond <+> "," <+> encode ifTrue <+> "," <+> encode ifFalse
    encode (Freeze expr) = "freeze " <+> encode expr
    encode (FnCallOp fnCall) = encode fnCall

emptyNode : VString 
emptyNode = "!{}"
nonTempNode : VString
nonTempNode = "!{ i32 1 }"
export
Encode AtomicOrder VString where
    encode Unordered = "unordered"
    encode Monotonic = "monotonic"
    encode Acquire = "acquire"
    encode Release = "release"
    encode AcquireRelease = "acq_rel"
    encode SequentiallyConsistent = "seq_cst"

export 
Encode MemoryOpcode VString where
    encode (Alloc tpe numElements alignment addressSpace) =
        "alloca" <+> encode tpe <+> 
        writeIf (\n => "," <+> encode @{ewt} n) numElements <+>
        writeIf (\n => ", align " <+> vshow n) alignment <+>
        writeIf (\space => ", addrspace(" <+> encode space <+> ")") addressSpace
    
    encode (LoadRegular volatile tpe address align nonTemporal invariantLoad invariantGroup nonNull dereferenceable dereferenceableOrNull aligned noUndef) =
        spaced [
            "load",
            if volatile then "volatile" else "",
            encode tpe <+> ",",
            encode tpe <+> "*",
            encode address,
            writeIf (\n => ", align " <+> vshow n) align,
            if nonTemporal then ", !nontemporal " <+> nonTempNode else "",
            if invariantLoad then ", !invariant.load " <+> emptyNode else "",
            if invariantGroup then ", !invariant.group " <+> emptyNode else "",
            if nonNull then ", !nonnull " <+> emptyNode else "",
            writeIf (\m => ", !dereferenceable " <+> encode m) dereferenceable,
            writeIf (\m => ", !dereferenceable_or_null " <+> encode m) dereferenceableOrNull,
            writeIf (\n => ", !align " <+> vshow n) aligned,
            if noUndef then ", !noundef " <+> emptyNode else ""
        ]
    
    encode (LoadAtomic volatile tpe address scope ordering align nontemporal invariantGroup) =
        spaced [
            "load atomic",
            if volatile then "volatile" else "",
            encode tpe <+> ",",
            encode tpe <+> "*",
            encode address,
            writeIf (\s => "syncscope(\"" <+> go s <+> "\")") scope,
            encodeIf ordering,
            writeIf (\n => ", align " <+> vshow n) align,
            if nontemporal then ", !nontemporal " <+> nonTempNode else "",
            if invariantGroup then ", !invariant.group " <+> emptyNode else ""
        ]
    
    encode (StoreRegular volatile tpe address align nonTemporal invariantGroup) =
        spaced [
            "store",
            if volatile then "volatile" else "",
            encode tpe <+> ",",
            encode tpe <+> "*",
            encode address,
            writeIf (\n => ", align " <+> vshow n) align,
            if nonTemporal then ", !nontemporal " <+> nonTempNode else "",
            if invariantGroup then ", !invariant.group " <+> emptyNode else ""
        ]
    
    encode (StoreAtomic volatile tpe address scope ordering align invariantGroup) =
        spaced [
            "store atomic",
            if volatile then "volatile" else "",
            encode tpe <+> ",",
            encode tpe <+> "*", 
            encode address,
            writeIf (\s => "syncscope(\"" <+> go s <+> "\")") scope,
            encodeIf ordering,
            writeIf (\n => ", align " <+> vshow n) align,
            if invariantGroup then ", !invariant.group " <+> emptyNode else ""
        ]
    
    encode (Fence scope ordering) =
        spaced [
            "fence",
            writeIf (\s => "syncscope(\"" <+> go s <+> "\")") scope,
            encode @{just} ordering
        ]
   
export 
Encode CatchClause VString where
    encode (Catching ty name) =
        "catch " <+> encode ty <+> encode name
    encode (Filtering ty matches) =
        "filter " <+> encode ty <+> encode matches 
export 
Encode ExceptOpcode VString where
    encode (LandingPad ty matches) = "landingpad" <+> encode ty <+> encode @{spacing} matches
    encode (LandingPadCleanup ty matches) = "landingpad" <+> encode ty <+> "cleanup" <+> encode @{spacing} matches
    encode (CatchPad ty matches) = "catchpad" <+> encode ty <+> encode matches
    encode (CleanupPad ty matches) = "cleanuppad" <+> encode ty <+> encode matches
export
Encode LOperation VString where
    encode (UnaryOp op ty expr) = 
        encode op <+> encode ty <+> encode expr
    encode (BinaryOp op ty expr1 expr2) =
        encode op <+> encode ty <+> encode expr1 <+> "," <+> encode expr2 
    encode (ConversionOp op expr ty) =
        encode op <+> encode expr <+> "to" <+> encode ty
    encode (TerminatorOp term) = encode term
    encode (VectorOp vOp) = encode vOp 
    encode (AggregateOp aOp) = encode aOp
    encode (MiscOp mOp) = encode mOp
    encode (MemoryOp mOp) = encode mOp
    encode (ExceptOp mOp) = encode mOp 

export
Encode LStatement VString where
    encode (Targeted reg expr) =
        encode reg <+> "=" <+> encode expr
    encode (Discarded expr) =
        encode expr
    encode (Labelled name) =
        encode name <++> ":"
    
public export
Encode FunctionBody VString where
    encode (MkFunctionBody statements) = "{\n" <++> encode @{tabbed} statements <++> "\n}" -- Placeholder for actual function body encoding
public export
Encode FunctionArgSpec VString where
    encode (MkFunctionArgSpec tpe attrs (Just name)) =
        encode tpe <+> encode @{nosep} attrs <+> "%" <++> encode name
    encode (MkFunctionArgSpec tpe attrs Nothing) =
        encode tpe <+> encode @{nosep} attrs

{-

define [linkage] [PreemptionSpecifier] [visibility] [DLLStorageClass]
       [cconv] [ret attrs]
       <ResultType> @<FunctionName> ([argument list])
       [(unnamed_addr|local_unnamed_addr)] [AddrSpace] [fn Attrs]
       [section "name"] [partition "name"] [comdat [($name)]] [align N]
       [gc] [prefix Constant] [prologue Constant] [personality Constant]
       (!name !N)* { ... }

 -}
public export
Encode FunctionDef VString where
    encode (MkFunctionDef name symbolInfo callingConvention returnAttrs returnType args addressInfo addressSpace fnAttributes section partition comdat alignment gc fprefix prologue personality metadata body tags) =
        let r : VString = "define" <+> 
            encode symbolInfo <+> 
            encodeIf callingConvention <+> 
            intercalate "," (map encode returnAttrs) <+> 
            encode returnType <+> 
            "@" <++>
            go name <+> 
            "(" <+> intercalate "," (map encode args) <+> ")" <+>
            encodeIf addressInfo <+>
            encodeIf addressSpace <+>
            (encodeIf $ show <$> alignment) <+>
            encodeIf section <+>
            encodeIf partition <+>
            encodeIf comdat <+>
            encodeIf gc <+>
            encodeIf fprefix <+>
            encodeIf prologue <+>
            encodeIf personality <+>
            encode @{each} metadata <+>
            intercalate "," (map (encode) tags) <+>
            encode body
             in r

{-
declare [linkage] [visibility] [DLLStorageClass]
        [cconv] [ret attrs]
        <ResultType> @<FunctionName> ([argument list])
        [(unnamed_addr|local_unnamed_addr)] [align N] [gc]
        [prefix Constant] [prologue Constant]
 -}

    
public export
Encode FunctionDec VString where 
    encode (MkFunctionDec name symbolInfo callingConvention returnAttrs returnType args addressInfo alignment gc fprefix prologue tags) =
        let r : VString = "declare" <+> 
            encode symbolInfo <+> 
            encodeIf callingConvention <+> 
            intercalate "," (map encode returnAttrs) <+> 
            encode returnType <+> 
            "@" <++>
            go name <+> 
            "(" <+> intercalate "," (map encode args) <+> ")" <+>
            encodeIf addressInfo <+>
            (encodeIf $ show <$> alignment) <+>
            encodeIf gc <+>
            encodeIf fprefix <+>
            encodeIf prologue <+>
            intercalate "," (map (encode) tags) in r

{-
@<Name> = [Linkage] [PreemptionSpecifier] [Visibility] [DLLStorageClass] [ThreadLocal] [(unnamed_addr|local_unnamed_addr)] alias <AliaseeTy>, <AliaseeTy>* @<Aliasee>
          [, partition "name"]
           -}

public export
Encode Alias VString where 
    encode (MkAlias name symbolInfo threadLocality addressInfo aliaseeTy aliasee tags) =
        let r : VString = "@" <++> go name <+> "=" <+> encode symbolInfo <+> 
            encodeIf threadLocality <+> 
            encodeIf addressInfo <+> 
            "alias" <+> 
            encode aliaseeTy <+> 
            "," <+> "@" <++> 
            encode aliasee in r

public export 
Encode IFunc VString where 

    -- TODO:
    encode (MkIFunc name symbolInfo threadLocality addressInfo addressSpace aliaseeTy aliasee tags) =
        let r : VString = "@" <++> go name <+> 
            encode symbolInfo <+> 
            encodeIf threadLocality <+> 
            encodeIf addressInfo <+> 
            encode addressSpace <+> 
            "ifunc" <+> 
            encode aliaseeTy <+> 
            encode aliasee <+>
            intercalate "," (map (encode) tags) in r

public export 
Encode AttributeGroupDef VString where 
    encode (MkAttributeGroupDef name attrs) =
        let r : VString = "attributes #" <++> encode name <+> "=" <+> "{" <+> intercalate "," (map encode attrs) <+> "}" in r
public export 
Encode LClause VString where 
    encode (GlobalDefC def) = encode def
    encode (FunctionDefC def) = encode def
    encode (FunctionDecC dec) = encode dec
    encode (AliasC alias) = encode alias
    encode (IFuncC ifunc) = encode ifunc
    encode (MetadataC name metadata) = "!" <++> go name <+> "=" <+> encode metadata
    encode (AttributeGroupC attrs) = encode attrs
    encode (OtherC other) = go other -- Placeholder for other clause types
public export
Encode LModule VString where
    encode (MkLModule dataLayout target text tags) =
        let 
            layout : VString = 
                case dataLayout of  
                    Just dl => "target datalayout = \"" <++> encode dl <++> "\"\n"
                    Nothing => ""
            target : VString = 
                case target of
                    Just t => "target triple = \"" <++> encode t <++> "\"\n"
                    Nothing => ""
            r : VString = 
            layout <++> target <++>
            encode @{lined} text
            in r

