||| LLVM IR serialization to textual representation.
|||
||| This module provides Encode ATM instances for all LLVM IR data types,
||| allowing them to be converted to their textual LLVM IR representation.
||| It handles proper formatting, escaping, and syntax generation for
||| a complete LLVM IR module.
module Data.LLVM.Write.Assembly

import Data.LLVM.IR
import Data.LLVM.Class
import Data.Walk
import public Data.LLVM.Write.Types
--%default covering
--%default partial --TODO: FIXME:
export 
Encode ATM Linkage VString where
    encode Private = pure "private"
    encode Internal = pure "internal"
    encode Available = pure "available"
    encode LinkOnce = pure "linkonce"
    encode Weak = pure "weak"
    encode Common = pure "common"
    encode Appending = pure "appending"
    encode ExternWeak = pure "extern_weak"
    encode LinkOnceODR = pure "linkonce_odr"
    encode WeakODR = pure "weak_odr"
    encode External = pure "external"
export
Encode ATM DLLStorage VString where
    encode DLLExport = pure "dllexport"
    encode DLLImport = pure "dllimport"
export
Encode ATM ThreadLocality VString where
    encode LocalDynamic = pure "localdynamic"
    encode InitialExec = pure "initialexec"
    encode LocalExec = pure "localexec"
export
Encode ATM Preemption VString where
    encode Preemptible = pure "dso_preemptible"
    encode NonPreemptible = pure "dso_local"
export
Encode ATM AddressSpace VString where 
    encode (NamedSpace name) = pure $ "addrspace(" <+> go name <+> ")"
    encode (UnnamedSpace n) = pure $ "addrspace(" <+> vshow n <+> ")" 



export
Encode ATM CallingConvention VString where 
    encode C = pure "ccc"
    encode Fast = pure "fastcc"
    encode Cold = pure "coldcc"
    encode GHC = pure "ghccc"
    encode CC11 = pure "cc11"
    encode AnyReg = pure "anyregcc"
    encode PreserveMost = pure "preserve_mostcc"
    encode PreserveAll = pure "preserve_allcc"
    encode PreserveNone = pure "preserve_nonecc"
    encode CxxFastTL = pure "cxx_fast_tlscc"
    encode Tail = pure "tailcc"
    encode Swift = pure "swiftcc"
    encode SwiftTail = pure "swift_tailcc"
    encode CFGuardCheck = pure "cfguard_checkcc"
    encode (CustomCC n) = pure $ "cc " <+> vshow n 
export
Encode ATM Visibility VString where 
    encode Default = pure "default"
    encode Hidden = pure "hidden"
    encode Protected = pure "protected"
export
Encode ATM Name VString where
    encode (Temporary name) = pure $ "%" <++> (go $ show name)
    encode (Unnamed name) = pure $ "%" <++> (go $ show name)
    encode (Local name) = pure $ "%" <++> go name
    encode (Parameter name) = pure $ "%" <++> go name
    encode (Global name) = pure $ "@" <++> go name

export
Encode ATM LTypeF VString where 
    encode Half = pure "half"
    encode Bfloat = pure "bfloat"
    encode LType.LFloat = pure "float"
    encode LDouble = pure "double"
    encode FP128 = pure "fp128"
    encode X86_FP80 = pure "x86_fp80"
    encode PPC_FP128 = pure "ppc_fp128"
export
Encode ATM SymbolInfo VString where  
    encode (MkSymbolInfo linkage preemption visibility dllStorage) = 
        encode linkage <+> encode preemption <+> encode visibility <+> encode dllStorage
export
Encode ATM LType VString where
    encode LType.LPtr = pure "ptr"
    encode (LNamed n) = pure $ "%" <++> go n
    encode (LPtrTo _) = pure "ptr"
    encode (LPtrToAddr space _) = do
        sp <- encode space
        pure $ "ptr " <+> sp
    encode (LPtrAddr space) = do
        sp <- encode space
        pure $ "ptr " <+> sp
    encode LVoid = pure "void"
    encode (LFun ret args) = do
        retStr <- encode ret
        argsStr <- encode @{each} args
        pure $ retStr <+> "(" <+> argsStr <+> ")"
    encode (LFunVarArg ret args varArg) = do
        retStr <- encode ret
        argsStr <- encode @{each} args
        varStr <- encode varArg
        pure $ retStr <+> "(" <+> argsStr <+> "," <+> varStr <+> " ... )"
    encode LOpaque = pure "opaque"
    encode (LType.LInt n) = pure $ "i" <++> vshow n
    encode (LFloating _) = pure "float"
    encode (LType.LVector s t) = do
        tStr <- encode t
        pure $ "<" <++> vshow s <+> " x " <+> tStr <+> ">"
    encode LLabel = pure "label"
    encode LType.LToken = pure "token"
    encode (LType.LArray n t) = do
        tStr <- encode t
        pure $ "[" <+> vshow n <+> " x " <+> tStr <+> "]"
    encode (LType.LStruct ts) = do
        tsStr <- encode @{each} ts
        pure $ "{" <+> tsStr <+> "}"
    encode (LPackedStruct ts) = do
        tsStr <- encode @{each} ts
        pure $ "<{" <+> tsStr <+> "}>"
    encode LType.LMetadata = pure "metadata"
    encode (LVectorScale s t) = do
        tStr <- encode t
        pure $ "< vscale x " <+> vshow s <+> " x " <+> tStr <+> ">"
    encode LX86_AMX = pure "x86_amx"
public export
[ewt] {a : Type} -> Encode ATM a VString => Encode ATM (WithType a) VString where 
    encode (MkWithType t v) = do
        tStr <- encode t
        vStr <- encode v
        pure $ tStr <+> vStr

public export
{a : Type} -> Encode ATM a VString => Encode ATM (WithType a) VString where 
    encode (MkWithType t v) = do
        tStr <- encode t
        vStr <- encode v
        pure $ tStr <+> vStr 
export
Encode ATM AddressInfo VString where
    encode UnnamedGlobal = pure "unnamed_addr"
    encode UnnamedLocal = pure "local_unnamed_addr"
export
Encode ATM LTag VString where
    encode (CTag s) = pure $ go s 

mutual 
    export
    Encode ATM Metadata VString where
        encode (MetadataNamed name) = pure $ "!" <++> go name
        encode (MetadataString s) = pure $ "!\"" <++> go s <++> "\""
        encode (MetadataValue v) = encode v
        encode (MetadataTuple vals) = do
            valsStr <- encode @{each} vals
            pure $ "!{" <+> valsStr <+> "}"
        encode (MetadataCustom s) = pure $ go s
        encode (MetadataNode n) = pure $ "!" <++> (go $ show n)
        encode (MetadataSpecial h t) = ?enmda

    export
    Encode ATM LValue VString where
        encode (LTerm.LInt n) = pure $ go $ show n
        encode (LTerm.LFloat s) = pure $ go s
        encode (LTerm.LBool b) = pure $ if b then "true" else "false"
        encode LTerm.LNull = pure "null"
        encode LTerm.LToken = pure "none"
        encode (LTerm.LString s) = pure $ "c\"" <+> go s <+> "\""
        encode (LTerm.LArray cs) = do
            cssStr <- encode @{each} cs
            pure $ "[" <+> cssStr <+> "]"
        encode (LTerm.LVector cs) = do
            cssStr <- encode @{each} cs
            pure $ "<" <+> cssStr <+> ">"
        encode (LTerm.LStruct cs) = do
            cssStr <- encode @{each} cs
            pure $ "{" <+> cssStr <+> "}"
        encode LTerm.LUndefined = pure "undef"
        encode LTerm.LPoison = pure "poison"
        encode LTerm.LZero = pure "zeroinitializer"
        encode (LTerm.LMetadata m) = do
            mStr <- encode m
            pure $ "metadata" <+> mStr
        encode (LTerm.LPtr name) = encode name
        encode (LTerm.LVar name) = encode name
export 
Encode ATM GVarDef VString where
    encode (MkGVarDef name symbolInfo threadLocality addressInfo addressSpace externallyInitialized global tpe init tags) = do
        symbolStr <- encode symbolInfo
        threadStr <- encodeIf threadLocality
        addrInfoStr <- encodeIf addressInfo 
        addrSpaceStr <- encodeIf addressSpace
        tpeStr <- encode tpe
        initStr <- encode @{just} init
        tagsStr <- encode @{each} tags
        let externStr = if externallyInitialized == Just True then "external" else ""
        let globalStr = if global then "global" else "constant"
        pure $ "@" <++> go name <+> "=" <+> spacedVString [
            symbolStr,
            threadStr,
            addrInfoStr,
            addrSpaceStr,
            externStr,
            globalStr,
            tpeStr,
            initStr,
            tagsStr
        ]






export
Encode ATM Attribute VString where 
    encode ZeroExt = pure "zeroext"
    encode SignExt = pure "signext"
    encode NoExt = pure "noext"
    encode (ByVal t) = do
        tStr <- encode t
        pure $ "byval(" <+> tStr <+> ")"
    encode (ByRef t) = do
        tStr <- encode t
        pure $ "byref(" <+> tStr <+> ")"
    encode (Preallocated t) = do
        tStr <- encode t
        pure $ "preallocated(" <+> tStr <+> ")"
    encode (Inalloca t) = do
        tStr <- encode t
        pure $ "inalloca(" <+> tStr <+> ")"
    encode (SRet t) = do
        tStr <- encode t
        pure $ "sret(" <+> tStr <+> ")"
    encode (Align n) = pure $ "align" <+> vshow n
    encode NoAlias = pure "noalias"
    encode NoFree = pure "nofree"
    encode Nest = pure "nest"
    encode Returned = pure "returned"
    encode NoNull = pure "nonnull"
    encode (Dereferenceable n) = pure $ "dereferenceable(" <+> vshow n <+> ")"
    encode (DereferenceableOrNull n) = pure $ "dereferenceable_or_null(" <+> vshow n <+> ")"
    encode SwiftSelf = pure "swiftself"
    encode SwiftAsync = pure "swiftasync"
    encode SwiftError = pure "swifterror"
    encode ImmArg = pure "immarg"
    encode NoUndef = pure "nounwind"
    encode (AlignStack n) = pure $ "alignstack(" <+> vshow n <+> ")"
    encode AllocAlign = pure "allocalign"
    encode AllocPtr = pure "allocptr"
    encode ReadNone = pure "readnone"
    encode ReadOnly = pure "readonly"
    encode WriteOnly = pure "writeonly"
    encode Writeable = pure "writeable"
    encode DeadOnUnwind = pure "dead_on_unwind"
    encode DeadOnReturn = pure "dead_on_return"
    encode (OtherAttribute name) = pure $ go name 



export
Encode ATM CaseBranch VString where
    encode (MkCaseBranch tpe value label) = do
        tpeStr <- encode tpe
        valueStr <- encode value  
        labelStr <- encode label
        pure $ tpeStr <+> " " <++> valueStr <+> ", label " <+> labelStr

export
Encode ATM InvokeCall VString where
    encode (MkInvokeCall cc returnAttrs addressSpace tpe fnval args normal unwind) = do
        ccStr <- encodeIf cc
        attrsStr <- encode @{each} returnAttrs
        addrStr <- encodeIf addressSpace
        tpeStr <- encode tpe
        fnvalStr <- encode fnval
        argsStr <- encode @{each} args
        normalStr <- encode normal
        unwindStr <- encode unwind
        pure $ spacedVString [
            "invoke",
            ccStr,
            attrsStr,
            addrStr,
            tpeStr,
            fnvalStr,
            "(",
            argsStr,
            ")",
            "to label",
            normalStr,
            "unwind label",
            unwindStr
        ]
export
Encode ATM Wrapping VString where
    encode NoSigned = pure "nsw"
    encode NoUnsigned = pure "nuw"
    encode NoSignedUnsigned = pure "nuw nsw"
export
Encode ATM UnaryOpcode VString where
    encode FNeg = pure "fneg"

export
Encode ATM BinaryOpcode VString where
    encode Add = pure "add"
    encode (AddWrap wrapping) = do
        wrapStr <- encode wrapping
        pure $ "add " <+> wrapStr
    encode (FAdd fastMath) = pure "fadd" -- TODO: <+> encode fastMath
    encode Sub = pure "sub"
    encode (SubWrap wrapping) = do
        wrapStr <- encode wrapping
        pure $ "sub " <+> wrapStr
    encode (FSub fastMath) = pure "fsub" -- TODO: <+> encode fastMath
    encode Mul = pure "mul"
    encode (MulWrap wrapping) = do
        wrapStr <- encode wrapping
        pure $ "mul " <+> wrapStr
    encode (FMul fastMath) = pure "fmul" -- TODO: <+> encode fastMath
    encode UDiv = pure "udiv"
    encode (FDiv fastMath) = pure "fdiv" -- TODO: <+> encode fastMath
    encode UDivExact = pure "udiv exact"
    encode (SDiv) = pure "sdiv" -- TODO: <+> encode fastMath
    encode (SDivExact) = pure "sdiv exact"
    encode SRem = pure "srem"
    encode URem = pure "urem"
    encode (FRem fastMath) = pure "frem" -- TODO: <+> encode fastMath
    encode Shl = pure "shl"
    encode (ShlWrap wrapping) = do
        wrapStr <- encode wrapping
        pure $ "shl " <+> wrapStr
    encode LShr = pure "lshr"
    encode LShrExact = pure "lshr exact"
    encode AShr = pure "ashr"
    encode AShrExact = pure "ashr exact"
    encode And = pure "and"
    encode Or = pure "or"
    encode Xor = pure "xor"
    encode DisjointOr = pure "or disjoint"
public export
Encode ATM BrCall VString where 
    encode (MkBrCall cc returnAttrs addressSpace tpe fnval args fallthrough indirect) = do
        ccStr <- encodeIf cc
        attrsStr <- encode @{nosep} returnAttrs
        addrStr <- encodeIf addressSpace
        tpeStr <- encode tpe
        fnvalStr <- encode fnval
        argsStr <- encode @{each} args
        fallthroughStr <- encode fallthrough
        indirectStr <- encode @{each} indirect
        pure $ spacedVString [
            "callbr",
            ccStr,
            attrsStr,
            addrStr,
            tpeStr,
            fnvalStr,
            "(",
            argsStr,
            ")",
            "to label",
            fallthroughStr,
            indirectStr
        ]
public export
Encode ATM CatchSwitch VString where
    encode (MkCatchSwitch name parent handler (Just caller)) = do
        nameStr <- encode name
        parentStr <- encode parent
        handlerStr <- encode @{each} handler
        callerStr <- encode caller
        pure $ spacedVString [
            "catchswitch within",
            nameStr,
            parentStr,
            "[" <+> handlerStr <+> "]",
            "unwind label",
            callerStr
        ]
    encode (MkCatchSwitch name parent handler Nothing) = do
        nameStr <- encode name
        parentStr <- encode parent
        handlerStr <- encode @{each} handler
        pure $ spacedVString [
            "catchswitch within",
            nameStr,
            parentStr,
            "[" <+> handlerStr <+> "]",
            "unwind to default"
        ]


public export
Encode ATM FastMathFlag VString where 
    encode FFast = pure "fast"
    encode NoNaNs = pure "nnan"
    encode NoInfs = pure "ninf"
    encode NoSignedZeros = pure "nsz"
public export
Encode ATM TailCall VString where 
    encode Tail = pure "tail"
    encode MustTail = pure "musttail"
    encode NoTail = pure "notail"
public export
Encode ATM FnCall VString where
    encode (MkFnCall tail fastMath cc returnAttrs addressSpace tpe fnval args attrs) = do
        tailStr <- encode {a=TailCall} {b=VString} tail
        fastMathStr <- encode @{nosep} fastMath
        ccStr <- encodeIf cc
        returnAttrsStr <- encode @{nosep} returnAttrs
        addrSpaceStr <- encodeIf addressSpace
        tpeStr <- encode tpe
        fnvalStr <- encode fnval
        argsStr <- encode @{each} args
        attrsStr <- encode @{nosep} attrs
        pure $ spacedVString [
            tailStr,
            "call",
            fastMathStr,
            ccStr,
            returnAttrsStr,
            addrSpaceStr,
            tpeStr,
            fnvalStr,
            "(",
            argsStr,
            ")",
            attrsStr
        ]




public export
Encode ATM Terminator VString where
    encode RetVoid = pure "ret void"
    encode (Ret ty expr) = do
        tyStr <- encode ty
        exprStr <- encode expr
        pure $ "ret" <+> tyStr <+> exprStr
    encode (CondBr cond true false) = do
        condStr <- encode cond
        trueStr <- encode true
        falseStr <- encode false
        pure $ "br i1" <+> condStr <+> ", label" <+> trueStr <+> "," <+> falseStr
    encode (JumpBr label) = do
        labelStr <- encode label
        pure $ "br label" <+> labelStr
    encode (Switch ty expr defaultBranch cases) = do
        tyStr <- encode ty
        exprStr <- encode expr
        defaultStr <- encode defaultBranch
        casesStr <- encode @{lined} cases
        pure $ "switch" <+> tyStr <+> exprStr <+> ", label" <+> defaultStr <+> "[" <+> casesStr <+> "]"
    encode (IndirectBr address labels) = do
        addressStr <- encode address
        labelsStr <- encode @{each} (map (\l => "label " <++> runIdentity (encode l)) labels)
        pure $ "indirectbr ptr " <+> addressStr <+> ", [" <+> labelsStr <+> "]"
    encode (Invoke invokeCall) = encode invokeCall
    encode (CallBR callBr) = encode callBr
    encode (Resume ty val) = do
        tyStr <- encode ty
        valStr <- encode val
        pure $ "resume" <+> tyStr <+> valStr
    encode Unreachable = pure "unreachable"
    encode (CatchSwitchOp catchSwitch) = encode catchSwitch
    encode (CatchRet expr label) = do
        exprStr <- encode expr
        labelStr <- encode label
        pure $ "catchret from" <+> exprStr <+> "to label" <+> labelStr
    encode (CleanupRetCaller expr) = do
        exprStr <- encode expr
        pure $ "cleanupret from" <+> exprStr <+> "unwind to caller"
    encode (CleanupRet expr label) = do
        exprStr <- encode expr
        labelStr <- encode label
        pure $ "cleanupret from" <+> exprStr <+> "unwind label" <+> labelStr
public export
Encode ATM ConversionOpCode VString where
    encode (Trunc wrapping) = do
        wrapStr <- encode wrapping
        pure $ "trunc" <+> wrapStr
    encode ZExt = pure "zext"
    encode SExt = pure "sext"
    encode (FPTrunc fastMath) = pure "fptrunc" --TODO: add fastMath
    encode (FPExt fastMath) = pure "fpext" --TODO: add fastMath
    encode FPToUi = pure "fptoui"
    encode FPToSi = pure "fptosi"
    encode UiToFP = pure "uitofp"
    encode SiToFP = pure "sitofp"
    --TODO: encode IntToPtr = "inttoptr"
    encode PtrToInt = pure "ptrtoint"
    encode BitCast = pure "bitcast"
    encode (AddrSpaceCast addressSpace) = do
        addrStr <- encode addressSpace
        pure $ "addrspacecast ptr" <+> addrStr 
public export
Encode ATM VectorOpcode VString where
    encode (ExtractElement ty index) = do
        tyStr <- encode ty
        indexStr <- encode index
        pure $ "extractelement " <+> tyStr <+> "," <+> indexStr
    encode (InsertElement ty value index) = do
        tyStr <- encode ty
        valueStr <- encode value
        indexStr <- encode index
        pure $ "insertelement " <+> tyStr <+> "," <+> valueStr <+> "," <+> indexStr
    encode (ShuffleVector vec1 vec2 mask) = do
        vec1Str <- encode vec1
        vec2Str <- encode vec2
        maskStr <- encode mask
        pure $ "shufflevector " <+> vec1Str <+> "," <+> vec2Str <+> "," <+> maskStr
export
Encode ATM AggregateOpcode VString where
    encode (ExtractValue expr index) = do
        exprStr <- encode expr
        pure $ "extractvalue " <+> exprStr <+> "," <+> vshow index
    encode (InsertValue expr value index) = do
        exprStr <- encode expr
        valueStr <- encode value
        pure $ "insertvalue " <+> exprStr <+> "," <+> valueStr <+> "," <+> vshow index 

export 
Encode ATM MiscOpcode VString where
    -- TODO: Check this is right
    encode (Phi tpe pairs) = do
        tpeStr <- encode tpe
        pairsStr <- pure $ intercalate "," (map (\(e, l) => runIdentity (encode e) <+> ", label" <+> runIdentity (encode l)) pairs)
        pure $ "phi " <+> tpeStr <+> "[" <+> pairsStr <+> "]"
    encode (Select fastMath cond ifTrue ifFalse) = do
        fastMathStr <- encode @{nosep} fastMath
        condStr <- encode cond
        ifTrueStr <- encode ifTrue
        ifFalseStr <- encode ifFalse
        pure $ "select " <+> fastMathStr <+> condStr <+> "," <+> ifTrueStr <+> "," <+> ifFalseStr
    encode (Freeze expr) = do
        exprStr <- encode expr
        pure $ "freeze " <+> exprStr 
    encode (FnCallOp fnCall) = encode fnCall 

emptyNode : VString 
emptyNode = "!{}"
nonTempNode : VString
nonTempNode = "!{ i32 1 }"
export
Encode ATM AtomicOrder VString where
    encode Unordered = pure "unordered"
    encode Monotonic = pure "monotonic"
    encode Acquire = pure "acquire"
    encode Release = pure "release"
    encode AcquireRelease = pure "acq_rel"
    encode SequentiallyConsistent = pure "seq_cst"

export 
Encode ATM MemoryOpcode VString where
    encode (Alloc tpe numElements alignment addressSpace) = do
        let tpeStr = runIdentity (encode tpe)
        let numElementsStr = writeIf (\n => go "," <++> runIdentity (encode @{ewt} n)) numElements
        let alignmentStr = writeIf (\n => ", align " <+> vshow n) alignment
        let addressSpaceStr = writeIf (\space => go ", addrspace(" <++> runIdentity (encode space) <++> go ")") addressSpace
        pure $ "alloca" <+> tpeStr <+> numElementsStr <+> alignmentStr <+> addressSpaceStr
    
    encode (LoadRegular volatile tpe address align nonTemporal invariantLoad invariantGroup nonNull dereferenceable dereferenceableOrNull aligned noUndef) = do
        let tpeStr = runIdentity (encode tpe)
        addressStr <- encode address
        let alignStr = writeIf (\n => ", align " <+> vshow n) align
        let derefStr = writeIf (\m => ", !dereferenceable " <+> runIdentity (encode m)) dereferenceable
        let derefOrNullStr = writeIf (\m => ", !dereferenceable_or_null " <+> runIdentity (encode m)) dereferenceableOrNull
        let alignedStr = writeIf (\n => ", !align " <+> vshow n) aligned
        pure $ spacedVString [
            "load",
            if volatile then "volatile" else "",
            tpeStr <++> ",",
            tpeStr <++> "*",
            addressStr,
            alignStr,
            if nonTemporal then ", !nontemporal " <+> nonTempNode else "",
            if invariantLoad then ", !invariant.load " <+> emptyNode else "",
            if invariantGroup then ", !invariant.group " <+> emptyNode else "",
            if nonNull then ", !nonnull " <+> emptyNode else "",
            derefStr,
            derefOrNullStr,
            alignedStr,
            if noUndef then ", !noundef " <+> emptyNode else ""
        ]
    
    encode (LoadAtomic volatile tpe address scope ordering align nontemporal invariantGroup) = do
        let tpeStr = runIdentity (encode tpe)
        addressStr <- encode address
        let scopeStr = writeIf (\s => "syncscope(\"" <+> go s <+> "\")") scope
        orderingStr <- encodeIf ordering
        let alignStr = writeIf (\n => ", align " <+> vshow n) align
        pure $ spacedVString [
            "load atomic",
            if volatile then "volatile" else "",
            tpeStr <++> ",",
            tpeStr <++> "*",
            addressStr,
            scopeStr,
            orderingStr,
            alignStr,
            if nontemporal then ", !nontemporal " <+> nonTempNode else "",
            if invariantGroup then ", !invariant.group " <+> emptyNode else ""
        ]
    
    encode (StoreRegular volatile tpe address align nonTemporal invariantGroup) = do
        let tpeStr = runIdentity (encode tpe)
        addressStr <- encode address
        let alignStr = writeIf (\n => ", align " <+> vshow n) align
        pure $ spacedVString [
            "store",
            if volatile then "volatile" else "",
            tpeStr <++> ",",
            tpeStr <++> "*",
            addressStr,
            alignStr,
            if nonTemporal then ", !nontemporal " <+> nonTempNode else "",
            if invariantGroup then ", !invariant.group " <+> emptyNode else ""
        ]
    
    encode (StoreAtomic volatile tpe address scope ordering align invariantGroup) = do
        let tpeStr = runIdentity (encode tpe)
        addressStr <- encode address
        let scopeStr = writeIf (\s => "syncscope(\"" <+> go s <+> "\")") scope
        orderingStr <- encodeIf ordering
        let alignStr = writeIf (\n => ", align " <+> vshow n) align
        pure $ spacedVString [
            "store atomic",
            if volatile then "volatile" else "",
            tpeStr <++> ",",
            tpeStr <++> "*", 
            addressStr,
            scopeStr,
            orderingStr,
            alignStr,
            if invariantGroup then ", !invariant.group " <+> emptyNode else ""
        ]
    
    encode (Fence scope ordering) = do
        let scopeStr = writeIf (\s => ("syncscope(\"" <+> go s <+> "\")")) scope
        orderingStr <- encode @{just} ordering
        pure $ spacedVString [
            "fence",
            scopeStr,
            orderingStr
        ]
   

   
export 
Encode ATM CatchClause VString where
    encode (Catching ty name) = do
        tyStr <- encode ty
        nameStr <- encode name
        pure $ "catch " <+> tyStr <+> nameStr
    encode (Filtering ty matches) = do
        tyStr <- encode ty
        matchesStr <- encode matches
        pure $ "filter " <+> tyStr <+> matchesStr
export 
Encode ATM ExceptOpcode VString where
    encode (LandingPad ty matches) = do
        tyStr <- encode ty
        matchesStr <- encode @{spacing} matches
        pure $ "landingpad" <+> tyStr <+> matchesStr
    encode (LandingPadCleanup ty matches) = do
        tyStr <- encode ty
        matchesStr <- encode @{spacing} matches
        pure $ "landingpad" <+> tyStr <+> "cleanup" <+> matchesStr
    encode (CatchPad ty matches) = do
        tyStr <- encode ty
        matchesStr <- encode matches
        pure $ "catchpad" <+> tyStr <+> matchesStr
    encode (CleanupPad ty matches) = do
        tyStr <- encode ty
        matchesStr <- encode matches
        pure $ "cleanuppad" <+> tyStr <+> matchesStr 

export 
Encode ATM Comparison VString where 
    encode CEq = pure "eq"
    encode CNe = pure "ne"
    encode CULt = pure "ult"
    encode CUGt = pure "ugt"
    encode CULe = pure "ule"
    encode CUGe = pure "uge"
    encode CSLt = pure "slt"
    encode CSGt = pure "sgt"
    encode CSLe = pure "sle"
    encode CSGe = pure "sge"
export 
Encode ATM CompareOpcode VString where
    encode (ICmp wrap) = do
        wrapStr <- encode wrap
        pure $ "icmp" <+> wrapStr

    encode _ = ?covs

export
Encode ATM LInstruction VString where
    encode (UnaryOp op ty expr) = do
        opStr <- encode op
        tyStr <- encode ty
        exprStr <- encode expr
        pure $ opStr <+> tyStr <+> exprStr
    encode (CompareOp op ty expr1 expr2) = do
        opStr <- encode op
        tyStr <- encode ty
        expr1Str <- encode expr1
        expr2Str <- encode expr2
        pure $ opStr <+> tyStr <+> expr1Str <+> "," <+> expr2Str
    encode (BinaryOp op ty expr1 expr2) = do
        opStr <- encode op
        tyStr <- encode ty
        expr1Str <- encode expr1
        expr2Str <- encode expr2
        pure $ opStr <+> tyStr <+> expr1Str <+> "," <+> expr2Str
    encode (ConversionOp op expr ty) = do
        opStr <- encode op
        exprStr <- encode expr
        tyStr <- encode ty
        pure $ opStr <+> exprStr <+> "to" <+> tyStr
    encode (TerminatorOp term) = encode term 
    encode (VectorOp vOp) = encode vOp
    encode (AggregateOp aOp) = encode aOp
    encode (MiscOp mOp) = encode mOp
    encode (MemoryOp mOp) = encode mOp
    encode (ExceptOp mOp) = encode mOp 


export
Encode ATM LStatement VString where
    -- TODO: Metadata
    encode (MkLStatement Nothing expr _) = encode expr
    encode (MkLStatement (Just reg) expr _) = do
        regStr <- encode reg
        exprStr <- encode expr
        pure $ regStr <+> "=" <+> exprStr
        
    
public export
Encode ATM BasicBlock VString where
    encode (MkBasicBlock name statements term) = do
        nameStr <- (<++> ":") <$> encode name
        statementsStr <- encode @{tabbed} statements
        termStr : VString <- encode term
        pure $ nameStr <+> statementsStr <+> termStr
public export
Encode ATM Argument VString where
    encode (MkArgument tpe attrs (Just name)) = do
        tpeStr <- encode tpe
        attrsStr <- encode @{nosep} attrs
        nameStr <- encode name
        pure $ tpeStr <+> attrsStr <+> "%" <++> nameStr
    encode (MkArgument tpe attrs Nothing) = do
        tpeStr <- encode tpe
        attrsStr <- encode @{nosep} attrs
        pure $ tpeStr <+> attrsStr

export 
Encode ATM Annotation VString where 
    encode (MkAnnotation metadata) = encode @{nosep} metadata
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
Encode ATM TypeDef VString where 
    encode (MkTypeDef n ty tags) = do
        nameStr <- encode n
        tyStr <- encode ty
        tagsStr <- encode tags
        pure $ "%" <++> nameStr <+> "=" <+> "type" <+> tyStr <+> tagsStr


public export
Encode ATM FunctionDef VString where
    encode (MkFunctionDef name symbolInfo callingConvention returnAttrs returnType args addressInfo addressSpace fnAttributes section partition comdat alignment gc fprefix prologue personality metadata body tags) = do
        symbolInfoStr <- encode symbolInfo
        callingConventionStr <- encodeIf callingConvention
        returnAttrsStr <- pure $ intercalate "," (map (runIdentity . encode) returnAttrs)
        returnTypeStr <- encode returnType
        argsStr <- pure $ intercalate "," (map (runIdentity . encode) args)
        addressInfoStr <- encodeIf addressInfo
        addressSpaceStr <- encodeIf addressSpace
        alignmentStr <- encodeIf $ show <$> alignment
        sectionStr <- encodeIf section
        partitionStr <- encodeIf partition
        comdatStr <- encodeIf comdat
        gcStr <- encodeIf gc
        fprefixStr <- encodeIf fprefix
        prologueStr <- encodeIf prologue
        personalityStr <- encodeIf personality
        metadataStr <- encode @{each} metadata
        tagsStr <- pure $ intercalate "," (map (runIdentity . encode) tags)
        bodyStr <- encode @{tabbed} body
        pure $ "define" <+> 
            symbolInfoStr <+> 
            callingConventionStr <+> 
            returnAttrsStr <+> 
            returnTypeStr <+> 
            "@" <++>
            go name <+> 
            "(" <+> argsStr <+> ")" <+>
            addressInfoStr <+>
            addressSpaceStr <+>
            alignmentStr <+>
            sectionStr <+>
            partitionStr <+>
            comdatStr <+>
            gcStr <+>
            fprefixStr <+>
            prologueStr <+>
            personalityStr <+>
            metadataStr <+>
            tagsStr <+>
            "{" <+>
            bodyStr <+>
            "}"

{-
declare [linkage] [visibility] [DLLStorageClass]
        [cconv] [ret attrs]
        <ResultType> @<FunctionName> ([argument list])
        [(unnamed_addr|local_unnamed_addr)] [align N] [gc]
        [prefix Constant] [prologue Constant]
 -}

    
public export
Encode ATM FunctionDec VString where 
    encode (MkFunctionDec name symbolInfo callingConvention returnAttrs returnType args addressInfo alignment gc fprefix prologue tags) = do
        symbolInfoStr <- encode symbolInfo
        callingConventionStr <- encodeIf callingConvention
        returnAttrsStr <- pure $ intercalate "," (map (runIdentity . encode) returnAttrs)
        returnTypeStr <- encode returnType
        argsStr <- pure $ intercalate "," (map (runIdentity . encode) args)
        addressInfoStr <- encodeIf addressInfo
        alignmentStr <- encodeIf $ show <$> alignment
        gcStr <- encodeIf gc
        fprefixStr <- encodeIf fprefix
        prologueStr <- encodeIf prologue
        tagsStr <- pure $ intercalate "," (map (runIdentity . encode) tags)
        pure $ "declare" <+> 
            symbolInfoStr <+> 
            callingConventionStr <+> 
            returnAttrsStr <+> 
            returnTypeStr <+> 
            "@" <++>
            go name <+> 
            "(" <+> argsStr <+> ")" <+>
            addressInfoStr <+>
            alignmentStr <+>
            gcStr <+>
            fprefixStr <+>
            prologueStr <+>
            tagsStr

{-
@<Name> = [Linkage] [PreemptionSpecifier] [Visibility] [DLLStorageClass] [ThreadLocal] [(unnamed_addr|local_unnamed_addr)] alias <AliaseeTy>, <AliaseeTy>* @<Aliasee>
          [, partition "name"]
           -}

public export
Encode ATM Alias VString where 
    encode (MkAlias name symbolInfo threadLocality addressInfo aliaseeTy aliasee tags) = do
        symbolInfoStr <- encode symbolInfo
        threadLocalityStr <- encodeIf threadLocality
        addressInfoStr <- encodeIf addressInfo
        aliaseeTyStr <- encode aliaseeTy
        aliaseeStr <- encode aliasee
        pure $ "@" <++> go name <+> "=" <+> symbolInfoStr <+> 
            threadLocalityStr <+> 
            addressInfoStr <+> 
            "alias" <+> 
            aliaseeTyStr <+> 
            "," <+> "@" <++> 
            aliaseeStr

public export 
Encode ATM IFunc VString where 
    -- TODO:
    encode (MkIFunc name symbolInfo threadLocality addressInfo addressSpace aliaseeTy aliasee tags) = do
        symbolInfoStr <- encode symbolInfo
        threadLocalityStr <- encodeIf threadLocality
        addressInfoStr <- encodeIf addressInfo
        addressSpaceStr <- encode addressSpace
        aliaseeTyStr <- encode aliaseeTy
        aliaseeStr <- encode aliasee
        tagsStr <- pure $ intercalate "," (map (runIdentity . encode) tags)
        pure $ "@" <++> go name <+> 
            symbolInfoStr <+> 
            threadLocalityStr <+> 
            addressInfoStr <+> 
            addressSpaceStr <+> 
            "ifunc" <+> 
            aliaseeTyStr <+> 
            aliaseeStr <+>
            tagsStr

public export 
Encode ATM AttributeGroupDef VString where 
    encode (MkAttributeGroupDef name attrs) = do
        nameStr <- encode name
        attrsStr <- pure $ intercalate "," (map (runIdentity . encode) attrs)
        pure $ "attributes #" <++> nameStr <+> "=" <+> "{" <+> attrsStr <+> "}"
public export 
Encode ATM LClause VString where 
    encode (GlobalDefC def) = encode def
    encode (FunctionDefC def) = encode def
    encode (FunctionDecC dec) = encode dec
    encode (AliasC alias) = encode alias
    encode (IFuncC ifunc) = encode ifunc
    encode (MetadataC name metadata) = do
        metadataStr <- encode metadata
        pure $ "!" <++> go name <+> "=" <+> metadataStr
    encode (AttributeGroupC attrs) = encode attrs
    encode (TypeDefC def) = encode def
    encode (OtherC other) = pure $ go other -- Placeholder for other clause types ) 
public export
Encode ATM LModule VString where
    encode (MkLModule dataLayout target text tags) = do
        let layout : VString = 
                case dataLayout of  
                    Just dl => "target datalayout = \"" <++> runIdentity (encode dl) <++> "\"\n"
                    Nothing => ""
        let target : VString = 
                case target of
                    Just t => "target triple = \"" <++> runIdentity (encode t) <++> "\"\n"
                    Nothing => ""
        textStr <- encode @{lined} text
        pure $ layout <++> target <++> textStr


