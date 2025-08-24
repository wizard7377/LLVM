||| LLVM IR serialization to textual representation.
|||
||| This module provides Encode ATM instances for all LLVM IR data types,
||| allowing them to be converted to their textual LLVM IR representation.
||| It handles proper formatting, escaping, and syntax generation for
||| a complete LLVM IR module.
module Data.LLVM.Write.Text.Encode

import Data.LLVM.IR
import Data.LLVM.Class
import Data.Walk
import Data.Table
import public Data.LLVM.Write.Types
import public Data.LLVM.Write.Text.Types
--%default covering
--%default partial --TODO: FIXME:

wrapName : String -> VString
wrapName name = MkVString $ unescape name
Encode ATM Label VString where
    encode (NamedLabel name) = pure $ "%" <++> go name
    encode (LiftedLabel block) = ?ebl
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
    encode TailCC = pure "tailcc"
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
Encode ATM LFloatFormat VString where 
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
    encode (LTarget head args) = ?enlttar
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
    Encode ATM Annotation VString where
      encode (MkAnnotation tags) = intercalate "," <$> (traverse tagEncode tags)
        where 
          tagEncode : (String, Metadata) -> ATM VString
          tagEncode (s, m) = (pure $ "!" <++> go s) <+> encode m
    export
    {t : Bool} -> Encode ATM (LValue t) VString where
        encode (Core.LInt n) = pure $ go $ show n
        encode (Core.LFloat s) = pure $ go s
        encode (Core.LBool b) = pure $ if b then "true" else "false"
        encode Core.LNull = pure "null"
        encode Core.LToken = pure "none"
        encode (Core.LString s) = pure $ "c\"" <+> go s <+> "\""
        encode (Core.LArray cs) = do
            cssStr <- encode @{each} cs
            pure $ "[" <+> cssStr <+> "]"
        encode (Core.LVector cs) = do
            cssStr <- encode @{each} cs
            pure $ "<" <+> cssStr <+> ">"
        encode (Core.LStruct cs) = do
            cssStr <- encode @{each} cs
            pure $ "{" <+> cssStr <+> "}"
        encode Core.LUndefined = pure "undef"
        encode Core.LPoison = pure "poison"
        encode Core.LZero = pure "zeroinitializer"
        encode (Core.LMetadata m) = do
            mStr <- encode m
            pure $ "metadata" <+> mStr
        encode (Core.LPtr name) = pure "ptr" <+> encode name
        encode (Core.LVar name) = encode name
        encode _ = ?ltermres
export 
Encode ATM GVarDef VString where
    encode (MkGVarDef name symbolInfo threadLocality addressInfo addressSpace externallyInitialized global tpe init tags) = do
        symbolStr <- encode symbolInfo
        threadStr <- encodeIf threadLocality
        addrInfoStr <- encodeIf addressInfo 
        addrSpaceStr <- encodeIf addressSpace
        tpeStr <- encode tpe
        initStr <- encode @{just} init
        tagsStr <- encode tags
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
        pure $ "br i1" <+> condStr <+> ", label" <+> trueStr <+> "," <+> "label" <+> falseStr
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
        labelsStr <- encode @{each} (map (\l => "label " <++> runATM (encode l)) labels)
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
public export
Encode ATM LExpr VString where
    encode (Phi tpe pairs) = do
        tpeStr <- encode tpe
        pairsStr <- pure $ intercalate "," (map (\(e, l) => runATM (encode e) <+> ", label" <+> runATM (encode l)) pairs)
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
    encode (LandingPad tpe clauses) = do
        tpeStr <- encode tpe
        clausesStr <- encode @{spacing} clauses
        pure $ "landingpad " <+> tpeStr <+> clausesStr
    encode (LandingPadCleanup tpe clauses) = do
        tpeStr <- encode tpe
        clausesStr <- encode @{spacing} clauses
        pure $ "landingpad " <+> tpeStr <+> " cleanup" <+> clausesStr
    encode (CatchPad name val) = do
        nameStr <- encode name
        valStr <- encode val
        pure $ "catchpad " <+> nameStr <+> ", " <+> valStr
    encode (CleanupPad name val) = do
        nameStr <- encode name
        valStr <- encode val
        pure $ "cleanuppad " <+> nameStr <+> ", " <+> valStr
    encode (Alloc tpe numElems align addrSpace) = do
        tpeStr <- encode tpe
        numElemsStr <- maybe (pure "") (encode @{ewt}) numElems
        alignStr <- maybe (pure "") (\n => pure $ ", align " <+> vshow n) align
        addrStr <- maybe (pure "") (\a => pure $ ", addrspace(" <++> runATM (encode a) <++> ")") addrSpace
        pure $ "alloca " <+> tpeStr <+> numElemsStr <+> alignStr <+> addrStr
    encode (LoadRegular v tpe addr align nontemp invLoad invGroup nonNull deref derefOrNull aligned noUndef) = do
        tpeStr <- encode tpe
        addrStr <- encode addr
        alignStr <- maybe (pure "") (\n => pure $ ", align " <+> vshow n) align
        pure $ "load " <+> (if v then "volatile " else "") <+> tpeStr <+> ", " <+> addrStr <+> alignStr
    encode (LoadAtomic v tpe addr scope ordering align nontemp invGroup) = do
        tpeStr <- encode tpe
        addrStr <- encode addr
        pure $ "load atomic " <+> (if v then "volatile " else "") <+> tpeStr <+> ", " <+> addrStr
    encode (StoreRegular v tpe addr align nontemp invGroup) = do
        tpeStr <- encode tpe
        addrStr <- encode addr
        alignStr <- maybe (pure "") (\n => pure $ ", align " <+> vshow n) align
        pure $ "store " <+> (if v then "volatile " else "") <+> tpeStr <+> ", " <+> addrStr <+> alignStr
    encode (StoreAtomic v tpe addr scope ordering align invGroup) = do
        tpeStr <- encode tpe
        addrStr <- encode addr
        pure $ "store atomic " <+> (if v then "volatile " else "") <+> tpeStr <+> ", " <+> addrStr
    encode (Fence scope ordering) = pure "fence"
    encode (FNeg tpe val) = do
        tpeStr <- encode tpe
        valStr <- encode val
        pure $ "fneg " <+> tpeStr <+> valStr
    encode (Add tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "add " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (AddWrap wrap tpe a b) = do
        wrapStr <- encode wrap
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "add " <+> wrapStr <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (FAdd fm tpe a b) = do
        fmStr <- encode @{nosep} fm
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "fadd " <+> fmStr <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (Sub tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "sub " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (SubWrap wrap tpe a b) = do
        wrapStr <- encode wrap
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "sub " <+> wrapStr <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (FSub fm tpe a b) = do
        fmStr <- encode @{nosep} fm
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "fsub " <+> fmStr <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (Mul tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "mul " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (MulWrap wrap tpe a b) = do
        wrapStr <- encode wrap
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "mul " <+> wrapStr <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (FMul fm tpe a b) = do
        fmStr <- encode @{nosep} fm
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "fmul " <+> fmStr <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (UDiv tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "udiv " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (UDivExact tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "udiv exact " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (SDiv tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "sdiv " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (SDivExact tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "sdiv exact " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (FDiv fm tpe a b) = do
        fmStr <- encode @{nosep} fm
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "fdiv " <+> fmStr <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (URem tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "urem " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (SRem tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "srem " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (FRem fm tpe a b) = do
        fmStr <- encode @{nosep} fm
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "frem " <+> fmStr <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (Shl tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "shl " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (ShlWrap wrap tpe a b) = do
        wrapStr <- encode wrap
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "shl " <+> wrapStr <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (LShr tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "lshr " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (LShrExact tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "lshr exact " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (AShr tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "ashr " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (AShrExact tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "ashr exact " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (And tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "and " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (Or tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "or " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (DisjointOr tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "or disjoint " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (Xor tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "xor " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (InsertElement v1 v2 v3) = do
        v1Str <- encode v1
        v2Str <- encode v2
        v3Str <- encode v3
        pure $ "insertelement " <+> v1Str <+> ", " <+> v2Str <+> ", " <+> v3Str
    encode (ShuffleVector v1 v2 v3) = do
        v1Str <- encode v1
        v2Str <- encode v2
        v3Str <- encode v3
        pure $ "shufflevector " <+> v1Str <+> ", " <+> v2Str <+> ", " <+> v3Str
    encode (ExtractElement v1 v2) = do
        v1Str <- encode v1
        v2Str <- encode v2
        pure $ "extractelement " <+> v1Str <+> ", " <+> v2Str
    encode (ExtractValue v n) = do
        vStr <- encode v
        pure $ "extractvalue " <+> vStr <+> ", " <+> vshow n
    encode (InsertValue v1 v2 n) = do
        v1Str <- encode v1
        v2Str <- encode v2
        pure $ "insertvalue " <+> v1Str <+> ", " <+> v2Str <+> ", " <+> vshow n
    encode (Trunc wrap v tpe) = do
        wrapStr <- encode wrap
        vStr <- encode v
        tpeStr <- encode tpe
        pure $ "trunc " <+> wrapStr <+> vStr <+> " to " <+> tpeStr
    encode (ZExt v tpe) = do
        vStr <- encode v
        tpeStr <- encode tpe
        pure $ "zext " <+> vStr <+> " to " <+> tpeStr
    encode (SExt v tpe) = do
        vStr <- encode v
        tpeStr <- encode tpe
        pure $ "sext " <+> vStr <+> " to " <+> tpeStr
    encode (FPTrunc fm v tpe) = do
        fmStr <- encode @{nosep} fm
        vStr <- encode v
        tpeStr <- encode tpe
        pure $ "fptrunc " <+> fmStr <+> vStr <+> " to " <+> tpeStr
    encode (FPExt fm v tpe) = do
        fmStr <- encode @{nosep} fm
        vStr <- encode v
        tpeStr <- encode tpe
        pure $ "fpext " <+> fmStr <+> vStr <+> " to " <+> tpeStr
    encode (FPToUi v tpe) = do
        vStr <- encode v
        tpeStr <- encode tpe
        pure $ "fptoui " <+> vStr <+> " to " <+> tpeStr
    encode (FPToSi v tpe) = do
        vStr <- encode v
        tpeStr <- encode tpe
        pure $ "fptosi " <+> vStr <+> " to " <+> tpeStr
    encode (UiToFP v tpe) = do
        vStr <- encode v
        tpeStr <- encode tpe
        pure $ "uitofp " <+> vStr <+> " to " <+> tpeStr
    encode (SiToFP v tpe) = do
        vStr <- encode v
        tpeStr <- encode tpe
        pure $ "sitofp " <+> vStr <+> " to " <+> tpeStr
    encode (PtrToInt v tpe) = do
        vStr <- encode v
        tpeStr <- encode tpe
        pure $ "ptrtoint " <+> vStr <+> " to " <+> tpeStr
    encode (BitCast v tpe) = do
        vStr <- encode v
        tpeStr <- encode tpe
        pure $ "bitcast " <+> vStr <+> " to " <+> tpeStr
    encode (AddrSpaceCast addr v tpe) = do
        addrStr <- encode addr
        vStr <- encode v
        tpeStr <- encode tpe
        pure $ "addrspacecast " <+> addrStr <+> vStr <+> " to " <+> tpeStr
    encode (ICmp cmp tpe a b) = do
        cmpStr <- encode cmp
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "icmp " <+> cmpStr <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (ICmpSign cmp tpe a b) = do
        cmpStr <- encode cmp
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "icmp sign " <+> cmpStr <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (FCmpOrd fm cmp tpe a b) = do
        fmStr <- encode @{nosep} fm
        cmpStr <- encode cmp
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "fcmp ord " <+> fmStr <+> cmpStr <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (FCmpUnOrd fm cmp tpe a b) = do
        fmStr <- encode @{nosep} fm
        cmpStr <- encode cmp
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "fcmp unord " <+> fmStr <+> cmpStr <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (FCmpFalse tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "fcmp false " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode (FCmpTrue tpe a b) = do
        tpeStr <- encode tpe
        aStr <- encode a
        bStr <- encode b
        pure $ "fcmp true " <+> tpeStr <+> aStr <+> ", " <+> bStr
    encode _ = ?enle


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
    encode (MkBasicBlock statements term) = do
        statementsStr <- encode @{tabbed} statements
        termStr : VString <- encode term
        pure $ statementsStr <++> "\n" <++> termStr <++> "\n"

public export
Encode ATM (String, BasicBlock) VString where
    encode (n, b) = do
      let n' = go n
      b' <- encode b
      pure (n' <++> ":\n" <+> b')
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
        returnAttrsStr <- pure $ intercalate "," (map (runATM . encode) returnAttrs)
        returnTypeStr <- encode returnType
        argsStr <- pure $ intercalate "," (map (runATM . encode) args)
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
        tagsStr <- encode tags
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
        returnAttrsStr <- pure $ intercalate "," (map (runATM . encode) returnAttrs)
        returnTypeStr <- encode returnType
        argsStr <- pure $ intercalate "," (map (runATM . encode) args)
        addressInfoStr <- encodeIf addressInfo
        alignmentStr <- encodeIf $ show <$> alignment
        gcStr <- encodeIf gc
        fprefixStr <- encodeIf fprefix
        prologueStr <- encodeIf prologue
        tagsStr <- encode tags
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
        tagsStr <- encode tags
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
        attrsStr <- pure $ intercalate "," (map (runATM . encode) attrs)
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
                    Just dl => "target datalayout = \"" <++> runATM (encode dl) <++> "\"\n"
                    Nothing => ""
        let target : VString = 
                case target of
                    Just t => "target triple = \"" <++> runATM (encode t) <++> "\"\n"
                    Nothing => ""
        textStr <- encode @{lined} text
        pure $ layout <++> target <++> textStr


