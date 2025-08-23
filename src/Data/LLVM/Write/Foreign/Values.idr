module Data.LLVM.Write.Foreign.Values


import Data.LLVM.Class
import Data.LLVM.IR
import Data.LLVM.CC
import Data.LLVM.CC
import public Control.Monad.State
import public Control.Monad.Either 
import public Data.LLVM.Write.Types
import System
import Data.String
import Data.LLVM.Write.Foreign.Monad
%default covering 



export 
Encode FCM Linkage CEnum where
	encode External = fcmPure $ (the CEnum (cast 0))     -- LLVMExternalLinkage
	encode Available = fcmPure $ (the CEnum (cast 1))    -- LLVMAvailableExternallyLinkage
	encode LinkOnce = fcmPure $ (the CEnum (cast 2))     -- LLVMLinkOnceAnyLinkage
	encode LinkOnceODR = fcmPure $ (the CEnum (cast 3))  -- LLVMLinkOnceODRLinkage
	encode Weak = fcmPure $ (the CEnum (cast 5))         -- LLVMWeakAnyLinkage
	encode WeakODR = fcmPure $ (the CEnum (cast 6))      -- LLVMWeakODRLinkage
	encode Appending = fcmPure $ (the CEnum (cast 7))    -- LLVMAppendingLinkage
	encode Internal = fcmPure $ (the CEnum (cast 8))     -- LLVMInternalLinkage
	encode Private = fcmPure $ (the CEnum (cast 9))      -- LLVMPrivateLinkage
	encode ExternWeak = fcmPure $ (the CEnum (cast 12))  -- LLVMExternalWeakLinkage
	encode Common = fcmPure $ (the CEnum (cast 14))      -- LLVMCommonLinkage

export
Encode FCM DLLStorage CEnum where
	encode DLLExport = fcmPure $ (the CEnum (cast 1))
	encode DLLImport = fcmPure $ (the CEnum (cast 2))

export
Encode FCM ThreadLocality CEnum where
	encode LocalDynamic = fcmPure $ (the CEnum (cast 2))  -- LocalDynamicTLSModel
	encode InitialExec = fcmPure $ (the CEnum (cast 3))   -- InitialExecTLSModel
	encode LocalExec = fcmPure $ (the CEnum (cast 4))     -- LocalExecTLSModel

export
Encode FCM Preemption CEnum where
	encode Preemptible = fcmPure $ (the CEnum (cast 0))
	encode NonPreemptible = fcmPure $ (the CEnum (cast 1))

export
Encode FCM AddressSpace CEnum where 
    encode (NamedSpace name) = fcmPure $ (the CEnum (cast 0)) -- Default for named
    encode (UnnamedSpace n) = fcmPure $ (the CEnum (cast n))

export
Encode FCM CallingConvention CEnum where 
    encode C = fcmPure $ (the CEnum (cast 0))            -- LLVMCCallConv
    encode Fast = fcmPure $ (the CEnum (cast 8))         -- LLVMFastCallConv
    encode Cold = fcmPure $ (the CEnum (cast 9))         -- LLVMColdCallConv
    encode GHC = fcmPure $ (the CEnum (cast 10))         -- LLVMGHCCallConv
    encode CC11 = fcmPure $ (the CEnum (cast 11))        -- LLVMHiPECallConv
    encode AnyReg = fcmPure $ (the CEnum (cast 13))      -- LLVMAnyRegCallConv
    encode PreserveMost = fcmPure $ (the CEnum (cast 14)) -- LLVMPreserveMostCallConv
    encode PreserveAll = fcmPure $ (the CEnum (cast 15)) -- LLVMPreserveAllCallConv
    encode Swift = fcmPure $ (the CEnum (cast 16))       -- LLVMSwiftCallConv
    encode CxxFastTL = fcmPure $ (the CEnum (cast 17))   -- LLVMCXXFASTTLSCallConv
    -- Non-standard calling conventions mapped to custom values
    encode PreserveNone = fcmPure $ (the CEnum (cast 100))
    encode TailCC = fcmPure $ (the CEnum (cast 101))
    encode SwiftTail = fcmPure $ (the CEnum (cast 102))
    encode CFGuardCheck = fcmPure $ (the CEnum (cast 103))
    encode (CustomCC n) = fcmPure $ (the CEnum (cast n))

export
Encode FCM Visibility CEnum where 
    encode Default = fcmPure $ (the CEnum (cast 0))
    encode Hidden = fcmPure $ (the CEnum (cast 1))
    encode Protected = fcmPure $ (the CEnum (cast 2))
{- 
export
Encode FCM Name CPtr where
    encode (Local name) = liftFCM $ LLVMConstString ("%" ++ name) (cast $ length name + 1) 0
    encode (Global name) = liftFCM $ LLVMConstString ("@" ++ name) (cast $ length name + 1) 0
    encode (Special name) = liftFCM $ LLVMConstString ("$" ++ name) (cast $ length name + 1) 0
    encode (MetadataN name) = liftFCM $ LLVMConstString ("!" ++ name) (cast $ length name + 1) 0
    encode (AttributeN name) = liftFCM $ LLVMConstString ("#" ++ name) (cast $ length name + 1) 0
    encode (LabelN name) = liftFCM $ LLVMConstString (name ++ ":") (cast $ length name + 1) 0
    encode (IntrinsicN name) = liftFCM $ LLVMConstString ("@llvm." ++ name) (cast $ length name + 6) 0
    encode (CustomN name) = liftFCM $ LLVMConstString name (cast $ length name) 0
    encode Trash = liftFCM $ LLVMConstString "_" 1 0
-}
export
Encode FCM LFloatFormat CPtr where 
    encode Half = liftFCM $ LLVMIntType 16  -- Half precision is typically 16-bit
    encode Bfloat = liftFCM $ LLVMIntType 16  -- Brain float is also 16-bit
    encode LType.LFloat = liftFCM LLVMFloatType
    encode LDouble = liftFCM LLVMDoubleType
    encode FP128 = liftFCM LLVMFP128Type
    encode X86_FP80 = liftFCM LLVMX86FP80Type
    encode PPC_FP128 = liftFCM LLVMPPCFP128Type
export
Encode FCM SymbolInfo CPtr where  
    encode = ?h9

export
Encode FCM AddressInfo CEnum where
	encode UnnamedLocal = fcmPure $ (the CEnum (cast 1))   -- LLVMLocalUnnamedAddr
	encode UnnamedGlobal = fcmPure $ (the CEnum (cast 2))  -- LLVMGlobalUnnamedAddr

export
Encode FCM LTag CPtr where
	encode _ = liftFCM $ LLVMConstString "!{}" 3 0

%default partial
public export 
maybeListToMap : Ord a => {default 0 index : Int} -> List (Maybe a) -> SortedMap a Int
maybeListToMap {index} ((Just x) :: xs) = mergeWith (\x, y => y) (insert x index empty) (maybeListToMap {index = index + 1} xs)
maybeListToMap {index} (Nothing :: xs) = maybeListToMap {index = index + 1} xs
maybeListToMap [] = empty
public export
woScope : Functor f => {b : Type} -> f (WithScope {ref = b} a) -> f a
woScope = map (WithScope.value)
  
export
Encode FCM LType LLVMType where
  encode LType.LPtr = do
    gbl <- liftFCM $ LLVMGetGlobalContext
    ty <- liftFCM $ LLVMPointerTypeInContext gbl
    pure ty
  encode (LPtrTo ty) = do 
    ty' <- encode' ty
    liftFCM $ LLVMPointerType ty' 0
  encode LVoid = liftFCM $ LLVMVoidType
  encode (LFun ret args) = do 
    args' <- traverse encode' args
    ret' <- encode' ret
    argList <- liftIO $ toCList' args'
    let argList' = prim__castPtr argList
    liftFCM $ LLVMFunctionType ret' argList' (cast $ length args) 0
  encode (LType.LInt n) = liftFCM $ LLVMIntType $ cast n
  encode (LType.LVector n ty) = do 
    ty' <- encode' ty
    liftFCM $ LLVMVectorType ty' (cast n)
  encode LOpaque = ?enlopaq
  encode LLabel = fcm $ LLVMLabelTypeInContext !inCon
  encode LType.LToken = fcm $ LLVMTokenTypeInContext !inCon
  encode (LType.LArray size e) = do 
    e' <- encode' e 
    fcm $ LLVMArrayType2 e' $ cast size
  encode (LFloating f) = encode f
  encode (LType.LStruct fields) = do 
    fields' <- traverse encode' fields
    structTy <- fcm $ LLVMStructTypeInContext !inCon !(liftIO $ prim__castPtr <$> toCList' fields') (cast $ length fields) 0
    pure structTy
  encode (LPackedStruct fields) = do 
    fields' <- traverse encode' fields
    structTy <- fcm $ LLVMStructTypeInContext !inCon !(liftIO $ prim__castPtr <$> toCList' fields') (cast $ length fields) 1
    pure structTy
  encode (LFunVarArg ret args rest) = do 
    ret' <- encode' ret
    args' <- traverse encode' args
    rest' <- encode' rest
    argList <- liftIO $ toCList' (args' ++ [rest'])
    liftFCM $ LLVMFunctionType ret' (castPtr argList) (cast $ length args + 1) 1
  encode _ = ?h10
public export
[ewt] {a : Type} -> Encode FCM a CPtr => Encode FCM (WithType a) CPtr where 
    encode = ?h11

public export
{a : Type} -> Encode FCM a CPtr => Encode FCM (WithType a) CPtr where 
    encode = ?h12

withIndex : List a -> List (Int, a)
withIndex xs = go 0 xs
  where
    go : Int -> List a -> List (Int, a)
    go _ [] = []
    go n (x :: xs) = (n, x) :: go (n + 1) xs
mutual
    export
    Encode FCM Metadata CPtr where
        encode (MetadataTuple elems) = do
            elems' <- traverse encode' elems
            elemList <- liftIO $ toCList' elems'
            liftFCM $ LLVMMDNodeInContext !inCon (castPtr elemList) (cast $ length elems)
        encode (MetadataNamed name) = do
            liftFCM $ LLVMMDStringInContext !inCon name (cast $ length name)
        encode (MetadataNode name) = ?mnh
        encode (MetadataString str) = do
            liftFCM $ LLVMMDStringInContext !inCon str (cast $ length str)
        encode (MetadataValue (MkWithType ty expr)) = do
            ty' <- encode' ty
            expr' <- encode' (MkWithType ty expr)
            liftFCM $ LLVMValueAsMetadata expr'
        encode (MetadataCustom custom) = do
            liftFCM $ LLVMMDStringInContext !inCon custom (cast $ length custom)
    
    export
    Encode FCM (WithType LValue) CPtr where
      encode (MkWithType ty v) = step "Make with type" $ do 
        ty' <- encode' ty
        case v of 
          LVar name => step ("get name value: " ++ show name) $ do 
            case name of 
              Temporary n => ?ewte10
              Local n => getScope name
              Global n => liftFCM $ LLVMGetNamedGlobal !inMod n  
              Parameter n => getScope name
              Unnamed n => ?ewte13
          Core.LToken => ?ewte30
          LBool b => do 
            if b then liftFCM $ LLVMConstInt ty' 1 0 else liftFCM $ LLVMConstInt ty' 0 0
          LInt i => liftFCM $ LLVMConstInt ty' (cast i) 0
          LNull => liftFCM $ LLVMConstNull ty'
          Core.LFloat f => liftFCM $ LLVMConstReal ty' ?ewte2
          
          Core.LString s => liftFCM $ LLVMConstString s (cast $ length s + 1) 0
          Core.LPoison => liftFCM $ LLVMGetPoison ty'
          Core.LZero => liftFCM $ LLVMConstNull ty'
          Core.LUndefined => liftFCM $ LLVMGetUndef ty'
          Core.LMetadata md => do 
            md' <- encode md 
            liftFCM $ LLVMMetadataAsValue !inCon md'
          Core.LPtr p => do 
            p' <- encode $ MkWithType ty $ Core.LVar p 
            pure p' -- TODO: is this correct?

          



export 
Encode FCM GVarDef CPtr where
	encode (MkGVarDef name symbolInfo threadLocality addressInfo addressSpace externallyInitialized constant ty init tags) = step "Global var def" $ do 
    ty' <- encode' ty
    seed <- liftFCM $ LLVMAddGlobal !inMod ty' name
    pure seed --TODO:
export
Encode FCM Attribute CPtr where 
    encode = ?h18
export
Encode FCM CaseBranch CPtr where
	encode = ?h19

export
Encode FCM InvokeCall CPtr where
	encode = ?h20

export
Encode FCM Wrapping CEnum where
	encode NoSigned = fcmPure $ (the CEnum (cast 1))
	encode NoUnsigned = fcmPure $ (the CEnum (cast 2))
	encode NoSignedUnsigned = fcmPure $ (the CEnum (cast 3))

    

public export
Encode FCM BrCall CPtr where 
	encode = ?h24
public export
Encode FCM CatchSwitch CPtr where
	encode = ?h25
export
Encode FCM FastMathFlag CEnum where 
	encode FFast = fcmPure $ (the CEnum (cast 1))
	encode NoNaNs = fcmPure $ (the CEnum (cast 2))
	encode NoInfs = fcmPure $ (the CEnum (cast 4))
	encode NoSignedZeros = fcmPure $ (the CEnum (cast 8))

public export
Encode FCM TailCall CEnum where 
	encode Tail = fcmPure $ (the CEnum (cast 1))
	encode MustTail = fcmPure $ (the CEnum (cast 2))
	encode NoTail = fcmPure $ (the CEnum (cast 0))
public export
Encode FCM FnCall CPtr where
	encode fc = step "function call: " $ do 
    -- TODO: addressSpace' <- encode fc.addressSpace
    args' <- traverse encode fc.args
    -- TODO: cc' <- encode fc.cc 
    -- TODO: fm' <- traverse encode fc.fastMath 
    -- TODO: fa' <- encode fc.fnAttrs 
    fv' <- encode $ MkWithType fc.tpe fc.fnval 
    -- TODO: ra' <- encode fc.returnAttrs 
    tc' : CEnum <- encode fc.tail
    ft' <- encode fc.tpe
    cargs <- liftIO $ prim__castPtr <$> toCList' args'
    inBuilder (\b => liftFCM $ LLVMBuildCall2 b ft' fv' cargs (cast $ length fc.args) "fnCall")




emptyNode : VString 
emptyNode = "!{}"
nonTempNode : VString
nonTempNode = "!{ i32 1 }"
export
Encode FCM AtomicOrder CEnum where
	encode Unordered = fcmPure $ (the CEnum (cast 1))
	encode Monotonic = fcmPure $ (the CEnum (cast 2))
	encode Acquire = fcmPure $ (the CEnum (cast 4))
	encode Release = fcmPure $ (the CEnum (cast 5))
	encode AcquireRelease = fcmPure $ (the CEnum (cast 6))
	encode SequentiallyConsistent = fcmPure $ (the CEnum (cast 7))



export 
Encode FCM CatchClause CPtr where
	encode = ?h36



