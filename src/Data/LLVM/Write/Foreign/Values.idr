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
    encode Tail = fcmPure $ (the CEnum (cast 101))
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
Encode FCM LTypeF CPtr where 
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

