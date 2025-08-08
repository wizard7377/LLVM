module Data.LLVM.CC.Prim

import Data.LLVM.CC.Macros
import Data.LLVM.CC.Reflect
import Language.Reflection
%language ElabReflection

-- Because I am lazy, a large part of this file was auto generated
public export
||| Primitive Monad for LLVM C bindings.
PM : Type -> Type 
PM = PrimIO
public export
||| `void*`
CPtr : Type 
CPtr = AnyPtr
public export
||| A C FFI List 
CList : Type -> Type 
CList a = Ptr a
public export
||| `unsigned`
CUnsigned : Type 
CUnsigned = Bits32

public export
CEnum : Type 
CEnum = Bits32

public export
CUInt64 : Type 
CUInt64 = Bits64
public export
CULongLong : Type 
CULongLong = Bits64 -- TODO: Check this
public export
LLVMContext : Type 
LLVMContext = CPtr
public export
LLVMType : Type 
LLVMType = CPtr 
public export
LLVMValue : Type
LLVMValue = CPtr
public export
LLVMBool : Type 
LLVMBool = Int -- Not sure who thought of this, but apparently bool is a Int
public export
-- Additional useful type aliases and constants
LLVMBuilder : Type
LLVMBuilder = CPtr
public export
LLVMModule : Type 
LLVMModule = CPtr
public export
LLVMBasicBlock : Type
LLVMBasicBlock = CPtr
public export
LLVMInstruction : Type
LLVMInstruction = CPtr
public export
LLVMGenericValue : Type
LLVMGenericValue = CPtr
public export
LLVMExecutionEngine : Type
LLVMExecutionEngine = CPtr
public export
LLVMPassManager : Type
LLVMPassManager = CPtr
public export
LLVMTargetData : Type
LLVMTargetData = CPtr
public export
LLVMTargetMachine : Type
LLVMTargetMachine = CPtr
public export
LLVMMemoryBuffer : Type
LLVMMemoryBuffer = CPtr
public export
LLVMAttribute : Type
LLVMAttribute = CPtr
public export
LLVMUse : Type
LLVMUse = CPtr
public export
LLVMDiagnosticInfo : Type
LLVMDiagnosticInfo = CPtr
public export
LLVMComdat : Type
LLVMComdat = CPtr
public export
LLVMModuleFlag : Type
LLVMModuleFlag = CPtr
public export
LLVMContextYieldType : Type 
LLVMContextYieldType = CPtr -> CPtr -> PM ()
-- TODO: Contexts?
namespace CPrim 
  -- Contexts 
  %runElab decl `[
    LLVMContextCreate : PM LLVMContext
    LLVMGetGlobalContext : PM LLVMContext
    LLVMContextDispose : LLVMContext -> PM ()
  ]

  -- Modules
  %runElab decl `[
    LLVMModuleCreateWithName : String -> PM CPtr
    LLVMModuleCreateWithNameInContext : String -> CPtr -> PM CPtr
    LLVMCloneModule : CPtr -> PM CPtr
    LLVMDisposeModule : CPtr -> PM ()
    LLVMSetModuleIdentifier : CPtr -> String -> PM ()
    LLVMGetModuleIdentifier : CPtr -> PM String
    LLVMAddFunction : CPtr -> String -> LLVMType -> PM LLVMValue
    LLVMGetDataLayout : LLVMModule -> PM String 
    LLVMSetDataLayout : LLVMModule -> String -> PM ()
    LLVMGetTarget : LLVMModule -> PM String
    LLVMSetTarget : LLVMModule -> String -> PM ()
  ]
  
  -- Types
  %runElab decl `[
    LLVMPrintTypeToString : CPtr -> PM String
    LLVMIntType : CUnsigned -> PM LLVMType
    LLVMFloatType : PM LLVMType
    LLVMDoubleType : PM LLVMType
    LLVMFunctionType : LLVMType -> CList LLVMType -> CUnsigned -> Int -> PM LLVMType
    LLVMStructCreateNamed : CPtr -> String -> PM CPtr
    LLVMStructTypeInContext : CPtr -> CList LLVMType -> CUnsigned -> Int -> PM CPtr
    LLVMStructSetBody : CPtr -> CList CPtr -> CUnsigned -> Int -> PM ()
    LLVMStructType :  CList LLVMType -> CUnsigned -> Int -> PM LLVMType
    LLVMArrayType : LLVMType -> CUnsigned -> PM LLVMType
    LLVMArrayType2 : LLVMType -> CUInt64 -> PM LLVMType
    LLVMPointerType : LLVMType -> CUnsigned -> PM LLVMType
    LLVMPointerTypeInContext : LLVMContext -> PM LLVMType
    LLVMScalableVectorType : LLVMType -> CUnsigned -> PM LLVMType
    LLVMVectorType : LLVMType -> CUnsigned -> PM LLVMType
    LLVMLabelType : PM LLVMType
    LLVMVoidType : PM LLVMType
    LLVMMetadataTypeInContext : CPtr -> PM LLVMType
    LLVMTokenTypeInContext : CPtr -> PM LLVMType
  ]
  -- Values (scalar) TODO: More functions
  %runElab decl `[
    LLVMTypeOf : CPtr -> PM CPtr
    LLVMPrintValueToString : CPtr -> PM String
    LLVMGetValueContext : CPtr -> PM CPtr
    LLVMConstInt : LLVMType -> CULongLong -> LLVMBool -> PM LLVMValue
  ]

  -- Constants (composite)
  %runElab decl `[
    LLVMConstArray : LLVMType -> CList LLVMValue -> CUnsigned -> PM LLVMValue
    LLVMConstNamedStruct : LLVMType -> CList LLVMValue -> CUnsigned -> PM LLVMValue
    LLVMConstString : String -> CUnsigned -> Int -> PM LLVMValue
    LLVMConstStruct : CList LLVMValue -> CUnsigned -> Int -> PM LLVMValue 
    LLVMConstVector : CList LLVMValue -> CUnsigned -> PM LLVMValue
    LLVMGetAggregateElement : CPtr -> CUnsigned -> PM LLVMValue
  ]

  -- Constant expressions
  %runElab decl `[
    LLVMConstAdd : LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstNSWAdd : LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstNUWAdd : LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstSub : LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstNSWSub : LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstNUWSub : LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstMul : LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstNSWMul : LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstNUWMul : LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstAnd : LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstOr : LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstXor : LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstICmp : Int -> LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstFCmp : Int -> LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstShl : LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstLShr : LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstAShr : LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstGEP2 : LLVMType -> LLVMValue -> CList LLVMValue -> CUnsigned -> PM LLVMValue
    LLVMConstInBoundsGEP2 : LLVMType -> LLVMValue -> CList LLVMValue -> CUnsigned -> PM LLVMValue
    LLVMConstTrunc : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstSExt : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstZExt : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstFPTrunc : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstFPExt : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstUIToFP : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstSIToFP : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstFPToUI : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstFPToSI : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstPtrToInt : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstIntToPtr : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstBitCast : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstAddrSpaceCast : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstZExtOrBitCast : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstSExtOrBitCast : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstTruncOrBitCast : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstPointerCast : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstIntCast : LLVMValue -> LLVMType -> LLVMBool -> PM LLVMValue
    LLVMConstFPCast : LLVMValue -> LLVMType -> PM LLVMValue
    LLVMConstSelect : LLVMValue -> LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstExtractElement : LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstInsertElement : LLVMValue -> LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstShuffleVector : LLVMValue -> LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMConstExtractValue : LLVMValue -> CList CUnsigned -> CUnsigned -> PM LLVMValue
    LLVMConstInsertValue : LLVMValue -> LLVMValue -> CList CUnsigned -> CUnsigned -> PM LLVMValue
    LLVMBlockAddress : LLVMValue -> LLVMValue -> PM LLVMValue
  ]

  -- Global variables
  %runElab decl `[
    LLVMAddGlobal : CPtr -> LLVMType -> String -> PM LLVMValue
    LLVMAddGlobalInAddressSpace : CPtr -> LLVMType -> String -> CUnsigned -> PM LLVMValue
    LLVMGetNamedGlobal : CPtr -> String -> PM LLVMValue
    LLVMGetFirstGlobal : CPtr -> PM LLVMValue
    LLVMGetLastGlobal : CPtr -> PM LLVMValue
    LLVMGetNextGlobal : LLVMValue -> PM LLVMValue
    LLVMGetPreviousGlobal : LLVMValue -> PM LLVMValue
    LLVMDeleteGlobal : LLVMValue -> PM ()
    LLVMGetInitializer : LLVMValue -> PM LLVMValue
    LLVMSetInitializer : LLVMValue -> LLVMValue -> PM ()
    LLVMIsThreadLocal : LLVMValue -> PM LLVMBool
    LLVMSetThreadLocal : LLVMValue -> LLVMBool -> PM ()
    LLVMIsGlobalConstant : LLVMValue -> PM LLVMBool
    LLVMSetGlobalConstant : LLVMValue -> LLVMBool -> PM ()
    LLVMGetThreadLocalMode : LLVMValue -> PM Int
    LLVMSetThreadLocalMode : LLVMValue -> Int -> PM ()
    LLVMIsExternallyInitialized : LLVMValue -> PM LLVMBool
    LLVMSetExternallyInitialized : LLVMValue -> LLVMBool -> PM ()
  ]

  -- Global aliases
  %runElab decl `[
    LLVMAddAlias : CPtr -> LLVMType -> LLVMValue -> String -> PM LLVMValue
    LLVMGetNamedGlobalAlias : CPtr -> String -> PM LLVMValue
    LLVMGetFirstGlobalAlias : CPtr -> PM LLVMValue
    LLVMGetLastGlobalAlias : CPtr -> PM LLVMValue
    LLVMGetNextGlobalAlias : LLVMValue -> PM LLVMValue
    LLVMGetPreviousGlobalAlias : LLVMValue -> PM LLVMValue
    LLVMAliasGetAliasee : LLVMValue -> PM LLVMValue
    LLVMAliasSetAliasee : LLVMValue -> LLVMValue -> PM ()
  ]

  -- Functions
  %runElab decl `[
    LLVMDeleteFunction : LLVMValue -> PM ()
    LLVMHasPersonalityFn : LLVMValue -> PM LLVMBool
    LLVMGetPersonalityFn : LLVMValue -> PM LLVMValue
    LLVMSetPersonalityFn : LLVMValue -> LLVMValue -> PM ()
    LLVMLookupIntrinsicID : String -> PM CUnsigned
    LLVMGetIntrinsicDeclaration : CPtr -> CUnsigned -> CList LLVMType -> PM LLVMValue
    LLVMIntrinsicGetType : LLVMContext -> CUnsigned -> CList LLVMType -> PM LLVMType
    LLVMIntrinsicGetName : CUnsigned -> PM String
    LLVMIntrinsicCopyOverloadedName : CUnsigned -> CList LLVMType -> PM String
    LLVMIntrinsicIsOverloaded : CUnsigned -> PM LLVMBool
    LLVMGetFunctionCallConv : LLVMValue -> PM CUnsigned
    LLVMSetFunctionCallConv : LLVMValue -> CUnsigned -> PM ()
    LLVMGetGC : LLVMValue -> PM String
    LLVMSetGC : LLVMValue -> String -> PM ()
    LLVMAddAttributeAtIndex : LLVMValue -> Int -> LLVMValue -> PM ()
    LLVMGetAttributeCountAtIndex : LLVMValue -> Int -> PM CUnsigned
    LLVMGetAttributesAtIndex : LLVMValue -> Int -> CList LLVMValue -> PM ()
    LLVMGetEnumAttributeAtIndex : LLVMValue -> Int -> CUnsigned -> PM LLVMValue
    LLVMGetStringAttributeAtIndex : LLVMValue -> Int -> String -> CUnsigned -> PM LLVMValue
    LLVMRemoveEnumAttributeAtIndex : LLVMValue -> Int -> CUnsigned -> PM ()
    LLVMRemoveStringAttributeAtIndex : LLVMValue -> Int -> String -> CUnsigned -> PM ()
    LLVMAddTargetDependentFunctionAttr : LLVMValue -> String -> String -> PM ()
  ]

  -- Function parameters
  %runElab decl `[
    LLVMCountParams : LLVMValue -> PM CUnsigned
    LLVMGetParams : LLVMValue -> CList LLVMValue -> PM ()
    LLVMGetParam : LLVMValue -> CUnsigned -> PM LLVMValue
    LLVMGetParamParent : LLVMValue -> PM LLVMValue
    LLVMGetFirstParam : LLVMValue -> PM LLVMValue
    LLVMGetLastParam : LLVMValue -> PM LLVMValue
    LLVMGetNextParam : LLVMValue -> PM LLVMValue
    LLVMGetPreviousParam : LLVMValue -> PM LLVMValue
    LLVMSetParamAlignment : LLVMValue -> CUnsigned -> PM ()
  ]

  -- Basic blocks
  %runElab decl `[
    LLVMBasicBlockAsValue : LLVMValue -> PM LLVMValue
    LLVMValueIsBasicBlock : LLVMValue -> PM LLVMBool
    LLVMValueAsBasicBlock : LLVMValue -> PM LLVMValue
    LLVMGetBasicBlockName : LLVMValue -> PM String
    LLVMGetBasicBlockParent : LLVMValue -> PM LLVMValue
    LLVMGetBasicBlockTerminator : LLVMValue -> PM LLVMValue
    LLVMCountBasicBlocks : LLVMValue -> PM CUnsigned
    LLVMGetBasicBlocks : LLVMValue -> CList LLVMValue -> PM ()
    LLVMGetFirstBasicBlock : LLVMValue -> PM LLVMValue
    LLVMGetLastBasicBlock : LLVMValue -> PM LLVMValue
    LLVMGetNextBasicBlock : LLVMValue -> PM LLVMValue
    LLVMGetPreviousBasicBlock : LLVMValue -> PM LLVMValue
    LLVMGetEntryBasicBlock : LLVMValue -> PM LLVMValue
    LLVMInsertExistingBasicBlockAfterInsertBlock : CPtr -> LLVMValue -> PM ()
    LLVMAppendBasicBlockInContext : LLVMContext -> LLVMValue -> String -> PM LLVMValue
    LLVMAppendBasicBlock : LLVMValue -> String -> PM LLVMValue
    LLVMInsertBasicBlockInContext : LLVMContext -> LLVMValue -> String -> PM LLVMValue
    LLVMInsertBasicBlock : LLVMValue -> String -> PM LLVMValue
    LLVMDeleteBasicBlock : LLVMValue -> PM ()
    LLVMRemoveBasicBlockFromParent : LLVMValue -> PM ()
    LLVMMoveBasicBlockBefore : LLVMValue -> LLVMValue -> PM ()
    LLVMMoveBasicBlockAfter : LLVMValue -> LLVMValue -> PM ()
  ]

  -- Instructions
  %runElab decl `[
    LLVMHasMetadata : LLVMValue -> PM LLVMBool
    LLVMGetMetadata : LLVMValue -> CUnsigned -> PM LLVMValue
    LLVMSetMetadata : LLVMValue -> CUnsigned -> LLVMValue -> PM ()
    LLVMInstructionGetAllMetadataOtherThanDebugLoc : LLVMValue -> PM CPtr
    LLVMGetInstructionParent : LLVMValue -> PM LLVMValue
    LLVMGetNextInstruction : LLVMValue -> PM LLVMValue
    LLVMGetPreviousInstruction : LLVMValue -> PM LLVMValue
    LLVMInstructionRemoveFromParent : LLVMValue -> PM ()
    LLVMInstructionEraseFromParent : LLVMValue -> PM ()
    LLVMDeleteInstruction : LLVMValue -> PM ()
    LLVMGetInstructionOpcode : LLVMValue -> PM Int
    LLVMGetICmpPredicate : LLVMValue -> PM Int
    LLVMGetFCmpPredicate : LLVMValue -> PM Int
    LLVMInstructionClone : LLVMValue -> PM LLVMValue
    LLVMIsATerminatorInst : LLVMValue -> PM LLVMValue
  ]

  -- Call sites
  %runElab decl `[
    LLVMGetNumArgOperands : LLVMValue -> PM CUnsigned
    LLVMSetInstructionCallConv : LLVMValue -> CUnsigned -> PM ()
    LLVMGetInstructionCallConv : LLVMValue -> PM CUnsigned
    LLVMSetInstrParamAlignment : LLVMValue -> Int -> CUnsigned -> PM ()
    LLVMAddCallSiteAttribute : LLVMValue -> Int -> LLVMValue -> PM ()
    LLVMGetCallSiteAttributeCount : LLVMValue -> Int -> PM CUnsigned
    LLVMGetCallSiteAttributes : LLVMValue -> Int -> CList LLVMValue -> PM ()
    LLVMGetCallSiteEnumAttribute : LLVMValue -> Int -> CUnsigned -> PM LLVMValue
    LLVMGetCallSiteStringAttribute : LLVMValue -> Int -> String -> CUnsigned -> PM LLVMValue
    LLVMRemoveCallSiteEnumAttribute : LLVMValue -> Int -> CUnsigned -> PM ()
    LLVMRemoveCallSiteStringAttribute : LLVMValue -> Int -> String -> CUnsigned -> PM ()
    LLVMGetCalledFunctionType : LLVMValue -> PM LLVMType
    LLVMGetCalledValue : LLVMValue -> PM LLVMValue
  ]

  -- Operations on call instructions (only)
  %runElab decl `[
    LLVMIsTailCall : LLVMValue -> PM LLVMBool
    LLVMSetTailCall : LLVMValue -> LLVMBool -> PM ()
    LLVMGetNormalDest : LLVMValue -> PM LLVMValue
    LLVMGetUnwindDest : LLVMValue -> PM LLVMValue
    LLVMSetNormalDest : LLVMValue -> LLVMValue -> PM ()
    LLVMSetUnwindDest : LLVMValue -> LLVMValue -> PM ()
  ]

  -- Operations on terminators
  %runElab decl `[
    LLVMGetNumSuccessors : LLVMValue -> PM CUnsigned
    LLVMGetSuccessor : LLVMValue -> CUnsigned -> PM LLVMValue
    LLVMSetSuccessor : LLVMValue -> CUnsigned -> LLVMValue -> PM ()
    LLVMIsConditional : LLVMValue -> PM LLVMBool
    LLVMGetCondition : LLVMValue -> PM LLVMValue
    LLVMSetCondition : LLVMValue -> LLVMValue -> PM ()
    LLVMGetSwitchDefaultDest : LLVMValue -> PM LLVMValue
  ]

  -- Operations on branch instructions (only)
  %runElab decl `[
    LLVMGetBranchWeights : LLVMValue -> PM LLVMValue
    LLVMSetBranchWeights : LLVMValue -> CList CUnsigned -> CUnsigned -> PM ()
  ]

  -- Operations on alloca instructions (only)
  %runElab decl `[
    LLVMGetAllocatedType : LLVMValue -> PM LLVMType
  ]

  -- Operations on gep instructions (only)
  %runElab decl `[
    LLVMIsInBounds : LLVMValue -> PM LLVMBool
    LLVMSetIsInBounds : LLVMValue -> LLVMBool -> PM ()
    LLVMGetGEPSourceElementType : LLVMValue -> PM LLVMType
  ]

  -- Operations on phi nodes
  %runElab decl `[
    LLVMAddIncoming : LLVMValue -> CList LLVMValue -> CList LLVMValue -> CUnsigned -> PM ()
    LLVMCountIncoming : LLVMValue -> PM CUnsigned
    LLVMGetIncomingValue : LLVMValue -> CUnsigned -> PM LLVMValue
    LLVMGetIncomingBlock : LLVMValue -> CUnsigned -> PM LLVMValue
  ]

  -- Operations on extractvalue instructions (only)
  %runElab decl `[
    LLVMGetNumIndices : LLVMValue -> PM CUnsigned
    LLVMGetIndices : LLVMValue -> PM (CList CUnsigned)
  ]

  -- Instruction builders
  %runElab decl `[
    LLVMCreateBuilderInContext : LLVMContext -> PM CPtr
    LLVMCreateBuilder : PM CPtr
    LLVMPositionBuilder : CPtr -> LLVMValue -> LLVMValue -> PM ()
    LLVMPositionBuilderBefore : CPtr -> LLVMValue -> PM ()
    LLVMPositionBuilderAtEnd : CPtr -> LLVMValue -> PM ()
    LLVMGetInsertBlock : CPtr -> PM LLVMValue
    LLVMClearInsertionPosition : CPtr -> PM ()
    LLVMInsertIntoBuilder : CPtr -> LLVMValue -> PM ()
    LLVMInsertIntoBuilderWithName : CPtr -> LLVMValue -> String -> PM ()
    LLVMDisposeBuilder : CPtr -> PM ()
  ]

  -- Metadata
  %runElab decl `[
    LLVMSetCurrentDebugLocation : CPtr -> LLVMValue -> PM ()
    LLVMGetCurrentDebugLocation : CPtr -> PM LLVMValue
    LLVMSetInstDebugLocation : CPtr -> LLVMValue -> PM ()
  ]

  -- Terminators
  %runElab decl `[
    LLVMBuildRetVoid : CPtr -> PM LLVMValue
    LLVMBuildRet : CPtr -> LLVMValue -> PM LLVMValue
    LLVMBuildAggregateRet : CPtr -> CList LLVMValue -> CUnsigned -> PM LLVMValue
    LLVMBuildBr : CPtr -> LLVMValue -> PM LLVMValue
    LLVMBuildCondBr : CPtr -> LLVMValue -> LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMBuildSwitch : CPtr -> LLVMValue -> LLVMValue -> CUnsigned -> PM LLVMValue
    LLVMBuildIndirectBr : CPtr -> LLVMValue -> CUnsigned -> PM LLVMValue
    LLVMBuildInvoke2 : CPtr -> LLVMType -> LLVMValue -> CList LLVMValue -> CUnsigned -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildLandingPad : CPtr -> LLVMType -> LLVMValue -> CUnsigned -> String -> PM LLVMValue
    LLVMBuildResume : CPtr -> LLVMValue -> PM LLVMValue
    LLVMBuildUnreachable : CPtr -> PM LLVMValue
    LLVMBuildCleanupRet : CPtr -> LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMBuildCatchRet : CPtr -> LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMBuildCatchPad : CPtr -> LLVMValue -> CList LLVMValue -> CUnsigned -> String -> PM LLVMValue
    LLVMBuildCleanupPad : CPtr -> LLVMValue -> CList LLVMValue -> CUnsigned -> String -> PM LLVMValue
    LLVMBuildCatchSwitch : CPtr -> LLVMValue -> LLVMValue -> CUnsigned -> String -> PM LLVMValue
  ]

  -- Add a case to a switch instruction
  %runElab decl `[
    LLVMAddCase : LLVMValue -> LLVMValue -> LLVMValue -> PM ()
    LLVMAddDestination : LLVMValue -> LLVMValue -> PM ()
    LLVMGetNumClauses : LLVMValue -> PM CUnsigned
    LLVMGetClause : LLVMValue -> CUnsigned -> PM LLVMValue
    LLVMAddClause : LLVMValue -> LLVMValue -> PM ()
    LLVMIsCleanup : LLVMValue -> PM LLVMBool
    LLVMSetCleanup : LLVMValue -> LLVMBool -> PM ()
  ]

  -- Arithmetic
  %runElab decl `[
    LLVMBuildAdd : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildNSWAdd : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildNUWAdd : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildFAdd : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildSub : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildNSWSub : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildNUWSub : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildFSub : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildMul : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildNSWMul : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildNUWMul : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildFMul : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildUDiv : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildExactUDiv : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildSDiv : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildExactSDiv : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildFDiv : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildURem : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildSRem : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildFRem : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildShl : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildLShr : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildAShr : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildAnd : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildOr : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildXor : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildBinOp : CPtr -> Int -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildNeg : CPtr -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildNSWNeg : CPtr -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildNUWNeg : CPtr -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildFNeg : CPtr -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildNot : CPtr -> LLVMValue -> String -> PM LLVMValue
  ]

  -- Memory
  %runElab decl `[
    LLVMBuildMalloc : CPtr -> LLVMType -> String -> PM LLVMValue
    LLVMBuildArrayMalloc : CPtr -> LLVMType -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildMemSet : CPtr -> LLVMValue -> LLVMValue -> LLVMValue -> CUnsigned -> PM LLVMValue
    LLVMBuildMemCpy : CPtr -> LLVMValue -> CUnsigned -> LLVMValue -> CUnsigned -> LLVMValue -> PM LLVMValue
    LLVMBuildMemMove : CPtr -> LLVMValue -> CUnsigned -> LLVMValue -> CUnsigned -> LLVMValue -> PM LLVMValue
    LLVMBuildAlloca : CPtr -> LLVMType -> String -> PM LLVMValue
    LLVMBuildArrayAlloca : CPtr -> LLVMType -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildFree : CPtr -> LLVMValue -> PM LLVMValue
    LLVMBuildLoad2 : CPtr -> LLVMType -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildStore : CPtr -> LLVMValue -> LLVMValue -> PM LLVMValue
    LLVMBuildGEP2 : CPtr -> LLVMType -> LLVMValue -> CList LLVMValue -> CUnsigned -> String -> PM LLVMValue
    LLVMBuildInBoundsGEP2 : CPtr -> LLVMType -> LLVMValue -> CList LLVMValue -> CUnsigned -> String -> PM LLVMValue
    LLVMBuildStructGEP2 : CPtr -> LLVMType -> LLVMValue -> CUnsigned -> String -> PM LLVMValue
    LLVMBuildGlobalString : CPtr -> String -> String -> PM LLVMValue
    LLVMBuildGlobalStringPtr : CPtr -> String -> String -> PM LLVMValue
    LLVMGetVolatile : LLVMValue -> PM LLVMBool
    LLVMSetVolatile : LLVMValue -> LLVMBool -> PM ()
    LLVMGetWeak : LLVMValue -> PM LLVMBool
    LLVMSetWeak : LLVMValue -> LLVMBool -> PM ()
    LLVMGetOrdering : LLVMValue -> PM Int
    LLVMSetOrdering : LLVMValue -> Int -> PM ()
    LLVMGetAtomicRMWBinOp : LLVMValue -> PM Int
    LLVMSetAtomicRMWBinOp : LLVMValue -> Int -> PM ()
  ]

  -- Casts
  %runElab decl `[
    LLVMBuildTrunc : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildZExt : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildSExt : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildFPToUI : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildFPToSI : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildUIToFP : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildSIToFP : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildFPTrunc : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildFPExt : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildPtrToInt : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildIntToPtr : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildBitCast : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildAddrSpaceCast : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildZExtOrBitCast : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildSExtOrBitCast : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildTruncOrBitCast : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildCast : CPtr -> Int -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildPointerCast : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildIntCast2 : CPtr -> LLVMValue -> LLVMType -> LLVMBool -> String -> PM LLVMValue
    LLVMBuildFPCast : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMGetCastOpcode : LLVMValue -> LLVMBool -> LLVMType -> LLVMBool -> PM Int
  ]

  -- Comparisons
  %runElab decl `[
    LLVMBuildICmp : CPtr -> Int -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildFCmp : CPtr -> Int -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
  ]

  -- Miscellaneous instructions
  %runElab decl `[
    LLVMBuildPhi : CPtr -> LLVMType -> String -> PM LLVMValue
    LLVMBuildCall2 : CPtr -> LLVMType -> LLVMValue -> CList LLVMValue -> CUnsigned -> String -> PM LLVMValue
    LLVMBuildSelect : CPtr -> LLVMValue -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildVAArg : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
    LLVMBuildExtractElement : CPtr -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildInsertElement : CPtr -> LLVMValue -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildShuffleVector : CPtr -> LLVMValue -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildExtractValue : CPtr -> LLVMValue -> CUnsigned -> String -> PM LLVMValue
    LLVMBuildInsertValue : CPtr -> LLVMValue -> LLVMValue -> CUnsigned -> String -> PM LLVMValue
    LLVMBuildFreeze : CPtr -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildIsNull : CPtr -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildIsNotNull : CPtr -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildPtrDiff2 : CPtr -> LLVMType -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildFence : CPtr -> Int -> LLVMBool -> String -> PM LLVMValue
    LLVMBuildAtomicRMW : CPtr -> Int -> LLVMValue -> LLVMValue -> Int -> LLVMBool -> PM LLVMValue
    LLVMBuildAtomicCmpXchg : CPtr -> LLVMValue -> LLVMValue -> LLVMValue -> Int -> Int -> LLVMBool -> PM LLVMValue
  ]

  -- Module providers (Deprecated)
  %runElab decl `[
    LLVMCreateModuleProviderForExistingModule : CPtr -> PM CPtr
    LLVMDisposeModuleProvider : CPtr -> PM ()
  ]

  -- Memory buffers
  %runElab decl `[
    LLVMCreateMemoryBufferWithContentsOfFile : String -> CPtr -> CPtr -> PM LLVMBool
    LLVMCreateMemoryBufferWithSTDIN : CPtr -> CPtr -> PM LLVMBool
    LLVMCreateMemoryBufferWithMemoryRange : String -> CUnsigned -> String -> LLVMBool -> PM CPtr
    LLVMCreateMemoryBufferWithMemoryRangeCopy : String -> CUnsigned -> String -> PM CPtr
    LLVMGetBufferStart : CPtr -> PM String
    LLVMGetBufferSize : CPtr -> PM CUnsigned
    LLVMDisposeMemoryBuffer : CPtr -> PM ()
  ]

  -- Pass registry
  %runElab decl `[
    LLVMGetGlobalPassRegistry : PM CPtr
  ]

  -- Pass managers
  %runElab decl `[
    LLVMCreatePassManager : PM CPtr
    LLVMCreateFunctionPassManagerForModule : CPtr -> PM CPtr
    LLVMCreateFunctionPassManager : CPtr -> PM CPtr
    LLVMRunPassManager : CPtr -> CPtr -> PM LLVMBool
    LLVMInitializeFunctionPassManager : CPtr -> PM LLVMBool
    LLVMRunFunctionPassManager : CPtr -> LLVMValue -> PM LLVMBool
    LLVMFinalizeFunctionPassManager : CPtr -> PM LLVMBool
    LLVMDisposePassManager : CPtr -> PM ()
  ]

  -- Threading
  %runElab decl `[
    LLVMStartMultithreaded : PM LLVMBool
    LLVMStopMultithreaded : PM ()
    LLVMIsMultithreaded : PM LLVMBool
  ]

  -- Attributes
  %runElab decl `[
    LLVMCreateEnumAttribute : LLVMContext -> CUnsigned -> CULongLong -> PM LLVMValue
    LLVMGetEnumAttributeKind : LLVMValue -> PM CUnsigned
    LLVMGetEnumAttributeValue : LLVMValue -> PM CULongLong
    LLVMCreateTypeAttribute : LLVMContext -> CUnsigned -> LLVMType -> PM LLVMValue
    LLVMGetTypeAttributeValue : LLVMValue -> PM LLVMType
    LLVMCreateStringAttribute : LLVMContext -> String -> CUnsigned -> String -> CUnsigned -> PM LLVMValue
    LLVMGetStringAttributeKind : LLVMValue -> CPtr -> PM String
    LLVMGetStringAttributeValue : LLVMValue -> CPtr -> PM String
    LLVMIsEnumAttribute : LLVMValue -> PM LLVMBool
    LLVMIsStringAttribute : LLVMValue -> PM LLVMBool
    LLVMIsTypeAttribute : LLVMValue -> PM LLVMBool
    LLVMGetLastEnumAttributeKind : PM CUnsigned
  ]

  -- Module parsing and printing
  -- LLVMPrintValueToString : LLVMValue -> PM String 
  -- TODO: ?
  %runElab decl `[
    LLVMParseIRInContext : LLVMContext -> CPtr -> CPtr -> CPtr -> PM LLVMBool
    LLVMPrintModuleToString : CPtr -> PM String
    LLVMPrintModuleToFile : CPtr -> String -> CPtr -> PM LLVMBool
    LLVMSetModuleDataLayout : CPtr -> CPtr -> PM ()
    LLVMGetModuleDataLayout : CPtr -> PM CPtr
    LLVMCopyStringRepOfTargetData : CPtr -> PM String
    LLVMByteOrder : CPtr -> PM Int
    LLVMPointerSize : CPtr -> PM CUnsigned
    LLVMPointerSizeForAS : CPtr -> CUnsigned -> PM CUnsigned
    LLVMIntPtrType : CPtr -> PM LLVMType
    LLVMIntPtrTypeForAS : CPtr -> CUnsigned -> PM LLVMType
    LLVMIntPtrTypeInContext : LLVMContext -> CPtr -> PM LLVMType
    LLVMIntPtrTypeForASInContext : LLVMContext -> CPtr -> CUnsigned -> PM LLVMType
    LLVMSizeOfTypeInBits : CPtr -> LLVMType -> PM CULongLong
    LLVMStoreSizeOfType : CPtr -> LLVMType -> PM CULongLong
    LLVMABISizeOfType : CPtr -> LLVMType -> PM CULongLong
    LLVMABIAlignmentOfType : CPtr -> LLVMType -> PM CUnsigned
    LLVMCallFrameAlignmentOfType : CPtr -> LLVMType -> PM CUnsigned
    LLVMPreferredAlignmentOfType : CPtr -> LLVMType -> PM CUnsigned
    LLVMPreferredAlignmentOfGlobal : CPtr -> LLVMValue -> PM CUnsigned
    LLVMElementAtOffset : CPtr -> LLVMType -> CULongLong -> PM CUnsigned
    LLVMOffsetOfElement : CPtr -> LLVMType -> CUnsigned -> PM CULongLong
  ]

  -- Target data
  %runElab decl `[
    LLVMCreateTargetData : String -> PM CPtr
    LLVMDisposeTargetData : CPtr -> PM ()
    LLVMAddTargetLibraryInfo : CPtr -> CPtr -> PM ()
  ]

  -- Bitcode reading and writing
  %runElab decl `[
    LLVMParseBitcode : CPtr -> CPtr -> CPtr -> PM LLVMBool
    LLVMParseBitcode2 : CPtr -> CPtr -> PM LLVMBool
    LLVMParseBitcodeInContext : LLVMContext -> CPtr -> CPtr -> CPtr -> PM LLVMBool
    LLVMParseBitcodeInContext2 : LLVMContext -> CPtr -> CPtr -> PM LLVMBool
    LLVMGetBitcodeModuleInContext : LLVMContext -> CPtr -> CPtr -> CPtr -> PM LLVMBool
    LLVMGetBitcodeModuleInContext2 : LLVMContext -> CPtr -> CPtr -> PM LLVMBool
    LLVMGetBitcodeModule : CPtr -> CPtr -> CPtr -> PM LLVMBool
    LLVMGetBitcodeModule2 : CPtr -> CPtr -> PM LLVMBool
    LLVMWriteBitcodeToFile : CPtr -> String -> PM Int
    LLVMWriteBitcodeToFD : CPtr -> Int -> Int -> Int -> PM Int
    LLVMWriteBitcodeToFileHandle : CPtr -> Int -> PM Int
    LLVMWriteBitcodeToMemoryBuffer : CPtr -> PM CPtr
  ]

  -- Linker
  %runElab decl `[
    LLVMLinkModules2 : CPtr -> CPtr -> PM LLVMBool
  ]

  -- Error handling
  %runElab decl `[
    LLVMInstallFatalErrorHandler : CPtr -> PM ()
    LLVMResetFatalErrorHandler : PM ()
    LLVMEnablePrettyStackTrace : PM ()
    LLVMGetErrorTypeId : CPtr -> PM CPtr
    LLVMGetErrorMessage : CPtr -> PM String
    LLVMDisposeErrorMessage : String -> PM ()
    LLVMGetStringErrorTypeId : PM CPtr
    LLVMCreateStringError : String -> PM CPtr
  ]

  -- Object file reading
  %runElab decl `[
    LLVMCreateObjectFile : CPtr -> PM CPtr
    LLVMDisposeObjectFile : CPtr -> PM ()
    LLVMGetSections : CPtr -> PM CPtr
    LLVMDisposeSectionIterator : CPtr -> PM ()
    LLVMIsSectionIteratorAtEnd : CPtr -> CPtr -> PM LLVMBool
    LLVMMoveToNextSection : CPtr -> PM ()
    LLVMMoveToContainingSection : CPtr -> CPtr -> PM ()
    LLVMDisposeSymbolIterator : CPtr -> PM ()
    LLVMIsSymbolIteratorAtEnd : CPtr -> CPtr -> PM LLVMBool
    LLVMMoveToNextSymbol : CPtr -> PM ()
    LLVMGetSectionName : CPtr -> PM String
    LLVMGetSectionSize : CPtr -> PM CULongLong
    LLVMGetSectionContents : CPtr -> PM String
    LLVMGetSectionAddress : CPtr -> PM CULongLong
    LLVMGetSectionContainsSymbol : CPtr -> CPtr -> PM LLVMBool
    LLVMGetRelocations : CPtr -> PM CPtr
    LLVMDisposeRelocationIterator : CPtr -> PM ()
    LLVMIsRelocationIteratorAtEnd : CPtr -> CPtr -> PM LLVMBool
    LLVMMoveToNextRelocation : CPtr -> PM ()
    LLVMGetSymbolName : CPtr -> PM String
    LLVMGetSymbolAddress : CPtr -> PM CULongLong
    LLVMGetSymbolSize : CPtr -> PM CULongLong
    LLVMGetRelocationOffset : CPtr -> PM CULongLong
    LLVMGetRelocationSymbol : CPtr -> PM CPtr
    LLVMGetRelocationType : CPtr -> PM CULongLong
    LLVMGetRelocationTypeName : CPtr -> PM String
    LLVMGetRelocationValueString : CPtr -> PM String
  ]

  -- Disassembler
  %runElab decl `[
    LLVMCreateDisasm : String -> CPtr -> Int -> CPtr -> CPtr -> PM CPtr
    LLVMCreateDisasmCPU : String -> String -> CPtr -> Int -> CPtr -> CPtr -> PM CPtr
    LLVMCreateDisasmCPUFeatures : String -> String -> String -> CPtr -> Int -> CPtr -> CPtr -> PM CPtr
    LLVMSetDisasmOptions : CPtr -> CULongLong -> PM Int
    LLVMDisasmDispose : CPtr -> PM ()
    LLVMDisasmInstruction : CPtr -> CPtr -> CULongLong -> CULongLong -> String -> CUnsigned -> PM CUnsigned
  ]

  -- JIT/MCJIT (Legacy)
  %runElab decl `[
    LLVMLinkInMCJIT : PM ()
    LLVMLinkInInterpreter : PM ()
    LLVMCreateGenericValueOfInt : LLVMType -> CULongLong -> LLVMBool -> PM CPtr
    LLVMCreateGenericValueOfPointer : CPtr -> PM CPtr
    LLVMCreateGenericValueOfFloat : LLVMType -> Double -> PM CPtr
    LLVMGenericValueIntWidth : CPtr -> PM CUnsigned
    LLVMGenericValueToInt : CPtr -> LLVMBool -> PM CULongLong
    LLVMGenericValueToPointer : CPtr -> PM CPtr
    LLVMGenericValueToFloat : LLVMType -> CPtr -> PM Double
    LLVMDisposeGenericValue : CPtr -> PM ()
    LLVMCreateExecutionEngineForModule : CPtr -> CPtr -> CPtr -> PM LLVMBool
    LLVMCreateInterpreterForModule : CPtr -> CPtr -> CPtr -> PM LLVMBool
    LLVMCreateJITCompilerForModule : CPtr -> CPtr -> CUnsigned -> CPtr -> PM LLVMBool
    LLVMInitializeMCJITCompilerOptions : CPtr -> CUnsigned -> PM ()
    LLVMCreateMCJITCompilerForModule : CPtr -> CPtr -> CPtr -> CUnsigned -> CPtr -> PM LLVMBool
    LLVMDisposeExecutionEngine : CPtr -> PM ()
    LLVMRunStaticConstructors : CPtr -> PM ()
    LLVMRunStaticDestructors : CPtr -> PM ()
    LLVMRunFunctionAsMain : CPtr -> LLVMValue -> CUnsigned -> CList String -> CList String -> PM Int
    LLVMRunFunction : CPtr -> LLVMValue -> CUnsigned -> CList CPtr -> PM CPtr
    LLVMFreeMachineCodeForFunction : CPtr -> LLVMValue -> PM ()
    LLVMAddModule : CPtr -> CPtr -> PM ()
    LLVMRemoveModule : CPtr -> CPtr -> CPtr -> CPtr -> PM LLVMBool
    LLVMFindFunction : CPtr -> String -> CPtr -> PM LLVMBool
    LLVMRecompileAndRelinkFunction : CPtr -> LLVMValue -> PM CPtr
    LLVMGetExecutionEngineTargetData : CPtr -> PM CPtr
    LLVMGetExecutionEngineTargetMachine : CPtr -> PM CPtr
    LLVMAddGlobalMapping : CPtr -> LLVMValue -> CPtr -> PM ()
    LLVMGetPointerToGlobal : CPtr -> LLVMValue -> PM CPtr
    LLVMGetGlobalValueAddress : CPtr -> String -> PM CULongLong
    LLVMGetFunctionAddress : CPtr -> String -> PM CULongLong
    LLVMExecutionEngineGetErrMsg : CPtr -> CPtr -> PM LLVMBool
  ]

  -- ORC JIT API v2
  %runElab decl `[
    LLVMOrcGetErrorMsg : CPtr -> PM String
    LLVMOrcDisposeSymbols : CPtr -> PM ()
    LLVMOrcCreateNewThreadSafeContext : PM CPtr
    LLVMOrcThreadSafeContextGetContext : CPtr -> PM LLVMContext
    LLVMOrcDisposeThreadSafeContext : CPtr -> PM ()
    LLVMOrcCreateNewThreadSafeModule : CPtr -> CPtr -> PM CPtr
    LLVMOrcDisposeThreadSafeModule : CPtr -> PM ()
    LLVMOrcJITTargetMachineBuilderDetectHost : CPtr -> PM CPtr
    LLVMOrcJITTargetMachineBuilderCreateFromTargetMachine : CPtr -> PM CPtr
    LLVMOrcDisposeJITTargetMachineBuilder : CPtr -> PM ()
    LLVMOrcCreateLLJITBuilder : PM CPtr
    LLVMOrcDisposeLLJITBuilder : CPtr -> PM ()
    LLVMOrcLLJITBuilderSetJITTargetMachineBuilder : CPtr -> CPtr -> PM ()
    LLVMOrcCreateLLJIT : CPtr -> CPtr -> PM CPtr
    LLVMOrcDisposeLLJIT : CPtr -> PM ()
    LLVMOrcLLJITGetExecutionSession : CPtr -> PM CPtr
    LLVMOrcLLJITGetMainJITDylib : CPtr -> PM CPtr
    LLVMOrcLLJITGetTripleString : CPtr -> PM String
    LLVMOrcLLJITGetGlobalPrefix : CPtr -> PM Char
    LLVMOrcLLJITMangleAndIntern : CPtr -> CPtr -> String -> PM CPtr
    LLVMOrcLLJITAddLLVMIRModule : CPtr -> CPtr -> CPtr -> PM CPtr
    LLVMOrcLLJITAddLLVMIRModuleWithRT : CPtr -> CPtr -> CPtr -> PM CPtr
    LLVMOrcLLJITAddObjectFile : CPtr -> CPtr -> CPtr -> PM CPtr
    LLVMOrcLLJITAddObjectFileWithRT : CPtr -> CPtr -> CPtr -> PM CPtr
    LLVMOrcLLJITLookup : CPtr -> CPtr -> String -> PM CULongLong
    LLVMOrcLLJITGetObjLinkingLayer : CPtr -> PM CPtr
    LLVMOrcLLJITGetObjTransformLayer : CPtr -> PM CPtr
    LLVMOrcLLJITGetIRTransformLayer : CPtr -> PM CPtr
    LLVMOrcLLJITGetDataLayoutStr : CPtr -> PM String
  ]

  -- Target machine
  %runElab decl `[
    LLVMGetFirstTarget : PM CPtr
    LLVMGetNextTarget : CPtr -> PM CPtr
    LLVMGetTargetFromName : String -> PM CPtr
    LLVMGetTargetFromTriple : String -> CPtr -> CPtr -> PM LLVMBool
    LLVMGetTargetName : CPtr -> PM String
    LLVMGetTargetDescription : CPtr -> PM String
    LLVMTargetHasJIT : CPtr -> PM LLVMBool
    LLVMTargetHasTargetMachine : CPtr -> PM LLVMBool
    LLVMTargetHasAsmBackend : CPtr -> PM LLVMBool
    LLVMCreateTargetMachine : CPtr -> String -> String -> String -> Int -> Int -> Int -> PM CPtr
    LLVMDisposeTargetMachine : CPtr -> PM ()
    LLVMGetTargetMachineTarget : CPtr -> PM CPtr
    LLVMGetTargetMachineTriple : CPtr -> PM String
    LLVMGetTargetMachineCPU : CPtr -> PM String
    LLVMGetTargetMachineFeatureString : CPtr -> PM String
    LLVMCreateTargetDataLayout : CPtr -> PM CPtr
    LLVMSetTargetMachineAsmVerbosity : CPtr -> LLVMBool -> PM ()
    LLVMTargetMachineEmitToFile : CPtr -> CPtr -> String -> Int -> CPtr -> PM LLVMBool
    LLVMTargetMachineEmitToMemoryBuffer : CPtr -> CPtr -> Int -> CPtr -> CPtr -> PM LLVMBool
    LLVMGetDefaultTargetTriple : PM String
    LLVMNormalizeTargetTriple : String -> PM String
    LLVMGetHostCPUName : PM String
    LLVMGetHostCPUFeatures : PM String
    LLVMAddAnalysisPasses : CPtr -> CPtr -> PM ()
  ]

  -- Transform utilities
  %runElab decl `[
    LLVMAddAggressiveInstCombinerPass : CPtr -> PM ()
    LLVMAddCoroEarlyPass : CPtr -> PM ()
    LLVMAddCoroSplitPass : CPtr -> PM ()
    LLVMAddCoroElidePass : CPtr -> PM ()
    LLVMAddCoroCleanupPass : CPtr -> PM ()
    LLVMAddInstructionCombiningPass : CPtr -> PM ()
    LLVMAddArgumentPromotionPass : CPtr -> PM ()
    LLVMAddConstantMergePass : CPtr -> PM ()
    LLVMAddMergeFunctionsPass : CPtr -> PM ()
    LLVMAddCalledValuePropagationPass : CPtr -> PM ()
    LLVMAddDeadArgEliminationPass : CPtr -> PM ()
    LLVMAddFunctionAttrsPass : CPtr -> PM ()
    LLVMAddFunctionInliningPass : CPtr -> PM ()
    LLVMAddAlwaysInlinerPass : CPtr -> PM ()
    LLVMAddGlobalDCEPass : CPtr -> PM ()
    LLVMAddGlobalOptimizerPass : CPtr -> PM ()
    LLVMAddPruneEHPass : CPtr -> PM ()
    LLVMAddIPSCCPPass : CPtr -> PM ()
    LLVMAddInternalizePass : CPtr -> CUnsigned -> PM ()
    LLVMAddStripDeadPrototypesPass : CPtr -> PM ()
    LLVMAddStripSymbolsPass : CPtr -> PM ()
  ]

  -- Scalar transforms
  %runElab decl `[
    LLVMAddAggressiveDCEPass : CPtr -> PM ()
    LLVMAddBitTrackingDCEPass : CPtr -> PM ()
    LLVMAddAlignmentFromAssumptionsPass : CPtr -> PM ()
    LLVMAddCFGSimplificationPass : CPtr -> PM ()
    LLVMAddDeadStoreEliminationPass : CPtr -> PM ()
    LLVMAddScalarizerPass : CPtr -> PM ()
    LLVMAddMergedLoadStoreMotionPass : CPtr -> PM ()
    LLVMAddGVNPass : CPtr -> PM ()
    LLVMAddNewGVNPass : CPtr -> PM ()
    LLVMAddIndVarSimplifyPass : CPtr -> PM ()
    LLVMAddJumpThreadingPass : CPtr -> PM ()
    LLVMAddLICMPass : CPtr -> PM ()
    LLVMAddLoopDeletionPass : CPtr -> PM ()
    LLVMAddLoopIdiomPass : CPtr -> PM ()
    LLVMAddLoopRotatePass : CPtr -> PM ()
    LLVMAddLoopRerollPass : CPtr -> PM ()
    LLVMAddLoopUnrollPass : CPtr -> PM ()
    LLVMAddLoopUnrollAndJamPass : CPtr -> PM ()
    LLVMAddLowerSwitchPass : CPtr -> PM ()
    LLVMAddMemCpyOptPass : CPtr -> PM ()
    LLVMAddPartiallyInlineLibCallsPass : CPtr -> PM ()
    LLVMAddReassociatePass : CPtr -> PM ()
    LLVMAddSCCPPass : CPtr -> PM ()
    LLVMAddScalarReplAggregatesPass : CPtr -> PM ()
    LLVMAddScalarReplAggregatesPassSSA : CPtr -> PM ()
    LLVMAddScalarReplAggregatesPassWithThreshold : CPtr -> Int -> PM ()
    LLVMAddSimplifyLibCallsPass : CPtr -> PM ()
    LLVMAddTailCallEliminationPass : CPtr -> PM ()
    LLVMAddDemoteMemoryToRegisterPass : CPtr -> PM ()
    LLVMAddVerifierPass : CPtr -> PM ()
    LLVMAddCorrelatedValuePropagationPass : CPtr -> PM ()
    LLVMAddEarlyCSEPass : CPtr -> PM ()
    LLVMAddEarlyCSEMemSSAPass : CPtr -> PM ()
    LLVMAddLowerExpectIntrinsicPass : CPtr -> PM ()
    LLVMAddLowerConstantIntrinsicsPass : CPtr -> PM ()
    LLVMAddTypeBasedAliasAnalysisPass : CPtr -> PM ()
    LLVMAddScopedNoAliasAAPass : CPtr -> PM ()
    LLVMAddBasicAliasAnalysisPass : CPtr -> PM ()
    LLVMAddUnifyFunctionExitNodesPass : CPtr -> PM ()
  ]

  -- Vectorization
  %runElab decl `[
    LLVMAddLoopVectorizePass : CPtr -> PM ()
    LLVMAddSLPVectorizePass : CPtr -> PM ()
  ]

  -- Analysis
  %runElab decl `[
    LLVMVerifyModule : CPtr -> Int -> CPtr -> PM LLVMBool
    LLVMVerifyFunction : LLVMValue -> Int -> PM LLVMBool
    LLVMViewFunctionCFG : LLVMValue -> PM ()
    LLVMViewFunctionCFGOnly : LLVMValue -> PM ()
  ]


  -- Operand and use operations
  namespace CPrim
    %runElab decl `[
      LLVMGetFirstUse : LLVMValue -> PM LLVMUse
      LLVMGetNextUse : LLVMUse -> PM LLVMUse
      LLVMGetUser : LLVMUse -> PM LLVMValue
      LLVMGetUsedValue : LLVMUse -> PM LLVMValue
      LLVMGetOperand : LLVMValue -> CUnsigned -> PM LLVMValue
      LLVMGetOperandUse : LLVMValue -> CUnsigned -> PM LLVMUse
      LLVMSetOperand : LLVMValue -> CUnsigned -> LLVMValue -> PM ()
      LLVMGetNumOperands : LLVMValue -> PM Int
    ]

    -- Constants (scalar)
    %runElab decl `[
      LLVMConstNull : LLVMType -> PM LLVMValue
      LLVMConstAllOnes : LLVMType -> PM LLVMValue
      LLVMGetUndef : LLVMType -> PM LLVMValue
      LLVMGetPoison : LLVMType -> PM LLVMValue
      LLVMIsConstant : LLVMValue -> PM LLVMBool
      LLVMIsNull : LLVMValue -> PM LLVMBool
      LLVMIsUndef : LLVMValue -> PM LLVMBool
      LLVMIsPoison : LLVMValue -> PM LLVMBool
      LLVMConstPointerNull : LLVMType -> PM LLVMValue
      LLVMConstReal : LLVMType -> Double -> PM LLVMValue
      LLVMConstRealOfString : LLVMType -> String -> PM LLVMValue
      LLVMConstRealOfStringAndSize : LLVMType -> String -> CUnsigned -> PM LLVMValue
      LLVMConstIntOfArbitraryPrecision : LLVMType -> CUnsigned -> CList CULongLong -> PM LLVMValue
      LLVMConstIntOfString : LLVMType -> String -> CUnsigned -> PM LLVMValue
      LLVMConstIntOfStringAndSize : LLVMType -> String -> CUnsigned -> CUnsigned -> PM LLVMValue
    ]

    -- More value operations
    %runElab decl `[
      LLVMGetValueName2 : LLVMValue -> CPtr -> PM String
      LLVMSetValueName2 : LLVMValue -> String -> CUnsigned -> PM ()
      LLVMDumpValue : LLVMValue -> PM ()
      LLVMReplaceAllUsesWith : LLVMValue -> LLVMValue -> PM ()
      LLVMIsAArgument : LLVMValue -> PM LLVMValue
      LLVMIsABasicBlock : LLVMValue -> PM LLVMValue
      LLVMIsAInlineAsm : LLVMValue -> PM LLVMValue
      LLVMIsAUser : LLVMValue -> PM LLVMValue
      LLVMIsAConstant : LLVMValue -> PM LLVMValue
      LLVMIsABlockAddress : LLVMValue -> PM LLVMValue
      LLVMIsAConstantAggregateZero : LLVMValue -> PM LLVMValue
      LLVMIsAConstantArray : LLVMValue -> PM LLVMValue
      LLVMIsAConstantDataSequential : LLVMValue -> PM LLVMValue
      LLVMIsAConstantDataArray : LLVMValue -> PM LLVMValue
      LLVMIsAConstantDataVector : LLVMValue -> PM LLVMValue
      LLVMIsAConstantExpr : LLVMValue -> PM LLVMValue
      LLVMIsAConstantFP : LLVMValue -> PM LLVMValue
      LLVMIsAConstantInt : LLVMValue -> PM LLVMValue
      LLVMIsAConstantPointerNull : LLVMValue -> PM LLVMValue
      LLVMIsAConstantStruct : LLVMValue -> PM LLVMValue
      LLVMIsAConstantTokenNone : LLVMValue -> PM LLVMValue
      LLVMIsAConstantVector : LLVMValue -> PM LLVMValue
      LLVMIsAGlobalValue : LLVMValue -> PM LLVMValue
      LLVMIsAGlobalAlias : LLVMValue -> PM LLVMValue
      LLVMIsAGlobalObject : LLVMValue -> PM LLVMValue
      LLVMIsAFunction : LLVMValue -> PM LLVMValue
      LLVMIsAGlobalVariable : LLVMValue -> PM LLVMValue
      LLVMIsAUndefValue : LLVMValue -> PM LLVMValue
      LLVMIsAPoisonValue : LLVMValue -> PM LLVMValue
      LLVMIsAInstruction : LLVMValue -> PM LLVMValue
      LLVMIsAUnaryInstruction : LLVMValue -> PM LLVMValue
      LLVMIsABinaryOperator : LLVMValue -> PM LLVMValue
      LLVMIsACallInst : LLVMValue -> PM LLVMValue
      LLVMIsAIntrinsicInst : LLVMValue -> PM LLVMValue
      LLVMIsADbgInfoIntrinsic : LLVMValue -> PM LLVMValue
      LLVMIsADbgVariableIntrinsic : LLVMValue -> PM LLVMValue
      LLVMIsADbgDeclareInst : LLVMValue -> PM LLVMValue
      LLVMIsADbgLabelInst : LLVMValue -> PM LLVMValue
      LLVMIsAMemIntrinsic : LLVMValue -> PM LLVMValue
      LLVMIsAMemCpyInst : LLVMValue -> PM LLVMValue
      LLVMIsAMemMoveInst : LLVMValue -> PM LLVMValue
      LLVMIsAMemSetInst : LLVMValue -> PM LLVMValue
      LLVMIsACmpInst : LLVMValue -> PM LLVMValue
      LLVMIsAFCmpInst : LLVMValue -> PM LLVMValue
      LLVMIsAICmpInst : LLVMValue -> PM LLVMValue
      LLVMIsAExtractElementInst : LLVMValue -> PM LLVMValue
      LLVMIsAGetElementPtrInst : LLVMValue -> PM LLVMValue
      LLVMIsAInsertElementInst : LLVMValue -> PM LLVMValue
      LLVMIsAInsertValueInst : LLVMValue -> PM LLVMValue
      LLVMIsALandingPadInst : LLVMValue -> PM LLVMValue
      LLVMIsAPHINode : LLVMValue -> PM LLVMValue
      LLVMIsASelectInst : LLVMValue -> PM LLVMValue
      LLVMIsAShuffleVectorInst : LLVMValue -> PM LLVMValue
      LLVMIsAStoreInst : LLVMValue -> PM LLVMValue
      LLVMIsABranchInst : LLVMValue -> PM LLVMValue
      LLVMIsAIndirectBrInst : LLVMValue -> PM LLVMValue
      LLVMIsAInvokeInst : LLVMValue -> PM LLVMValue
      LLVMIsAReturnInst : LLVMValue -> PM LLVMValue
      LLVMIsASwitchInst : LLVMValue -> PM LLVMValue
      LLVMIsAUnreachableInst : LLVMValue -> PM LLVMValue
      LLVMIsAResumeInst : LLVMValue -> PM LLVMValue
      LLVMIsACleanupReturnInst : LLVMValue -> PM LLVMValue
      LLVMIsACatchReturnInst : LLVMValue -> PM LLVMValue
      LLVMIsAFuncletPadInst : LLVMValue -> PM LLVMValue
      LLVMIsACatchPadInst : LLVMValue -> PM LLVMValue
      LLVMIsACleanupPadInst : LLVMValue -> PM LLVMValue
      LLVMIsAUnaryOperator : LLVMValue -> PM LLVMValue
      LLVMIsAFreezeInst : LLVMValue -> PM LLVMValue
      LLVMIsAAtomicCmpXchgInst : LLVMValue -> PM LLVMValue
      LLVMIsAAtomicRMWInst : LLVMValue -> PM LLVMValue
      LLVMIsAFenceInst : LLVMValue -> PM LLVMValue
    ]

    -- Type operations
    %runElab decl `[
      LLVMGetTypeKind : LLVMType -> PM Int
      LLVMTypeIsSized : LLVMType -> PM LLVMBool
      LLVMGetTypeContext : LLVMType -> PM LLVMContext
      LLVMDumpType : LLVMType -> PM ()
      LLVMGetIntTypeWidth : LLVMType -> PM CUnsigned
      LLVMGetFunctionTypeReturnType : LLVMType -> PM LLVMType
      LLVMCountParamTypes : LLVMType -> PM CUnsigned
      LLVMGetParamTypes : LLVMType -> CList LLVMType -> PM ()
      LLVMIsFunctionVarArg : LLVMType -> PM LLVMBool
      LLVMGetElementType : LLVMType -> PM LLVMType
      LLVMGetNumContainedTypes : LLVMType -> PM CUnsigned
      LLVMGetSubtypes : LLVMType -> CList LLVMType -> PM ()
      LLVMGetArrayLength : LLVMType -> PM CUnsigned
      LLVMGetArrayLength2 : LLVMType -> PM CULongLong
      LLVMGetPointerAddressSpace : LLVMType -> PM CUnsigned
      LLVMGetVectorSize : LLVMType -> PM CUnsigned
      LLVMGetPointerElementType : LLVMType -> PM LLVMType
    ]

    -- Struct type operations
    %runElab decl `[
      LLVMGetStructName : LLVMType -> PM String
      LLVMCountStructElementTypes : LLVMType -> PM CUnsigned
      LLVMGetStructElementTypes : LLVMType -> CList LLVMType -> PM ()
      LLVMStructGetTypeAtIndex : LLVMType -> CUnsigned -> PM LLVMType
      LLVMIsPackedStruct : LLVMType -> PM LLVMBool
      LLVMIsOpaqueStruct : LLVMType -> PM LLVMBool
      LLVMIsLiteralStruct : LLVMType -> PM LLVMBool
    ]

    -- Context operations
    %runElab decl `[
      LLVMContextSetDiagnosticHandler : LLVMContext -> CPtr -> CPtr -> PM ()
      LLVMContextGetDiagnosticHandler : LLVMContext -> PM CPtr
      LLVMContextGetDiagnosticContext : LLVMContext -> PM CPtr
      LLVMContextSetYieldCallback : LLVMContext -> CPtr -> CPtr -> PM ()
      LLVMContextShouldDiscardValueNames : LLVMContext -> PM LLVMBool
      LLVMContextSetDiscardValueNames : LLVMContext -> LLVMBool -> PM ()
      LLVMGetDiagInfoDescription : LLVMDiagnosticInfo -> PM String
      LLVMGetDiagInfoSeverity : LLVMDiagnosticInfo -> PM Int
      LLVMGetMDKindIDInContext : LLVMContext -> String -> CUnsigned -> PM CUnsigned
      LLVMGetMDKindID : String -> CUnsigned -> PM CUnsigned
      LLVMGetEnumAttributeKindForName : String -> CUnsigned -> PM CUnsigned
      LLVMGetLastEnumAttributeKind : PM CUnsigned
      LLVMCreateEnumAttribute : LLVMContext -> CUnsigned -> CULongLong -> PM LLVMAttribute
      LLVMGetEnumAttributeKind : LLVMAttribute -> PM CUnsigned
      LLVMGetEnumAttributeValue : LLVMAttribute -> PM CULongLong
      LLVMCreateTypeAttribute : LLVMContext -> CUnsigned -> LLVMType -> PM LLVMAttribute
      LLVMGetTypeAttributeValue : LLVMAttribute -> PM LLVMType
      LLVMCreateStringAttribute : LLVMContext -> String -> CUnsigned -> String -> CUnsigned -> PM LLVMAttribute
      LLVMGetStringAttributeKind : LLVMAttribute -> CPtr -> PM String
      LLVMGetStringAttributeValue : LLVMAttribute -> CPtr -> PM String
      LLVMIsEnumAttribute : LLVMAttribute -> PM LLVMBool
      LLVMIsStringAttribute : LLVMAttribute -> PM LLVMBool
      LLVMIsTypeAttribute : LLVMAttribute -> PM LLVMBool
    ]

    -- Module operations
    %runElab decl `[
      LLVMDumpModule : LLVMModule -> PM ()
      LLVMSetModuleInlineAsm : LLVMModule -> String -> PM ()
      LLVMGetModuleInlineAsm : LLVMModule -> CPtr -> PM String
      LLVMGetModuleContext : LLVMModule -> PM LLVMContext
      LLVMGetTypeByName : LLVMModule -> String -> PM LLVMType
      LLVMGetNamedMetadataNumOperands : LLVMModule -> String -> PM CUnsigned
      LLVMGetNamedMetadataOperands : LLVMModule -> String -> CList LLVMValue -> PM ()
      LLVMAddNamedMetadataOperand : LLVMModule -> String -> LLVMValue -> PM ()
      LLVMGetDebugMetadataVersion : PM CUnsigned
      LLVMGetModuleDebugMetadataVersion : LLVMModule -> PM CUnsigned
      LLVMStripModuleDebugInfo : LLVMModule -> PM LLVMBool
      LLVMCreateFunctionPassManagerForModule : LLVMModule -> PM LLVMPassManager
      LLVMCreateFunctionPassManager : CPtr -> PM LLVMPassManager
      LLVMGetOrInsertFunction : LLVMModule -> String -> LLVMType -> PM LLVMValue
    ]

    -- Metadata operations
    %runElab decl `[
      LLVMMDStringInContext : LLVMContext -> String -> CUnsigned -> PM LLVMValue
      LLVMMDString : String -> CUnsigned -> PM LLVMValue
      LLVMMDNodeInContext : LLVMContext -> CList LLVMValue -> CUnsigned -> PM LLVMValue
      LLVMMDNode : CList LLVMValue -> CUnsigned -> PM LLVMValue
      LLVMMetadataAsValue : LLVMContext -> CPtr -> PM LLVMValue
      LLVMValueAsMetadata : LLVMValue -> PM CPtr
      LLVMGetMDString : LLVMValue -> CPtr -> PM String
      LLVMGetMDNodeNumOperands : LLVMValue -> PM CUnsigned
      LLVMGetMDNodeOperands : LLVMValue -> CList LLVMValue -> PM ()
    ]

    -- Inline assembly operations
    %runElab decl `[
      LLVMGetInlineAsmAsmString : LLVMValue -> CPtr -> PM String
      LLVMGetInlineAsmConstraintString : LLVMValue -> CPtr -> PM String
      LLVMGetInlineAsmDialect : LLVMValue -> PM Int
      LLVMGetInlineAsmFunctionType : LLVMValue -> PM LLVMType
      LLVMGetInlineAsmHasSideEffects : LLVMValue -> PM LLVMBool
      LLVMGetInlineAsmNeedsAlignedStack : LLVMValue -> PM LLVMBool
      LLVMGetInlineAsmCanUnwind : LLVMValue -> PM LLVMBool
    ]

    -- Comdat operations  
    %runElab decl `[
      LLVMGetOrInsertComdat : LLVMModule -> String -> PM LLVMComdat
      LLVMGetComdat : LLVMValue -> PM LLVMComdat
      LLVMSetComdat : LLVMValue -> LLVMComdat -> PM ()
      LLVMGetComdatSelectionKind : LLVMComdat -> PM Int
      LLVMSetComdatSelectionKind : LLVMComdat -> Int -> PM ()
    ]

    -- Additional type constructors
    %runElab decl `[
      LLVMInt1Type : PM LLVMType
      LLVMInt8Type : PM LLVMType
      LLVMInt16Type : PM LLVMType
      LLVMInt32Type : PM LLVMType
      LLVMInt64Type : PM LLVMType
      LLVMInt128Type : PM LLVMType
      LLVMInt1TypeInContext : LLVMContext -> PM LLVMType
      LLVMInt8TypeInContext : LLVMContext -> PM LLVMType
      LLVMInt16TypeInContext : LLVMContext -> PM LLVMType
      LLVMInt32TypeInContext : LLVMContext -> PM LLVMType
      LLVMInt64TypeInContext : LLVMContext -> PM LLVMType
      LLVMInt128TypeInContext : LLVMContext -> PM LLVMType
      LLVMFloatTypeInContext : LLVMContext -> PM LLVMType
      LLVMDoubleTypeInContext : LLVMContext -> PM LLVMType
      LLVMX86FP80Type : PM LLVMType
      LLVMFP128Type : PM LLVMType
      LLVMPPCFP128Type : PM LLVMType
      LLVMX86FP80TypeInContext : LLVMContext -> PM LLVMType
      LLVMFP128TypeInContext : LLVMContext -> PM LLVMType
      LLVMPPCFP128TypeInContext : LLVMContext -> PM LLVMType
      LLVMVoidTypeInContext : LLVMContext -> PM LLVMType
      LLVMLabelTypeInContext : LLVMContext -> PM LLVMType
      LLVMX86MMXType : PM LLVMType
      LLVMX86MMXTypeInContext : LLVMContext -> PM LLVMType
      LLVMX86AMXType : PM LLVMType
      LLVMX86AMXTypeInContext : LLVMContext -> PM LLVMType
    ]

  -- Legacy builder operations (for completeness)
  %runElab decl `[
    LLVMBuildInvoke : CPtr -> LLVMValue -> CList LLVMValue -> CUnsigned -> LLVMValue -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildLoad : CPtr -> LLVMValue -> String -> PM LLVMValue
    LLVMBuildGEP : CPtr -> LLVMValue -> CList LLVMValue -> CUnsigned -> String -> PM LLVMValue
    LLVMBuildInBoundsGEP : CPtr -> LLVMValue -> CList LLVMValue -> CUnsigned -> String -> PM LLVMValue
    LLVMBuildStructGEP : CPtr -> LLVMValue -> CUnsigned -> String -> PM LLVMValue
    LLVMBuildIntCast : CPtr -> LLVMValue -> LLVMType -> String -> PM LLVMValue
  ]

  -- Additional builder operations 
  %runElab decl `[
    LLVMBuildCallWithOperandBundles : CPtr -> LLVMType -> LLVMValue -> CList LLVMValue -> CUnsigned -> CPtr -> CUnsigned -> String -> PM LLVMValue
    LLVMBuildInvokeWithOperandBundles : CPtr -> LLVMType -> LLVMValue -> CList LLVMValue -> CUnsigned -> LLVMValue -> LLVMValue -> CPtr -> CUnsigned -> String -> PM LLVMValue
    LLVMBuildCallBr : CPtr -> LLVMType -> LLVMValue -> CList LLVMValue -> CUnsigned -> LLVMValue -> CList LLVMValue -> CUnsigned -> String -> PM LLVMValue
  ]

  -- Advanced memory operations
  %runElab decl `[
    LLVMBuildAtomicLoad : CPtr -> LLVMType -> LLVMValue -> String -> Int -> CUnsigned -> PM LLVMValue
    LLVMBuildAtomicStore : CPtr -> LLVMValue -> LLVMValue -> Int -> CUnsigned -> PM LLVMValue
  ]
