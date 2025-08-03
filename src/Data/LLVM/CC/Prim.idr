module Data.LLVM.CC.Prim

import Data.LLVM.CC.Macros
%language ElabReflection

||| Primitive Monad for LLVM C bindings.
PM : Type -> Type 
PM = PrimIO
||| `void*`
CPtr : Type 
CPtr = AnyPtr
||| A C FFI List 
CList : Type -> Type 
CList a = Ptr a
||| `unsigned`
CUnsigned : Type 
CUnsigned = Bits32
CUInt64 : Type 
CUInt64 = Bits64
CULongLong : Type 
CULongLong = Bits64 -- TODO: Check this
LLVMContext : Type 
LLVMContext = CPtr
LLVMType : Type 
LLVMType = CPtr 
LLVMValue : Type
LLVMValue = CPtr
LLVMBool : Type 
LLVMBool = Int -- Not sure who thought of this, but apparently bool is a Int
-- TODO: Contexts?
namespace CPrim 
  -- Contexts + Modules
  %runElab decl `[
    LLVMContextCreate : PM LLVMContext
    LLVMGetGlobalContext : PM LLVMContext
    LLVMContextDispose : LLVMContext -> PM ()
    LLVMModuleCreateWithName : String -> PM CPtr
    LLVMModuleCreateWithNameInContext : String -> CPtr -> PM CPtr
    LLVMCloneModule : CPtr -> PM CPtr
    LLVMDisposeModule : CPtr -> PM ()
    LLVMSetModuleIdentifier : CPtr -> String -> PM ()
    LLVMGetModuleIdentifier : CPtr -> PM String
    LLVMAddFunction : CPtr -> String -> LLVMType -> PM LLVMValue
  ]
  -- Types
  %runElab decl `[
    LLVMPrintTypeToString : CPtr -> PM String
    LLVMIntType : CUnsigned -> PM LLVMType
    LLVMFloatType : PM LLVMType
    LLVMDoubleType : PM LLVMType
    LLVMFunctionType : LLVMType -> CList LLVMType -> CUnsigned -> Bool -> PM LLVMType
    LLVMStructCreateNamed : CPtr -> String -> PM CPtr
    LLVMStructSetBody : CPtr -> CList CPtr -> CUnsigned -> Bool -> PM ()
    LLVMStructType :  CList LLVMType -> CUnsigned -> Bool -> PM LLVMType
    LLVMArrayType : LLVMType -> CUnsigned -> PM LLVMType
    LLVMArrayType2 : LLVMType -> CUInt64 -> PM LLVMType
    LLVMPointerType : LLVMType -> CUnsigned -> PM LLVMType
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
  ]
