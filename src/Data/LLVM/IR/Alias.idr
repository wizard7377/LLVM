module Data.LLVM.IR.Alias

import Data.LLVM.IR.Core       
--import Data.LLVM.Write
import Data.LLVM.IR.Ops
import Data.LLVM.IR.Program

export
||| Create an integer constant.
mkInt : Int -> LExpr
mkInt i = LInt i

export
||| Create an i1 (boolean) type.
i1 : LType
i1 = LInt 1

export
||| Create an i8 type.
i8 : LType
i8 = LInt 8

export
||| Create an i16 type.
i16 : LType
i16 = LInt 16

export
||| Create an i32 type.
i32 : LType
i32 = LInt 32

export
||| Create an i64 type.
i64 : LType
i64 = LInt 64

export
||| Create a void type.
void : LType
void = LVoid

export
||| Create a pointer type.
ptr : LType
ptr = LPtr

export
||| Create a pointer type in specific address space.
ptrAddr : AddressSpace -> LType
ptrAddr addrSpace = LPtrAddr addrSpace

export
||| Create a float type.
float : LType
float = LFloating LFloat

export
||| Create a double type.
double : LType
double = LFloating LDouble



-- 10. Missing additional type builders
export
||| Create a label type.
labelType : LType
labelType = LLabel

export
||| Create a token type.
tokenType : LType
tokenType = LToken

export
||| Create a metadata type.
metadataType : LType
metadataType = LMetadata

export
||| Create an opaque type.
opaqueType : LType
opaqueType = LOpaque

export
||| Create an x86 AMX type.
x86AmxType : LType
x86AmxType = LX86_AMX


-- 15. Missing additional floating point type builders
export
||| Create a half precision float type.
half : LType
half = LFloating Half

export
||| Create a brain float type.
bfloat : LType
bfloat = LFloating Bfloat

export
||| Create an x86 extended precision type.
x86_fp80 : LType
x86_fp80 = LFloating X86_FP80

export
||| Create a PowerPC double-double type.
ppc_fp128 : LType
ppc_fp128 = LFloating PPC_FP128

export
||| Create a quad precision float type.
fp128 : LType
fp128 = LFloating FP128