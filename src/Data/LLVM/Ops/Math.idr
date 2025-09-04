module Data.LLVM.Ops.Math 


--import Data.LLVM.Class
import Data.LLVM.IR.Core       
--import Data.LLVM.Write.Text.Encode
import Data.LLVM.IR.Core
import Data.LLVM.IR.Program
import Data.LLVM.IR.Alias
import Data.List
import Data.Walk
import Data.LLVM.IR.Util



export
||| Create an integer comparison operation.
|||
||| Creates an integer comparison operation using the specified predicate.
||| The comparison evaluates to an i1 (boolean) result that can be used
||| in conditional branches and other boolean contexts.
|||
||| @ pred The comparison predicate (eq, ne, lt, gt, etc.)
||| @ ty The type of the operands being compared
||| @ lhs The left-hand side operand
||| @ rhs The right-hand side operand
icmp :
    (pred : Comparison) ->
    (ty : LType) ->
    (lhs : (LValue False)) ->
    (rhs : (LValue False)) ->
    LExpr
icmp pred ty lhs rhs = ((ICmp False) pred) ty (toRuntime (toRuntime lhs)) (toRuntime (toRuntime rhs))


export
||| Create a floating point comparison operation.
fcmp :
	    (ordered : Bool) ->
    {default [] fastMath : FastMath} ->
    (pred : Comparison) ->
    (ty : LType) ->
    (lhs : (LValue False)) ->
    (rhs : (LValue False)) ->
    LExpr
fcmp True {fastMath} pred ty lhs rhs = ((FCmp True) fastMath pred) ty (toRuntime lhs) (toRuntime rhs)
fcmp False {fastMath} pred ty lhs rhs = ((FCmp False) fastMath pred) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create division operation (unsigned).
udiv :
	    {default False exact : Bool} ->
    (ty : LType) ->
    (lhs : (LValue False)) ->
    (rhs : (LValue False)) ->
    LExpr
udiv {exact = False} ty lhs rhs = (UDiv False) ty (toRuntime lhs) (toRuntime rhs)
udiv {exact = True} ty lhs rhs = (UDiv True) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create division operation (signed).
sdiv :
	    {default False exact : Bool} ->
    (ty : LType) ->
    (lhs : (LValue False)) ->
    (rhs : (LValue False)) ->
    LExpr
sdiv {exact = False} ty lhs rhs = (SDiv False) ty (toRuntime lhs) (toRuntime rhs)  
sdiv {exact = True} ty lhs rhs = (SDiv True) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create floating point division operation.
fdiv :
	    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : (LValue False)) ->
    (rhs : (LValue False)) ->
    LExpr
fdiv {fastMath} ty lhs rhs = (FDiv fastMath) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create remainder operation (unsigned).
urem :  LType -> (LValue False) -> (LValue False) -> LExpr
urem ty lhs rhs = URem ty (toRuntime lhs) (toRuntime rhs)

export
||| Create remainder operation (signed).
srem :  LType -> (LValue False) -> (LValue False) -> LExpr  
srem ty lhs rhs = SRem ty (toRuntime lhs) (toRuntime rhs)

export
||| Create floating point remainder operation.
frem :
	    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : (LValue False)) ->
    (rhs : (LValue False)) ->
    LExpr
frem {fastMath} ty lhs rhs = (FRem fastMath) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create shift left operation.
shl :
	    {default Nothing wrap : Maybe Wrapping} ->
    (ty : LType) ->
    (value : (LValue False)) ->
    (amount : (LValue False)) ->
    LExpr
shl {wrap = Nothing} ty value amount = (Shl NoWrap) ty (toRuntime value) (toRuntime amount)
shl {wrap = Just w} ty value amount = (Shl w) ty (toRuntime value) (toRuntime amount)

export
||| Create logical shift right operation.
lshr :
	    {default False exact : Bool} ->
    (ty : LType) ->
    (value : (LValue False)) ->
    (amount : (LValue False)) ->
    LExpr
lshr {exact = False} ty value amount = (LShr False) ty (toRuntime value) (toRuntime amount)
lshr {exact = True} ty value amount = (LShr True) ty (toRuntime value) (toRuntime amount)

export
||| Create arithmetic shift right operation.
ashr :
	    {default False exact : Bool} ->
    (ty : LType) ->
    (value : (LValue False)) ->
    (amount : (LValue False)) ->
    LExpr
ashr {exact = False} ty value amount = (AShr False) ty (toRuntime value) (toRuntime amount)
ashr {exact = True} ty value amount = (AShr True) ty (toRuntime value) (toRuntime amount)

export
||| Create bitwise AND operation.
|||
||| Creates a bitwise AND operation between two integer operands of the same type.
||| Each bit in the result is the logical AND of the corresponding bits in the operands.
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand
||| @ rhs The right-hand side operand
and :  LType -> (LValue False) -> (LValue False) -> LExpr
and ty lhs rhs = And ty (toRuntime lhs) (toRuntime rhs)

export
||| Create bitwise OR operation.
|||
||| Creates a bitwise OR operation between two integer operands of the same type.
||| Each bit in the result is the logical OR of the corresponding bits in the operands.
||| Optionally can be marked as disjoint for optimization.
|||
||| @ disjoint Whether the operands are disjoint (no common set bits)
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand
||| @ rhs The right-hand side operand
or :
	    {default False disjoint : Bool} ->
    (ty : LType) ->
    (lhs : (LValue False)) ->
    (rhs : (LValue False)) ->
    LExpr
or {disjoint = False} ty lhs rhs = (Or False) ty (toRuntime lhs) (toRuntime rhs)
or {disjoint = True} ty lhs rhs = (Or True) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create bitwise XOR operation.
|||
||| Creates a bitwise XOR operation between two integer operands of the same type.
||| Each bit in the result is the logical XOR of the corresponding bits in the operands.
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand
||| @ rhs The right-hand side operand
xor :   LType -> (LValue False) -> (LValue False) -> LExpr
xor ty lhs rhs = Xor ty (toRuntime lhs) (toRuntime rhs)


export
||| Create an add operation.
|||
||| Creates an integer addition operation between two operands of the same type.
||| The operation performs standard arithmetic addition.
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand
||| @ rhs The right-hand side operand
add :   LType -> (LValue False) -> (LValue False) -> LExpr
add ty lhs rhs = (Add NoWrap) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create a subtract operation.
|||
||| Creates an integer subtraction operation between two operands of the same type.
||| The operation performs standard arithmetic subtraction (lhs - rhs).
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand (minuend)
||| @ rhs The right-hand side operand (subtrahend)
sub :   LType -> (LValue False) -> (LValue False) -> LExpr
sub ty lhs rhs = (Sub NoWrap) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create a multiply operation.
|||
||| Creates an integer multiplication operation between two operands of the same type.
||| The operation performs standard arithmetic multiplication.
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand
||| @ rhs The right-hand side operand
mul :   LType -> (LValue False) -> (LValue False) -> LExpr
mul ty lhs rhs = (Mul NoWrap) ty (toRuntime lhs) (toRuntime rhs)


export
||| Create a truncate operation.
trunc :
	{False : Bool} ->
    {default Nothing wrap : Maybe Wrapping} ->
    (from : WithType (LValue False)) ->
    (to : LType) ->
    LExpr
trunc {wrap = Nothing} from to = (Trunc NoSignedUnsigned)  (toRuntime' from) to
trunc {wrap = Just w} from to = (Trunc w)  (toRuntime' from) to

export
||| Create a zero extend operation.
|||
||| Creates a zero extension operation that increases the bit width of an
||| integer value by padding with zeros in the high-order bits.
|||
||| @ from The typed source value with smaller bit width
||| @ to The target type with larger bit width
zext : {t : Bool} -> (from : WithType (LValue t)) -> (to : LType) -> LExpr
zext from to = ZExt  (toRuntime' from) to

export
||| Create a sign extend operation.
|||
||| Creates a sign extension operation that increases the bit width of a
||| signed integer value by replicating the sign bit in the high-order bits.
|||
||| @ from The typed source value with smaller bit width
||| @ to The target type with larger bit width
sext : {t : Bool} -> (from : WithType (LValue t)) -> (to : LType) -> LExpr
sext from to = SExt  (toRuntime' from) to

export
||| Create a bitcast operation.
|||
||| Creates a bitcast operation that reinterprets the bits of a value as
||| a different type without changing the bit pattern. Both types must
||| have the same size.
|||
||| @ from The typed source value to reinterpret
||| @ to The target type to reinterpret as
bitcast : {t : Bool} -> (from : WithType (LValue t)) -> (to : LType) -> LExpr
bitcast from to = BitCast  (toRuntime' from) to

export
||| Create a floating point truncate operation.
fptrunc :
	{False : Bool} ->
    {default [] fastMath : FastMath} ->
    (from : WithType (LValue False)) ->
    (to : LType) ->
    LExpr
fptrunc {fastMath} from to = (FPTrunc fastMath)  (toRuntime' from) to

export
||| Create a floating point extend operation.
fpext :
    {False : Bool} ->
    {default [] fastMath : FastMath} ->
    (from : WithType (LValue False)) ->
    (to : LType) ->
    LExpr
fpext {fastMath} from to = (FPExt fastMath)  (toRuntime' from) to


-- Additional missing builders that were identified in the audit:

-- 1. Missing wrapped arithmetic operations
export
||| Create addition with wrapping flags.
addWrap :
	    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : (LValue False)) ->
    (rhs : (LValue False)) ->
    LExpr
addWrap wrap ty lhs rhs = (Add wrap) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create subtraction with wrapping flags.
subWrap :
	    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : (LValue False)) ->
    (rhs : (LValue False)) ->
    LExpr
subWrap wrap ty lhs rhs = (Sub wrap) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create multiplication with wrapping flags.
mulWrap :
	    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : (LValue False)) ->
    (rhs : (LValue False)) ->
    LExpr
mulWrap wrap ty lhs rhs = (Mul wrap) ty (toRuntime lhs) (toRuntime rhs)

-- 2. Missing enhanced shift operations
export
||| Create shift left with wrapping flags.
shlWrap :
	    (wrap : Wrapping) ->
    (ty : LType) ->
    (value : (LValue False)) ->
    (amount : (LValue False)) ->
    LExpr
shlWrap wrap ty value amount = (Shl wrap) ty (toRuntime value) (toRuntime amount)

-- 3. Missing enhanced comparison builders with specific predicates
export
||| Integer comparison: equal.
icmpEq :  LType -> (LValue False) -> (LValue False) -> LExpr
icmpEq t x y = icmp CEq t x y

export
||| Integer comparison: not equal.
icmpNe :  LType -> (LValue False) -> (LValue False) -> LExpr
icmpNe t x y = icmp CNe t x y

export
||| Integer comparison: unsigned less than.
icmpULt :  LType -> (LValue False) -> (LValue False) -> LExpr
icmpULt t x y = icmp CULt t x y

export
||| Integer comparison: signed less than.
icmpSLt :  LType -> (LValue False) -> (LValue False) -> LExpr
icmpSLt t x y = icmp CSLt t x y

export
||| Integer comparison: unsigned greater than.
icmpUGt :  LType -> (LValue False) -> (LValue False) -> LExpr
icmpUGt t x y = icmp CUGt t x y

export
||| Integer comparison: signed greater than.
icmpSGt :  LType -> (LValue False) -> (LValue False) -> LExpr
icmpSGt t x y = icmp CSGt t x y

export
||| Integer comparison: unsigned less than or equal.
icmpULe :  LType -> (LValue False) -> (LValue False) -> LExpr
icmpULe t x y = icmp CULe t x y

export
||| Integer comparison: signed less than or equal.
icmpSLe :  LType -> (LValue False) -> (LValue False) -> LExpr
icmpSLe t x y = icmp CSLe t x y

export
||| Integer comparison: unsigned greater than or equal.
icmpUGe :  LType -> (LValue False) -> (LValue False) -> LExpr
icmpUGe t x y = icmp CUGe t x y

export
||| Integer comparison: signed greater than or equal.
icmpSGe :  LType -> (LValue False) -> (LValue False) -> LExpr
icmpSGe t x y = icmp CSGe t x y


export
||| Create a floating point negation operation.
fneg : LType -> (LValue ?) -> LExpr
fneg ty operand = FNeg [] ty operand

export
||| Create floating point addition with fast math flags.
fadd :
	    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : (LValue False)) ->
    (rhs : (LValue False)) ->
    LExpr
fadd {fastMath} ty lhs rhs = (FAdd fastMath) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create floating point subtraction with fast math flags.
fsub :
	    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : (LValue False)) ->
    (rhs : (LValue False)) ->
    LExpr
fsub {fastMath} ty lhs rhs = (FSub fastMath) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create floating point multiplication with fast math flags.
fmul :
	    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : (LValue False)) ->
    (rhs : (LValue False)) ->
    LExpr
fmul {fastMath} ty lhs rhs = (FMul fastMath) ty (toRuntime lhs) (toRuntime rhs)
