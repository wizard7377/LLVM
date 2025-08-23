module Data.LLVM.Builders.Math 


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
    (lhs : LValue) ->
    (rhs : LValue) ->
    LExpr
icmp pred ty lhs rhs = (ICmp pred) ty lhs rhs


export
||| Create a floating point comparison operation.
fcmp :
    (ordered : Bool) ->
    {default [] fastMath : FastMath} ->
    (pred : Comparison) ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LExpr
fcmp True {fastMath} pred ty lhs rhs = (FCmpOrd fastMath pred) ty lhs rhs
fcmp False {fastMath} pred ty lhs rhs = (FCmpUnOrd fastMath pred) ty lhs rhs

export
||| Create division operation (unsigned).
udiv :
    {default False exact : Bool} ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LExpr
udiv {exact = False} ty lhs rhs = UDiv ty lhs rhs
udiv {exact = True} ty lhs rhs = UDivExact ty lhs rhs

export
||| Create division operation (signed).
sdiv :
    {default False exact : Bool} ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LExpr
sdiv {exact = False} ty lhs rhs = SDiv ty lhs rhs  
sdiv {exact = True} ty lhs rhs = SDivExact ty lhs rhs

export
||| Create floating point division operation.
fdiv :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LExpr
fdiv {fastMath} ty lhs rhs = (FDiv fastMath) ty lhs rhs

export
||| Create remainder operation (unsigned).
urem : LType -> LValue -> LValue -> LExpr
urem ty lhs rhs = URem ty lhs rhs

export
||| Create remainder operation (signed).
srem : LType -> LValue -> LValue -> LExpr  
srem ty lhs rhs = SRem ty lhs rhs

export
||| Create floating point remainder operation.
frem :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LExpr
frem {fastMath} ty lhs rhs = (FRem fastMath) ty lhs rhs

export
||| Create shift left operation.
shl :
    {default Nothing wrap : Maybe Wrapping} ->
    (ty : LType) ->
    (value : LValue) ->
    (amount : LValue) ->
    LExpr
shl {wrap = Nothing} ty value amount = Shl ty value amount
shl {wrap = Just w} ty value amount = (ShlWrap w) ty value amount

export
||| Create logical shift right operation.
lshr :
    {default False exact : Bool} ->
    (ty : LType) ->
    (value : LValue) ->
    (amount : LValue) ->
    LExpr
lshr {exact = False} ty value amount = LShr ty value amount
lshr {exact = True} ty value amount = LShrExact ty value amount

export
||| Create arithmetic shift right operation.
ashr :
    {default False exact : Bool} ->
    (ty : LType) ->
    (value : LValue) ->
    (amount : LValue) ->
    LExpr
ashr {exact = False} ty value amount = AShr ty value amount
ashr {exact = True} ty value amount = AShrExact ty value amount

export
||| Create bitwise AND operation.
|||
||| Creates a bitwise AND operation between two integer operands of the same type.
||| Each bit in the result is the logical AND of the corresponding bits in the operands.
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand
||| @ rhs The right-hand side operand
and : LType -> LValue -> LValue -> LExpr
and ty lhs rhs = And ty lhs rhs

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
    (lhs : LValue) ->
    (rhs : LValue) ->
    LExpr
or {disjoint = False} ty lhs rhs = Or ty lhs rhs
or {disjoint = True} ty lhs rhs = DisjointOr ty lhs rhs

export
||| Create bitwise XOR operation.
|||
||| Creates a bitwise XOR operation between two integer operands of the same type.
||| Each bit in the result is the logical XOR of the corresponding bits in the operands.
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand
||| @ rhs The right-hand side operand
xor : LType -> LValue -> LValue -> LExpr
xor ty lhs rhs = Xor ty lhs rhs


export
||| Create an add operation.
|||
||| Creates an integer addition operation between two operands of the same type.
||| The operation performs standard arithmetic addition.
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand
||| @ rhs The right-hand side operand
add : LType -> LValue -> LValue -> LExpr
add ty lhs rhs = Add ty lhs rhs

export
||| Create a subtract operation.
|||
||| Creates an integer subtraction operation between two operands of the same type.
||| The operation performs standard arithmetic subtraction (lhs - rhs).
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand (minuend)
||| @ rhs The right-hand side operand (subtrahend)
sub : LType -> LValue -> LValue -> LExpr
sub ty lhs rhs = Sub ty lhs rhs

export
||| Create a multiply operation.
|||
||| Creates an integer multiplication operation between two operands of the same type.
||| The operation performs standard arithmetic multiplication.
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand
||| @ rhs The right-hand side operand
mul : LType -> LValue -> LValue -> LExpr
mul ty lhs rhs = Mul ty lhs rhs


export
||| Create a truncate operation.
trunc :
    {default Nothing wrap : Maybe Wrapping} ->
    (from : WithType LValue) ->
    (to : LType) ->
    LExpr
trunc {wrap = Nothing} from to = (Trunc NoSignedUnsigned) from to
trunc {wrap = Just w} from to = (Trunc w) from to

export
||| Create a zero extend operation.
|||
||| Creates a zero extension operation that increases the bit width of an
||| integer value by padding with zeros in the high-order bits.
|||
||| @ from The typed source value with smaller bit width
||| @ to The target type with larger bit width
zext : (from : WithType LValue) -> (to : LType) -> LExpr
zext from to = ZExt from to

export
||| Create a sign extend operation.
|||
||| Creates a sign extension operation that increases the bit width of a
||| signed integer value by replicating the sign bit in the high-order bits.
|||
||| @ from The typed source value with smaller bit width
||| @ to The target type with larger bit width
sext : (from : WithType LValue) -> (to : LType) -> LExpr
sext from to = SExt from to

export
||| Create a bitcast operation.
|||
||| Creates a bitcast operation that reinterprets the bits of a value as
||| a different type without changing the bit pattern. Both types must
||| have the same size.
|||
||| @ from The typed source value to reinterpret
||| @ to The target type to reinterpret as
bitcast : (from : WithType LValue) -> (to : LType) -> LExpr
bitcast from to = BitCast from to

export
||| Create a floating point truncate operation.
fptrunc :
    {default [] fastMath : FastMath} ->
    (from : WithType LValue) ->
    (to : LType) ->
    LExpr
fptrunc {fastMath} from to = (FPTrunc fastMath) from to

export
||| Create a floating point extend operation.
fpext :
    {default [] fastMath : FastMath} ->
    (from : WithType LValue) ->
    (to : LType) ->
    LExpr
fpext {fastMath} from to = (FPExt fastMath) from to


-- Additional missing builders that were identified in the audit:

-- 1. Missing wrapped arithmetic operations
export
||| Create addition with wrapping flags.
addWrap :
    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LExpr
addWrap wrap ty lhs rhs = (AddWrap wrap) ty lhs rhs

export
||| Create subtraction with wrapping flags.
subWrap :
    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LExpr
subWrap wrap ty lhs rhs = (SubWrap wrap) ty lhs rhs

export
||| Create multiplication with wrapping flags.
mulWrap :
    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LExpr
mulWrap wrap ty lhs rhs = (MulWrap wrap) ty lhs rhs

-- 2. Missing enhanced shift operations
export
||| Create shift left with wrapping flags.
shlWrap :
    (wrap : Wrapping) ->
    (ty : LType) ->
    (value : LValue) ->
    (amount : LValue) ->
    LExpr
shlWrap wrap ty value amount = (ShlWrap wrap) ty value amount

-- 3. Missing enhanced comparison builders with specific predicates
export
||| Integer comparison: equal.
icmpEq : LType -> LValue -> LValue -> LExpr
icmpEq = icmp CEq

export
||| Integer comparison: not equal.
icmpNe : LType -> LValue -> LValue -> LExpr
icmpNe = icmp CNe

export
||| Integer comparison: unsigned less than.
icmpULt : LType -> LValue -> LValue -> LExpr
icmpULt = icmp CULt

export
||| Integer comparison: signed less than.
icmpSLt : LType -> LValue -> LValue -> LExpr
icmpSLt = icmp CSLt

export
||| Integer comparison: unsigned greater than.
icmpUGt : LType -> LValue -> LValue -> LExpr
icmpUGt = icmp CUGt

export
||| Integer comparison: signed greater than.
icmpSGt : LType -> LValue -> LValue -> LExpr
icmpSGt = icmp CSGt

export
||| Integer comparison: unsigned less than or equal.
icmpULe : LType -> LValue -> LValue -> LExpr
icmpULe = icmp CULe

export
||| Integer comparison: signed less than or equal.
icmpSLe : LType -> LValue -> LValue -> LExpr
icmpSLe = icmp CSLe

export
||| Integer comparison: unsigned greater than or equal.
icmpUGe : LType -> LValue -> LValue -> LExpr
icmpUGe = icmp CUGe

export
||| Integer comparison: signed greater than or equal.
icmpSGe : LType -> LValue -> LValue -> LExpr
icmpSGe = icmp CSGe


export
||| Create a floating point negation operation.
fneg : LType -> LValue -> LExpr
fneg ty operand = FNeg ty operand

export
||| Create floating point addition with fast math flags.
fadd :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LExpr
fadd {fastMath} ty lhs rhs = (FAdd fastMath) ty lhs rhs

export
||| Create floating point subtraction with fast math flags.
fsub :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LExpr
fsub {fastMath} ty lhs rhs = (FSub fastMath) ty lhs rhs

export
||| Create floating point multiplication with fast math flags.
fmul :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LExpr
fmul {fastMath} ty lhs rhs = (FMul fastMath) ty lhs rhs