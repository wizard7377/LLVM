module Data.LLVM.IR.Builders.Math 


--import Data.LLVM.Class
import Data.LLVM.IR.Core       
--import Data.LLVM.Write.Assembly
import Data.LLVM.IR.Ops
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
    LInstruction
icmp pred ty lhs rhs = CompareOp (ICmp pred) ty lhs rhs


export
||| Create a floating point comparison operation.
fcmp :
    (ordered : Bool) ->
    {default [] fastMath : FastMath} ->
    (pred : Comparison) ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LInstruction
fcmp True {fastMath} pred ty lhs rhs = CompareOp (FCmpOrd fastMath pred) ty lhs rhs
fcmp False {fastMath} pred ty lhs rhs = CompareOp (FCmpUnOrd fastMath pred) ty lhs rhs

export
||| Create division operation (unsigned).
udiv :
    {default False exact : Bool} ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LInstruction
udiv {exact = False} ty lhs rhs = BinaryOp UDiv ty lhs rhs
udiv {exact = True} ty lhs rhs = BinaryOp UDivExact ty lhs rhs

export
||| Create division operation (signed).
sdiv :
    {default False exact : Bool} ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LInstruction
sdiv {exact = False} ty lhs rhs = BinaryOp SDiv ty lhs rhs  
sdiv {exact = True} ty lhs rhs = BinaryOp SDivExact ty lhs rhs

export
||| Create floating point division operation.
fdiv :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LInstruction
fdiv {fastMath} ty lhs rhs = BinaryOp (FDiv fastMath) ty lhs rhs

export
||| Create remainder operation (unsigned).
urem : LType -> LValue -> LValue -> LInstruction
urem ty lhs rhs = BinaryOp URem ty lhs rhs

export
||| Create remainder operation (signed).
srem : LType -> LValue -> LValue -> LInstruction  
srem ty lhs rhs = BinaryOp SRem ty lhs rhs

export
||| Create floating point remainder operation.
frem :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LInstruction
frem {fastMath} ty lhs rhs = BinaryOp (FRem fastMath) ty lhs rhs

export
||| Create shift left operation.
shl :
    {default Nothing wrap : Maybe Wrapping} ->
    (ty : LType) ->
    (value : LValue) ->
    (amount : LValue) ->
    LInstruction
shl {wrap = Nothing} ty value amount = BinaryOp Shl ty value amount
shl {wrap = Just w} ty value amount = BinaryOp (ShlWrap w) ty value amount

export
||| Create logical shift right operation.
lshr :
    {default False exact : Bool} ->
    (ty : LType) ->
    (value : LValue) ->
    (amount : LValue) ->
    LInstruction
lshr {exact = False} ty value amount = BinaryOp LShr ty value amount
lshr {exact = True} ty value amount = BinaryOp LShrExact ty value amount

export
||| Create arithmetic shift right operation.
ashr :
    {default False exact : Bool} ->
    (ty : LType) ->
    (value : LValue) ->
    (amount : LValue) ->
    LInstruction
ashr {exact = False} ty value amount = BinaryOp AShr ty value amount
ashr {exact = True} ty value amount = BinaryOp AShrExact ty value amount

export
||| Create bitwise AND operation.
|||
||| Creates a bitwise AND operation between two integer operands of the same type.
||| Each bit in the result is the logical AND of the corresponding bits in the operands.
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand
||| @ rhs The right-hand side operand
and : LType -> LValue -> LValue -> LInstruction
and ty lhs rhs = BinaryOp And ty lhs rhs

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
    LInstruction
or {disjoint = False} ty lhs rhs = BinaryOp Or ty lhs rhs
or {disjoint = True} ty lhs rhs = BinaryOp DisjointOr ty lhs rhs

export
||| Create bitwise XOR operation.
|||
||| Creates a bitwise XOR operation between two integer operands of the same type.
||| Each bit in the result is the logical XOR of the corresponding bits in the operands.
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand
||| @ rhs The right-hand side operand
xor : LType -> LValue -> LValue -> LInstruction
xor ty lhs rhs = BinaryOp Xor ty lhs rhs


export
||| Create an add operation.
|||
||| Creates an integer addition operation between two operands of the same type.
||| The operation performs standard arithmetic addition.
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand
||| @ rhs The right-hand side operand
add : LType -> LValue -> LValue -> LInstruction
add ty lhs rhs = BinaryOp Add ty lhs rhs

export
||| Create a subtract operation.
|||
||| Creates an integer subtraction operation between two operands of the same type.
||| The operation performs standard arithmetic subtraction (lhs - rhs).
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand (minuend)
||| @ rhs The right-hand side operand (subtrahend)
sub : LType -> LValue -> LValue -> LInstruction
sub ty lhs rhs = BinaryOp Sub ty lhs rhs

export
||| Create a multiply operation.
|||
||| Creates an integer multiplication operation between two operands of the same type.
||| The operation performs standard arithmetic multiplication.
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand
||| @ rhs The right-hand side operand
mul : LType -> LValue -> LValue -> LInstruction
mul ty lhs rhs = BinaryOp Mul ty lhs rhs


export
||| Create a truncate operation.
trunc :
    {default Nothing wrap : Maybe Wrapping} ->
    (from : WithType LValue) ->
    (to : LType) ->
    LInstruction
trunc {wrap = Nothing} from to = ConversionOp (Trunc NoSignedUnsigned) from to
trunc {wrap = Just w} from to = ConversionOp (Trunc w) from to

export
||| Create a zero extend operation.
|||
||| Creates a zero extension operation that increases the bit width of an
||| integer value by padding with zeros in the high-order bits.
|||
||| @ from The typed source value with smaller bit width
||| @ to The target type with larger bit width
zext : (from : WithType LValue) -> (to : LType) -> LInstruction
zext from to = ConversionOp ZExt from to

export
||| Create a sign extend operation.
|||
||| Creates a sign extension operation that increases the bit width of a
||| signed integer value by replicating the sign bit in the high-order bits.
|||
||| @ from The typed source value with smaller bit width
||| @ to The target type with larger bit width
sext : (from : WithType LValue) -> (to : LType) -> LInstruction
sext from to = ConversionOp SExt from to

export
||| Create a bitcast operation.
|||
||| Creates a bitcast operation that reinterprets the bits of a value as
||| a different type without changing the bit pattern. Both types must
||| have the same size.
|||
||| @ from The typed source value to reinterpret
||| @ to The target type to reinterpret as
bitcast : (from : WithType LValue) -> (to : LType) -> LInstruction
bitcast from to = ConversionOp BitCast from to

export
||| Create a floating point truncate operation.
fptrunc :
    {default [] fastMath : FastMath} ->
    (from : WithType LValue) ->
    (to : LType) ->
    LInstruction
fptrunc {fastMath} from to = ConversionOp (FPTrunc fastMath) from to

export
||| Create a floating point extend operation.
fpext :
    {default [] fastMath : FastMath} ->
    (from : WithType LValue) ->
    (to : LType) ->
    LInstruction
fpext {fastMath} from to = ConversionOp (FPExt fastMath) from to


-- Additional missing builders that were identified in the audit:

-- 1. Missing wrapped arithmetic operations
export
||| Create addition with wrapping flags.
addWrap :
    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LInstruction
addWrap wrap ty lhs rhs = BinaryOp (AddWrap wrap) ty lhs rhs

export
||| Create subtraction with wrapping flags.
subWrap :
    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LInstruction
subWrap wrap ty lhs rhs = BinaryOp (SubWrap wrap) ty lhs rhs

export
||| Create multiplication with wrapping flags.
mulWrap :
    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LInstruction
mulWrap wrap ty lhs rhs = BinaryOp (MulWrap wrap) ty lhs rhs

-- 2. Missing enhanced shift operations
export
||| Create shift left with wrapping flags.
shlWrap :
    (wrap : Wrapping) ->
    (ty : LType) ->
    (value : LValue) ->
    (amount : LValue) ->
    LInstruction
shlWrap wrap ty value amount = BinaryOp (ShlWrap wrap) ty value amount

-- 3. Missing enhanced comparison builders with specific predicates
export
||| Integer comparison: equal.
icmpEq : LType -> LValue -> LValue -> LInstruction
icmpEq = icmp CEq

export
||| Integer comparison: not equal.
icmpNe : LType -> LValue -> LValue -> LInstruction
icmpNe = icmp CNe

export
||| Integer comparison: unsigned less than.
icmpULt : LType -> LValue -> LValue -> LInstruction
icmpULt = icmp CULt

export
||| Integer comparison: signed less than.
icmpSLt : LType -> LValue -> LValue -> LInstruction
icmpSLt = icmp CSLt

export
||| Integer comparison: unsigned greater than.
icmpUGt : LType -> LValue -> LValue -> LInstruction
icmpUGt = icmp CUGt

export
||| Integer comparison: signed greater than.
icmpSGt : LType -> LValue -> LValue -> LInstruction
icmpSGt = icmp CSGt

export
||| Integer comparison: unsigned less than or equal.
icmpULe : LType -> LValue -> LValue -> LInstruction
icmpULe = icmp CULe

export
||| Integer comparison: signed less than or equal.
icmpSLe : LType -> LValue -> LValue -> LInstruction
icmpSLe = icmp CSLe

export
||| Integer comparison: unsigned greater than or equal.
icmpUGe : LType -> LValue -> LValue -> LInstruction
icmpUGe = icmp CUGe

export
||| Integer comparison: signed greater than or equal.
icmpSGe : LType -> LValue -> LValue -> LInstruction
icmpSGe = icmp CSGe


export
||| Create a floating point negation operation.
fneg : LType -> LValue -> LInstruction
fneg ty operand = UnaryOp FNeg ty operand

export
||| Create floating point addition with fast math flags.
fadd :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LInstruction
fadd {fastMath} ty lhs rhs = BinaryOp (FAdd fastMath) ty lhs rhs

export
||| Create floating point subtraction with fast math flags.
fsub :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LInstruction
fsub {fastMath} ty lhs rhs = BinaryOp (FSub fastMath) ty lhs rhs

export
||| Create floating point multiplication with fast math flags.
fmul :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LValue) ->
    (rhs : LValue) ->
    LInstruction
fmul {fastMath} ty lhs rhs = BinaryOp (FMul fastMath) ty lhs rhs