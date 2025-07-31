module Data.LLVM.IR.Builders.Math 


--import Data.LLVM.Class
import Data.LLVM.IR.Core       
--import Data.LLVM.Write
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
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
icmp pred ty lhs rhs = MiscOp (FnCallOp (MkFnCall NoTail [] Nothing [] Nothing ty (LConstE (LPtr (IntrinsicN "icmp"))) [MkWithType ty lhs, MkWithType ty rhs] []))

export
||| Create a floating point comparison operation.
fcmp :
    (ordered : Bool) ->
    {default [] fastMath : FastMath} ->
    (pred : Comparison) ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
fcmp True {fastMath} pred ty lhs rhs = MiscOp (FnCallOp (MkFnCall NoTail fastMath Nothing [] Nothing ty (LConstE (LPtr (IntrinsicN "fcmp.ord"))) [MkWithType ty lhs, MkWithType ty rhs] []))
fcmp False {fastMath} pred ty lhs rhs = MiscOp (FnCallOp (MkFnCall NoTail fastMath Nothing [] Nothing ty (LConstE (LPtr (IntrinsicN "fcmp.unord"))) [MkWithType ty lhs, MkWithType ty rhs] []))

export
||| Create division operation (unsigned).
udiv :
    {default False exact : Bool} ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
udiv {exact = False} ty lhs rhs = BinaryOp UDiv ty lhs rhs
udiv {exact = True} ty lhs rhs = BinaryOp UDivExact ty lhs rhs

export
||| Create division operation (signed).
sdiv :
    {default False exact : Bool} ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
sdiv {exact = False} ty lhs rhs = BinaryOp SDiv ty lhs rhs  
sdiv {exact = True} ty lhs rhs = BinaryOp SDivExact ty lhs rhs

export
||| Create floating point division operation.
fdiv :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
fdiv {fastMath} ty lhs rhs = BinaryOp (FDiv fastMath) ty lhs rhs

export
||| Create remainder operation (unsigned).
urem : LType -> LExpr -> LExpr -> LOperation
urem ty lhs rhs = BinaryOp URem ty lhs rhs

export
||| Create remainder operation (signed).
srem : LType -> LExpr -> LExpr -> LOperation  
srem ty lhs rhs = BinaryOp SRem ty lhs rhs

export
||| Create floating point remainder operation.
frem :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
frem {fastMath} ty lhs rhs = BinaryOp (FRem fastMath) ty lhs rhs

export
||| Create shift left operation.
shl :
    {default Nothing wrap : Maybe Wrapping} ->
    (ty : LType) ->
    (value : LExpr) ->
    (amount : LExpr) ->
    LOperation
shl {wrap = Nothing} ty value amount = BinaryOp Shl ty value amount
shl {wrap = Just w} ty value amount = BinaryOp (ShlWrap w) ty value amount

export
||| Create logical shift right operation.
lshr :
    {default False exact : Bool} ->
    (ty : LType) ->
    (value : LExpr) ->
    (amount : LExpr) ->
    LOperation
lshr {exact = False} ty value amount = BinaryOp LShr ty value amount
lshr {exact = True} ty value amount = BinaryOp LShrExact ty value amount

export
||| Create arithmetic shift right operation.
ashr :
    {default False exact : Bool} ->
    (ty : LType) ->
    (value : LExpr) ->
    (amount : LExpr) ->
    LOperation
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
and : LType -> LExpr -> LExpr -> LOperation
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
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
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
xor : LType -> LExpr -> LExpr -> LOperation
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
add : LType -> LExpr -> LExpr -> LOperation
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
sub : LType -> LExpr -> LExpr -> LOperation
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
mul : LType -> LExpr -> LExpr -> LOperation
mul ty lhs rhs = BinaryOp Mul ty lhs rhs


export
||| Create a truncate operation.
trunc :
    {default Nothing wrap : Maybe Wrapping} ->
    (from : WithType LExpr) ->
    (to : LType) ->
    LOperation
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
zext : (from : WithType LExpr) -> (to : LType) -> LOperation
zext from to = ConversionOp ZExt from to

export
||| Create a sign extend operation.
|||
||| Creates a sign extension operation that increases the bit width of a
||| signed integer value by replicating the sign bit in the high-order bits.
|||
||| @ from The typed source value with smaller bit width
||| @ to The target type with larger bit width
sext : (from : WithType LExpr) -> (to : LType) -> LOperation
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
bitcast : (from : WithType LExpr) -> (to : LType) -> LOperation
bitcast from to = ConversionOp BitCast from to

export
||| Create a floating point truncate operation.
fptrunc :
    {default [] fastMath : FastMath} ->
    (from : WithType LExpr) ->
    (to : LType) ->
    LOperation
fptrunc {fastMath} from to = ConversionOp (FPTrunc fastMath) from to

export
||| Create a floating point extend operation.
fpext :
    {default [] fastMath : FastMath} ->
    (from : WithType LExpr) ->
    (to : LType) ->
    LOperation
fpext {fastMath} from to = ConversionOp (FPExt fastMath) from to


-- Additional missing builders that were identified in the audit:

-- 1. Missing wrapped arithmetic operations
export
||| Create addition with wrapping flags.
addWrap :
    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
addWrap wrap ty lhs rhs = BinaryOp (AddWrap wrap) ty lhs rhs

export
||| Create subtraction with wrapping flags.
subWrap :
    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
subWrap wrap ty lhs rhs = BinaryOp (SubWrap wrap) ty lhs rhs

export
||| Create multiplication with wrapping flags.
mulWrap :
    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
mulWrap wrap ty lhs rhs = BinaryOp (MulWrap wrap) ty lhs rhs

-- 2. Missing enhanced shift operations
export
||| Create shift left with wrapping flags.
shlWrap :
    (wrap : Wrapping) ->
    (ty : LType) ->
    (value : LExpr) ->
    (amount : LExpr) ->
    LOperation
shlWrap wrap ty value amount = BinaryOp (ShlWrap wrap) ty value amount

-- 3. Missing enhanced comparison builders with specific predicates
export
||| Integer comparison: equal.
icmpEq : LType -> LExpr -> LExpr -> LOperation
icmpEq = icmp CEq

export
||| Integer comparison: not equal.
icmpNe : LType -> LExpr -> LExpr -> LOperation
icmpNe = icmp CNe

export
||| Integer comparison: unsigned less than.
icmpULt : LType -> LExpr -> LExpr -> LOperation
icmpULt = icmp CULt

export
||| Integer comparison: signed less than.
icmpSLt : LType -> LExpr -> LExpr -> LOperation
icmpSLt = icmp CSLt

export
||| Integer comparison: unsigned greater than.
icmpUGt : LType -> LExpr -> LExpr -> LOperation
icmpUGt = icmp CUGt

export
||| Integer comparison: signed greater than.
icmpSGt : LType -> LExpr -> LExpr -> LOperation
icmpSGt = icmp CSGt

export
||| Integer comparison: unsigned less than or equal.
icmpULe : LType -> LExpr -> LExpr -> LOperation
icmpULe = icmp CULe

export
||| Integer comparison: signed less than or equal.
icmpSLe : LType -> LExpr -> LExpr -> LOperation
icmpSLe = icmp CSLe

export
||| Integer comparison: unsigned greater than or equal.
icmpUGe : LType -> LExpr -> LExpr -> LOperation
icmpUGe = icmp CUGe

export
||| Integer comparison: signed greater than or equal.
icmpSGe : LType -> LExpr -> LExpr -> LOperation
icmpSGe = icmp CSGe


export
||| Create a floating point negation operation.
fneg : LType -> LExpr -> LOperation
fneg ty operand = UnaryOp FNeg ty operand

export
||| Create floating point addition with fast math flags.
fadd :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
fadd {fastMath} ty lhs rhs = BinaryOp (FAdd fastMath) ty lhs rhs

export
||| Create floating point subtraction with fast math flags.
fsub :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
fsub {fastMath} ty lhs rhs = BinaryOp (FSub fastMath) ty lhs rhs

export
||| Create floating point multiplication with fast math flags.
fmul :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
fmul {fastMath} ty lhs rhs = BinaryOp (FMul fastMath) ty lhs rhs