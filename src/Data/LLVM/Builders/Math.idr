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
	{t0, t1 : Bool} ->
    (pred : Comparison) ->
    (ty : LType) ->
    (lhs : (LValue t0)) ->
    (rhs : (LValue t1)) ->
    LExpr
icmp pred ty lhs rhs = (ICmp pred) ty (toRuntime (toRuntime lhs)) (toRuntime (toRuntime rhs))


export
||| Create a floating point comparison operation.
fcmp :
	{t0, t1 : Bool} ->
    (ordered : Bool) ->
    {default [] fastMath : FastMath} ->
    (pred : Comparison) ->
    (ty : LType) ->
    (lhs : (LValue t0)) ->
    (rhs : (LValue t1)) ->
    LExpr
fcmp True {fastMath} pred ty lhs rhs = (FCmpOrd fastMath pred) ty (toRuntime lhs) (toRuntime rhs)
fcmp False {fastMath} pred ty lhs rhs = (FCmpUnOrd fastMath pred) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create division operation (unsigned).
udiv :
	{t0, t1 : Bool} ->
    {default False exact : Bool} ->
    (ty : LType) ->
    (lhs : (LValue t0)) ->
    (rhs : (LValue t1)) ->
    LExpr
udiv {exact = False} ty lhs rhs = UDiv ty (toRuntime lhs) (toRuntime rhs)
udiv {exact = True} ty lhs rhs = UDivExact ty (toRuntime lhs) (toRuntime rhs)

export
||| Create division operation (signed).
sdiv :
	{t0, t1 : Bool} ->
    {default False exact : Bool} ->
    (ty : LType) ->
    (lhs : (LValue t0)) ->
    (rhs : (LValue t1)) ->
    LExpr
sdiv {exact = False} ty lhs rhs = SDiv ty (toRuntime lhs) (toRuntime rhs)  
sdiv {exact = True} ty lhs rhs = SDivExact ty (toRuntime lhs) (toRuntime rhs)

export
||| Create floating point division operation.
fdiv :
	{t0, t1 : Bool} ->
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : (LValue t0)) ->
    (rhs : (LValue t1)) ->
    LExpr
fdiv {fastMath} ty lhs rhs = (FDiv fastMath) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create remainder operation (unsigned).
urem : {t0, t1 : Bool} -> LType -> (LValue t0) -> (LValue t1) -> LExpr
urem ty lhs rhs = URem ty (toRuntime lhs) (toRuntime rhs)

export
||| Create remainder operation (signed).
srem : {t0, t1 : Bool} -> LType -> (LValue t0) -> (LValue t1) -> LExpr  
srem ty lhs rhs = SRem ty (toRuntime lhs) (toRuntime rhs)

export
||| Create floating point remainder operation.
frem :
	{t0, t1 : Bool} ->
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : (LValue t0)) ->
    (rhs : (LValue t1)) ->
    LExpr
frem {fastMath} ty lhs rhs = (FRem fastMath) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create shift left operation.
shl :
	{t0, t1 : Bool} ->
    {default Nothing wrap : Maybe Wrapping} ->
    (ty : LType) ->
    (value : (LValue t0)) ->
    (amount : (LValue t1)) ->
    LExpr
shl {wrap = Nothing} ty value amount = Shl ty (toRuntime value) (toRuntime amount)
shl {wrap = Just w} ty value amount = (ShlWrap w) ty (toRuntime value) (toRuntime amount)

export
||| Create logical shift right operation.
lshr :
	{t0, t1 : Bool} ->
    {default False exact : Bool} ->
    (ty : LType) ->
    (value : (LValue t0)) ->
    (amount : (LValue t1)) ->
    LExpr
lshr {exact = False} ty value amount = LShr ty (toRuntime value) (toRuntime amount)
lshr {exact = True} ty value amount = LShrExact ty (toRuntime value) (toRuntime amount)

export
||| Create arithmetic shift right operation.
ashr :
	{t0, t1 : Bool} ->
    {default False exact : Bool} ->
    (ty : LType) ->
    (value : (LValue t0)) ->
    (amount : (LValue t1)) ->
    LExpr
ashr {exact = False} ty value amount = AShr ty (toRuntime value) (toRuntime amount)
ashr {exact = True} ty value amount = AShrExact ty (toRuntime value) (toRuntime amount)

export
||| Create bitwise AND operation.
|||
||| Creates a bitwise AND operation between two integer operands of the same type.
||| Each bit in the result is the logical AND of the corresponding bits in the operands.
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand
||| @ rhs The right-hand side operand
and : {t0, t1 : Bool} -> LType -> (LValue t0) -> (LValue t1) -> LExpr
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
	{t0, t1 : Bool} ->
    {default False disjoint : Bool} ->
    (ty : LType) ->
    (lhs : (LValue t0)) ->
    (rhs : (LValue t1)) ->
    LExpr
or {disjoint = False} ty lhs rhs = Or ty (toRuntime lhs) (toRuntime rhs)
or {disjoint = True} ty lhs rhs = DisjointOr ty (toRuntime lhs) (toRuntime rhs)

export
||| Create bitwise XOR operation.
|||
||| Creates a bitwise XOR operation between two integer operands of the same type.
||| Each bit in the result is the logical XOR of the corresponding bits in the operands.
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand
||| @ rhs The right-hand side operand
xor : {t0, t1 : Bool} ->  LType -> (LValue t0) -> (LValue t1) -> LExpr
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
add : {t0, t1 : Bool} ->  LType -> (LValue t0) -> (LValue t1) -> LExpr
add ty lhs rhs = Add ty (toRuntime lhs) (toRuntime rhs)

export
||| Create a subtract operation.
|||
||| Creates an integer subtraction operation between two operands of the same type.
||| The operation performs standard arithmetic subtraction (lhs - rhs).
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand (minuend)
||| @ rhs The right-hand side operand (subtrahend)
sub : {t0, t1 : Bool} ->  LType -> (LValue t0) -> (LValue t1) -> LExpr
sub ty lhs rhs = Sub ty (toRuntime lhs) (toRuntime rhs)

export
||| Create a multiply operation.
|||
||| Creates an integer multiplication operation between two operands of the same type.
||| The operation performs standard arithmetic multiplication.
|||
||| @ ty The integer type of both operands (must be the same)
||| @ lhs The left-hand side operand
||| @ rhs The right-hand side operand
mul : {t0, t1 : Bool} ->  LType -> (LValue t0) -> (LValue t1) -> LExpr
mul ty lhs rhs = Mul ty (toRuntime lhs) (toRuntime rhs)


export
||| Create a truncate operation.
trunc :
	{t0 : Bool} ->
    {default Nothing wrap : Maybe Wrapping} ->
    (from : WithType (LValue t0)) ->
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
	{t0 : Bool} ->
    {default [] fastMath : FastMath} ->
    (from : WithType (LValue t0)) ->
    (to : LType) ->
    LExpr
fptrunc {fastMath} from to = (FPTrunc fastMath)  (toRuntime' from) to

export
||| Create a floating point extend operation.
fpext :
    {t0 : Bool} ->
    {default [] fastMath : FastMath} ->
    (from : WithType (LValue t0)) ->
    (to : LType) ->
    LExpr
fpext {fastMath} from to = (FPExt fastMath)  (toRuntime' from) to


-- Additional missing builders that were identified in the audit:

-- 1. Missing wrapped arithmetic operations
export
||| Create addition with wrapping flags.
addWrap :
	{t0, t1 : Bool} ->
    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : (LValue t0)) ->
    (rhs : (LValue t1)) ->
    LExpr
addWrap wrap ty lhs rhs = (AddWrap wrap) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create subtraction with wrapping flags.
subWrap :
	{t0, t1 : Bool} ->
    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : (LValue t0)) ->
    (rhs : (LValue t1)) ->
    LExpr
subWrap wrap ty lhs rhs = (SubWrap wrap) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create multiplication with wrapping flags.
mulWrap :
	{t0, t1 : Bool} ->
    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : (LValue t0)) ->
    (rhs : (LValue t1)) ->
    LExpr
mulWrap wrap ty lhs rhs = (MulWrap wrap) ty (toRuntime lhs) (toRuntime rhs)

-- 2. Missing enhanced shift operations
export
||| Create shift left with wrapping flags.
shlWrap :
	{t0, t1 : Bool} ->
    (wrap : Wrapping) ->
    (ty : LType) ->
    (value : (LValue t0)) ->
    (amount : (LValue t1)) ->
    LExpr
shlWrap wrap ty value amount = (ShlWrap wrap) ty (toRuntime value) (toRuntime amount)

-- 3. Missing enhanced comparison builders with specific predicates
export
||| Integer comparison: equal.
icmpEq : {t0, t1 : Bool} -> LType -> (LValue t0) -> (LValue t1) -> LExpr
icmpEq t x y = icmp CEq t x y

export
||| Integer comparison: not equal.
icmpNe : {t0, t1 : Bool} -> LType -> (LValue t0) -> (LValue t1) -> LExpr
icmpNe t x y = icmp CNe t x y

export
||| Integer comparison: unsigned less than.
icmpULt : {t0, t1 : Bool} -> LType -> (LValue t0) -> (LValue t1) -> LExpr
icmpULt t x y = icmp CULt t x y

export
||| Integer comparison: signed less than.
icmpSLt : {t0, t1 : Bool} -> LType -> (LValue t0) -> (LValue t1) -> LExpr
icmpSLt t x y = icmp CSLt t x y

export
||| Integer comparison: unsigned greater than.
icmpUGt : {t0, t1 : Bool} -> LType -> (LValue t0) -> (LValue t1) -> LExpr
icmpUGt t x y = icmp CUGt t x y

export
||| Integer comparison: signed greater than.
icmpSGt : {t0, t1 : Bool} -> LType -> (LValue t0) -> (LValue t1) -> LExpr
icmpSGt t x y = icmp CSGt t x y

export
||| Integer comparison: unsigned less than or equal.
icmpULe : {t0, t1 : Bool} -> LType -> (LValue t0) -> (LValue t1) -> LExpr
icmpULe t x y = icmp CULe t x y

export
||| Integer comparison: signed less than or equal.
icmpSLe : {t0, t1 : Bool} -> LType -> (LValue t0) -> (LValue t1) -> LExpr
icmpSLe t x y = icmp CSLe t x y

export
||| Integer comparison: unsigned greater than or equal.
icmpUGe : {t0, t1 : Bool} -> LType -> (LValue t0) -> (LValue t1) -> LExpr
icmpUGe t x y = icmp CUGe t x y

export
||| Integer comparison: signed greater than or equal.
icmpSGe : {t0, t1 : Bool} -> LType -> (LValue t0) -> (LValue t1) -> LExpr
icmpSGe t x y = icmp CSGe t x y


export
||| Create a floating point negation operation.
fneg : LType -> (LValue ?) -> LExpr
fneg ty operand = FNeg ty operand

export
||| Create floating point addition with fast math flags.
fadd :
	{t0, t1 : Bool} ->
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : (LValue t0)) ->
    (rhs : (LValue t1)) ->
    LExpr
fadd {fastMath} ty lhs rhs = (FAdd fastMath) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create floating point subtraction with fast math flags.
fsub :
	{t0, t1 : Bool} ->
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : (LValue t0)) ->
    (rhs : (LValue t1)) ->
    LExpr
fsub {fastMath} ty lhs rhs = (FSub fastMath) ty (toRuntime lhs) (toRuntime rhs)

export
||| Create floating point multiplication with fast math flags.
fmul :
	{t0, t1 : Bool} ->
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : (LValue t0)) ->
    (rhs : (LValue t1)) ->
    LExpr
fmul {fastMath} ty lhs rhs = (FMul fastMath) ty (toRuntime lhs) (toRuntime rhs)