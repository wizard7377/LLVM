||| A module that provides the syntax sugar for the smart constructors 
||| Each of these tries to follow a basic convention 
||| Ops starting with `?` create variables
||| Ops starting with `#` create (constant) values
||| `$` creates statements
||| `!` creates basic blocks
||| `:` creates types
module Data.LLVM.IR.Builders.Sugar

import Data.LLVM.IR.Builders.Core
import Data.LLVM.IR.Builders.Ops 
import Data.LLVM.IR.Core
import Data.LLVM.IR.Ops
import Data.LLVM.IR.Builders.Helper
export
implementation FromString Name where 
    fromString = local
public export 
prefix 10 ?*

public export 
||| Convenient operator to create a pointer expression from a name.
|||
||| Creates a pointer expression from a Name. This is a shorthand
||| for `ptrExpr name` that makes pointer operations more readable.
|||
||| @ name The name to create a pointer expression for
(?*) : Name -> LValue
(?*) name = ptrExpr name


public export 
infix 0 $<-

public export 
||| Convenient operator to create a targeted statement (assignment).
|||
||| Creates a statement that assigns the result of an operation to a
||| target variable. This is a shorthand for `MkLStatement target op` that
||| makes assignment operations more readable.
|||
||| @ target The target variable name to assign to
||| @ op The operation whose result to assign
($<-) : Name -> LInstruction -> LStatement
($<-) target op = MkLStatement (Just target) op []

public export 
prefix 1 $<< 

public export
($<<) : LInstruction -> LStatement
($<<) v = MkLStatement Nothing v []

public export 
prefix 10 ?%


public export 
(?%) : String -> LValue
(?%) name = LTerm.LVar $ local name

public export 
prefix 10 ?@

public export 
(?@) : String -> LValue
(?@) name = LTerm.LVar $ global name

public export 
prefix 10 ?^

public export 
(?^) : String -> LValue 
(?^) name = LTerm.LVar $ Parameter name


public export 
prefix 10 ##

public export 
(##) : Int -> LValue 
(##) i = LTerm.LInt i

public export 
infix 1 !:

public export 
(!:) : String -> (List LStatement, Terminator) -> BasicBlock 
(!:) name (stmts, term) = MkBasicBlock name stmts term

public export 
prefix 10 :?

public export
(:?) : Type -> LType 
(:?) t = lowerTypeReflect t

public export 
prefix 10 :#

public export
(:#) : Int -> LType 
(:#) t = LType.LInt t

public export 
infix 1 <:>

public export 
(<:>) : LType -> a -> WithType a
(<:>) t x = MkWithType t x

public export 
infixr 1 :<>

public export 
(:<>) : Int -> LType -> LType
(:<>) n x = LType.LVector n x