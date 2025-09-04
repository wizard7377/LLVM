||| A module that provides the syntax sugar for the smart constructors 
||| Each of these tries to follow a basic convention 
||| Ops starting with `?` create variables
||| Ops starting with `#` create (constant) values
||| `$` creates statements
||| `!` creates basic blocks *or* instructions *or* arguements
||| `:` creates types
||| `@` creates declerations
||| `^` creates metadata, and `^^` creates annotations, however `^^` attaches an annotation to a value
||| `&` creates a modifier

{-
FIXITY RULES
Everything that must precede a literal has precedence 90-100 or 0-10
 -}
module Data.LLVM.Ops.Sugar

import Data.LLVM.Ops.Core
import Data.LLVM.Ops.Ops 
import Data.LLVM.Ops.Control
import Data.LLVM.IR.Core
import Data.LLVM.IR.Core
import Data.LLVM.IR.Util
import Data.LLVM.Ops.Helper
import Data.LLVM.IR.Program
export
implementation FromString Name where 
    fromString = local
export 
implementation FromString Label where 
    fromString = NamedLabel
export
{t : Bool} -> Num (LValue t) where 
    (+) = ?noNumAdd
    (*) = ?noNumMul
    fromInteger x = cast $ Core.LInt $ cast x
export
implementation Num LType where 
    (+) = ?noNumAdd2
    (*) = ?noNumMul2
    fromInteger = LType.LInt . cast
export 
{t : Bool} -> FromString (LValue t) where 
    fromString x = cast $ Core.LString x
export prefix 18 ?*
export infixr 11 !:
export infixl 19 !^
export prefix 18 :?
export infixl 29 ^^ 
export prefix 20 ^#
export prefix 20 ^>



export infixr 11 :->

-- Prefixes and the like
export prefix 1 ?%, ?@, ?^, ##, #^, :#, #!
export infixr 2 ?+
export infixl 3 @-, @=, @+, @?
export infixl 4 <:>, <::>
-- Statements 
export infixl 8 <<-
export prefix 8 -<< 

export infixr 9 !>
export prefix 9 !#, #!

public export 
||| Convenient operator to create a pointer expression from a name.
|||
||| Creates a pointer expression from a Name. This is a shorthand
||| for `ptrExpr name` that makes pointer operations more readable.
|||
||| @ name The name to create a pointer expression for
(?*) : Name -> ALValue 
(?*) name = ptrExpr name



public export 
||| Convenient operator to create a targeted statement (assignment).
|||
||| Creates a statement that assigns the result of an operation to a
||| target variable. This is a shorthand for `MkLStatement target op` that
||| makes assignment operations more readable.
|||
||| @ target The target variable name to assign to
||| @ op The operation whose result to assign
(<<-) : Name -> LExpr -> LStatement
(<<-) target op = MkLStatement (Just target) op neutral



public export
(-<<) : LExpr -> LStatement
(-<<) v = MkLStatement Nothing v neutral




public export 
(?%) : String -> LValue False
(?%) name = Core.LVar $ local name



public export 
(?@) : String -> LValue False
(?@) name = Core.LVar $ global name


public export 
(?^) : String -> LValue False
(?^) name = Core.LVar $ Parameter name

public export 
(?+) : String -> List LType -> IntrinsicName
(?+) a b = MkIntrinsicName a b



public export 
(##) : Int -> ALValue
(##) i = cast $ Core.LInt i


public export 
(#^) : String -> Label
(#^) n = NamedLabel n


public export 
(!:) : String -> (List LStatement, Terminator) -> (String, BasicBlock) 
(!:) name (stmts, term) = (name, MkBasicBlock stmts term)


public export 
(!^) : String -> LType -> Argument
(!^) name ty = MkArgument ty neutral (Just name)

public export 
(!>) : List LStatement -> Terminator -> BasicBlock
(!>) stmts term = MkBasicBlock stmts term




public export
(:#) : Int -> LType 
(:#) t = LType.LInt t


||| Combine a type and value
public export 
(<:>) : a -> LType -> WithType a
(<:>) t x = MkWithType x t
public export 
(<::>) : LType -> a -> WithType a
(<::>) t x = MkWithType t x


public export 
(^#) : Nat -> Metadata 
(^#) n = MetadataNode n 


public export 
(@-) : Setter a b => a -> (b -> b) -> a
(@-) = flip setting


public export 
(@=) : Setter a b => a -> b -> a
(@=) x y = setting (const y) x


public export 
(@+) : Monoid b => Setter a b => a -> b -> a
(@+) x y = setting (<+> y) x
public export 
(@?) : Setter a (Maybe b) => a -> b -> a
(@?) x y = setting (<|> (the (Maybe b) $ Just y)) x

||| Get element ptr
(&*) : (WithType (LValue False)) -> List (WithType (LValue True)) -> LExpr
(&*) (MkWithType ty v) idx = (getElementPtr ty v idx)
||| Extract element 

(&@) : (WithType (LValue False)) -> List Int -> LValue False
  
