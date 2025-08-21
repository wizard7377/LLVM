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
module Data.LLVM.Builders.Sugar

import Data.LLVM.Builders.Core
import Data.LLVM.Builders.Ops 
import Data.LLVM.IR.Core
import Data.LLVM.IR.Ops
import Data.LLVM.IR.Util
import Data.LLVM.Builders.Helper
import Data.LLVM.IR.Program
export
implementation FromString Name where 
    fromString = local
export 
implementation FromString Label where 
    fromString = NamedLabel
export
implementation Num LValue where 
    (+) = ?noNumAdd
    (*) = ?noNumMul
    fromInteger = LTerm.LInt . cast
export
implementation Num LType where 
    (+) = ?noNumAdd2
    (*) = ?noNumMul2
    fromInteger = LType.LInt . cast
export 
implementation FromString LValue where 
    fromString = LTerm.LString
export prefix 18 ?*
export infixl 0 $<-
export prefix 11 $<< 
export infixr 11 !:
export infixl 19 !^
export prefix 18 :?
export infix 11 <:>
export infixr 11 <::>
export infixr 1 :<>
export infixr 1 :**
export infixl 29 ^^ 
export prefix 20 ^#
export prefix 20 ^>
export infixl 11 &-
export infixl 11 &=
export infixl 11 &+
export infixl 11 &?
export infixl 100 @= 
export infixl 100 @: 
export infixl 100 @< 
export infixl 100 @>
export infixr 11 :->

export prefix 0 ?%, ?@, ?^, ##, #^, :#
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
||| Convenient operator to create a targeted statement (assignment).
|||
||| Creates a statement that assigns the result of an operation to a
||| target variable. This is a shorthand for `MkLStatement target op` that
||| makes assignment operations more readable.
|||
||| @ target The target variable name to assign to
||| @ op The operation whose result to assign
($<-) : Name -> LExpr -> LStatement
($<-) target op = MkLStatement (Just target) op neutral



public export
($<<) : LExpr -> LStatement
($<<) v = MkLStatement Nothing v neutral




public export 
(?%) : String -> LValue
(?%) name = LTerm.LVar $ local name



public export 
(?@) : String -> LValue
(?@) name = LTerm.LVar $ global name


public export 
(?^) : String -> LValue 
(?^) name = LTerm.LVar $ Parameter name




public export 
(##) : Int -> LValue 
(##) i = LTerm.LInt i


public export 
(#^) : String -> Label
(#^) n = NamedLabel n


public export 
(!:) : String -> (List LStatement, Terminator) -> BasicBlock 
(!:) name (stmts, term) = MkBasicBlock name stmts term

public export 
(!^) : String -> LType -> Argument
(!^) name ty = MkArgument ty neutral (Just name)


public export
(:?) : Type -> LType 
(:?) t = lowerTypeReflect t



public export
(:#) : Int -> LType 
(:#) t = LType.LInt t

public export 
(:->) : LType -> LType -> LType
(:->) t1 (LFun res args) = LFun res (t1 :: args)
(:->) t1 t2 = LType.LFun t2 [t1]
||| Combine a type and value
public export 
(<:>) : a -> LType -> WithType a
(<:>) t x = MkWithType x t
public export 
(<::>) : LType -> a -> WithType a
(<::>) t x = MkWithType t x


public export 
(:<>) : Int -> LType -> LType
(:<>) n x = LType.LVector n x

public export 
(:**) : Int -> LType -> LType
(:**) n x = LType.LArray n x
public export 
(^^) : CanNote a => a -> (String, Metadata) -> a
(^^) x n = note x (MkAnnotation [n])

public export 
(^#) : Nat -> Metadata 
(^#) n = MetadataNode n 


public export 
(^>) : String -> Metadata
(^>) name = MetadataString name


public export 
(&-) : Setter a b => a -> (b -> b) -> a
(&-) = flip setting


public export 
(&=) : Setter a b => a -> b -> a
(&=) x y = setting (const y) x


public export 
(&+) : Monoid b => Setter a b => a -> b -> a
(&+) x y = setting (<+> y) x
public export 
(&?) : Setter a (Maybe b) => a -> b -> a
(&?) x y = setting (<|> (the (Maybe b) $ Just y)) x
public export 
(@=) : String -> WithType LValue -> LClause
(@=) name (MkWithType ty v) = GlobalDefC $ globalDef name {init = Just v} ty

public export 
(@<) : String -> WithType LValue -> LClause
(@<) name (MkWithType ty v) = GlobalDefC $ globalDef name {init = Just v} {isConst = True} ty

public export 
(@:) : String -> LType -> LClause
(@:) name ty = GlobalDefC $ globalDef name ty