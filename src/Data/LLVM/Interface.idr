module Data.LLVM.Interface
import Data.LLVM.Class
import Data.LLVM.Write
import Data.LLVM.Core
import Data.LLVM.Ops
import Data.LLVM.Program
import public Data.LLVM.Builders
public export
interface Startable a b where 
    startWith : b -> a -> a 

public export
interface Endable a b where 
    endWith : a -> b -> a

public export
interface Monoid a => Startable a b => Endable a b => Surrondable a b where

public export
infixl 9 <<+

public export 
infixr 9 +>>

public export
(<<+) : Startable a b => b -> a -> a
(<<+) = startWith
public export
(+>>) : Endable a b => a -> b -> a
(+>>) = endWith

public export 
[startCast] Startable a b => Cast c b => Startable a c where
    startWith s acc = let 
        v : b = cast s
        in startWith v acc 

public export 
[endCast] Endable a b => Cast c b => Endable a c where
    endWith acc s = let 
        v : b = cast s
        in endWith acc v
export
Startable FunctionBody LStatement where
    startWith s acc = let 
        v : LStatement = cast s
        in startWith v acc
export
Endable FunctionBody LStatement where
    endWith acc s = let 
        v : LStatement = cast s
        in endWith acc v

export 
Startable LModule LClause where
    startWith s acc = let 
        v : LClause = cast s
        in startWith v acc
export 
Endable LModule LClause where
    endWith acc s = let 
        v : LClause = cast s
        in endWith acc v
export 
Startable (List a) a where 
    startWith s acc = s :: acc
export 
Endable (List a) a where 
    endWith acc s = acc ++ [s]

export 
Endable FunctionBody LOperation where 
    endWith acc s = let 
        statement : LStatement = Discarded s 
        in endWith acc statement