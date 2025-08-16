module Data.Walk 

import Control.Monad.Identity 
public export 
interface Walk from to where 
    %globalhint
    go : (x : from) -> to

export
Cast a b => Walk a b where 
  go = cast

export  
Walk a b => Walk b c => Walk a c where 
  go = go . (the (a -> b) go)
 
public export 
[idWalk] Walk a a where 
  go = id
export 
Functor f => Walk a b => Walk (f a) (f b) where 
  go = map go

export 
Applicative f => Walk a (f a) where 
  go = pure

public export 
Walk a (Identity a) where 
  go = Id
Walk (Identity a) a where 
  go (Id x) = x
public export 
infixr 0 $$
public export 
($$) : Walk t a => (f : a -> b) -> (x : t) -> b
f $$ x = f (go x)

public export 
infixl 10 ?@
public export 
(?@) : Walk t a => (f : a -> b) -> (x : t) -> b
f ?@ x = f (go x)
