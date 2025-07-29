module Data.Walk 

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
 
export 
Functor f => Walk a b => Walk (f a) (f b) where 
  go = map go

export 
Applicative f => Walk a (f a) where 
  go = pure
public export 
infixr 0 $$
public export 
($$) : Walk t a => (f : a -> b) -> (x : t) -> b
f $$ x = f (go x)

public export 
infixl 10 @@
public export 
(@@) : Walk t a => (f : a -> b) -> (x : t) -> b
f @@ x = f (go x)