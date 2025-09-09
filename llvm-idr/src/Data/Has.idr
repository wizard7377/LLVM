module Data.Has 

%default total
export
hasJust : (1 m : Maybe a) -> Bool
hasJust (Just _) = True
hasJust Nothing = False
public export
data Has : (a : Type) -> {0 p : Bool} -> Type where
  Given : a -> Has a {p=True} 
  Blank : Has a {p=False}
export
having : {0 b : Bool} -> {auto 0 prop : p === True} -> Has a {p=p} -> a
having (Given x) = x
export
might : (1 x : Has a) -> Maybe a
might (Given x) = Just x
might Blank = Nothing
export
maybeHas : (1 m : Maybe a) -> Has a {p=hasJust m}
maybeHas (Just x) = Given x
maybeHas Nothing = Blank

export 
altHas : {0 p : Bool} -> {0 q : Bool} -> Has a {p=p} -> Has a {p=q} -> Has a {p = (p || q)}
altHas (Given x) _ = Given x
altHas Blank y = y
